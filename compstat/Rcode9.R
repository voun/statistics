# Adapted from ISLR Ch 6 

# Chapter 6 Lab 1: Subset Selection Methods

# Best Subset Selection

library(ISLR)
data(Hitters)
?Hitters

head(Hitters)
names(Hitters)
dim(Hitters)

summary(Hitters) # note missing variables in salary
sum(is.na(Hitters$Salary))  # we can also count them like this
Hitters=na.omit(Hitters)  # tar de raderna som inte har n??gra NA
dim(Hitters)
summary(Hitters)

library(leaps)
?regsubsets ##paket f??r att g??ra model selection!

regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19) # consider all possible submodels
reg.summary=summary(regfit.full) ## den hittar b??sta genom att anv??nda vanliga RSS (kanske kan ??ndra till R2)
reg.summary
names(reg.summary)
reg.summary$rsq

# plot criteria versus model size
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

# use built in plot function
?plot.regsubsets
par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)


# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)


# Choosing Among Models

# create indices for training and test sets 
#   for validation set approach
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)

# redo best subset regression on training data 
#   (why do we have to redo this?)
regfit.best = regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

# compute mean squared error on test set
test.mat = model.matrix(Salary~.,data=Hitters[test,])
val.errors = rep(NA,19)
for(i in 1:19){
  coefi = coef(regfit.best,id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}
val.errors
plot(c(1:19), val.errors, type="l")
which.min(val.errors)
coef(regfit.best,10)
# model with 10 variables is best in terms of prediction 
#   error on the validation set

# as there is no predict function for regsubsets, 
#   ISLR provided this:
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# refit model with 10 variables on full data to obtain our final best model
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

# ISLR also contains code for cross validation 
#    rather than validation set approach. 

####################

# Chapter 6 Lab 2: Ridge Regression and the Lasso

library(glmnet)
?glmnet

library(ISLR)
data(Hitters)
Hitters=na.omit(Hitters)

# define x and y, as we cannot use y~x notation in glmnet
x=model.matrix(Salary~.,Hitters)[,-1] #vi ska ha Salary som target s?? f??r en vanlig design matrix h??r, ta bort intercept f??r den l??gger till den sen automatiskt!!
y=Hitters$Salary        ## f??rsta kolonnen ??r bara massa ettor och vi tar bort de nu
dim(x)
length(y)

# Ridge Regression
grid=10^seq(from = 10, to = -2, length=100)  # grid for lambda values
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)    # alpha=0 corresponds to ridge anda alpha=1 lasso
dim(coef(ridge.mod)) # each row corresponds to a predictor, each column to value of lambda

ridge.mod$lambda[50] # =grid[50]
coef(ridge.mod)[,50] # corresponding coefficient estimates
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # corresponding ell_2 norm....

ridge.mod$lambda[60] # =grid[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# get coefficient estimates for new value lambda=35:
predict(ridge.mod, s=35, type="coefficients")[1:20,]

# create training and test set for validation set approach 
# (on purpose showing you a different method than in the subset selection part)
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)   # indices for training data
test=(-train)                        # indices we *leave out* for test data
y.test=y[test]
x.test=x[test,]

# conduct ridge regression on training data
ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)

# evaluate test MSE for fit corresponding to lambda=4
ridge.pred.4 = predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred.4 - y.test)^2)

# compare to test MSE for model with only an intercept
fitted.intercept <- mean(y[train])
mean((fitted.intercept - y.test)^2)

# note that we can mimic the above by taking lambda very large in ridge regression:
ridge.pred.inf = predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred.inf - y.test)^2)

# compare to LS model:
ridge.pred.0 = predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred.0 - y.test)^2)

# choose lambda via cross validation 
?cv.glmnet   # conducts 10 fold cross validation by default
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
par(mfrow=c(1,1))
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam  # 212

# evaluate test MSE for lambda=212
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

# refit on the full data
out=glmnet(x,y,alpha=0)   # use default values for lambda
predict(out,type="coefficients",s=bestlam)[1:20,]  # plug in lambda chosen by CV

### lasso

# By using alpha=1 we can do similar things for the lasso:
# Conduct lasso regression on training data
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid, thresh=1e-12)

cv.out = cv.glmnet(x[train,],y[train],alpha=1)
par(mfrow=c(1,1))
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam  # 35.3

# evaluate test MSE for lambda=35.3
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

# refit on the full data
out=glmnet(x,y,alpha=1)   # use default values for lambda
predict(out,type="coefficients",s=bestlam)  # plug in lambda chosen by CV

