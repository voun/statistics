# Confidence intervals and prediction intervals:

x = 
# Create first plot:

# 95% CI for intercept by hand:
n <- nrow(thuesen)
coef(fit)[1] - qt(.975, n-2)*.11748
coef(fit)[1] + qt(.975, n-2)*.11748

# Automatic:
confint(fit)

#############################################################

# Next, we want a confidence intervals for E(y_0), 
#   corresponding to some new value x0=10:
# We are interested in the *average* velocity for patients with 
#   a blood glucose of 10

x0 <- 10
(pred.frame <- data.frame(blood.glucose=x0)) ##f??r att predicta m??ste g??ra ny data frame!

# Obtain fitted value:
predict(fit, pred.frame)

# Check by hand:
(fitted <- fit$coef[1] + fit$coef[2]*x0)

# Obtain confidence interval for E(y0):
predict(fit, pred.frame, level=.95, interval="c") ## c ??r f??r CI och p ??r f??r PI

# Check by hand:
# Quantile of t-distribution that we need:
(quant <- qt(.975,n-2))
# Sigma.hat value:
(sigma.hat <- sqrt(sum((fit$resid)^2)/(n-2)))
# Design matrix:
X <- as.matrix(cbind(1,thuesen[,1]))
XtXi <- solve(t(X) %*% X)
x00 <- as.matrix(c(1,x0),nrow=2)
# Now compute estimate for sd(\hat y0):
(se <- sigma.hat * sqrt( t(x00) %*% XtXi %*% x00))
# Confidence interval:
(lower <- fitted - quant*se)
(upper <- fitted + quant*se)

# Double check:
predict(fit, pred.frame, level=.95, interval="c")

###############################################################
  
# We now want a confidence interval for y_0, 
#   also called prediction interval. 
# We are interested in the velocity for a patient with 
#   a blood glucose of 10

# By hand:
(se <- sigma.hat * sqrt( 1 + t(x00) %*% XtXi %*% x00))
(lower <- fitted - quant*se)
(upper <- fitted + quant*se)

# Automatically:
predict(fit, pred.frame, level=.95, interval="p")

###########################################################

# Finally, let's consider the *pointwise* confidence/prediction band 

# Use x-values 4,5,...,20
x <- c(4:20)
(pred.frame <- data.frame(blood.glucose=x))
CI <- predict(fit, pred.frame, interval="c")
PI <- predict(fit, pred.frame, interval="p")

# Create plot. We use ylim to make sure that 
# the confidence&prediction intervals fit in the plot
plot(short.velocity ~ blood.glucose, data=thuesen, 
          ylim=range(PI,thuesen$short.velocity))
matlines(x, CI, lty=c(1,2,2), col="red", lwd=2)
matlines(x, PI, lty=c(1,3,3), col="blue", lwd=2)

# What is the behavior of the CIs as n grows?
# What is the behavior of the PIs as n grows?



