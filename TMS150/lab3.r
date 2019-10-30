##################
#Assignment 1.1

surfun1.1 <- function(t) (1-pexp(t,1/2))*(1-pweibull(t,1/2,1)^3) #survival function

life1.1 = integrate(surfun1.1,0,Inf)$value # expected life length

time = seq(0,10,0.0001)
di1.1 = numDeriv::grad(function(t) -log(surfun1.1(t)),time) #calculates death intensity
plot(time,di1.1,col="BLACK",type="l",xlim=c(0,10),ylim=c(0.3,0.85),ylab="death intensity") #plot death intenstiy

####################
#Assignment 1.2
myfun1.2 <- function(t) (1-pweibull(t,1/2,1)^3)*dexp(t,1/2) #the integrand
P = integrate(myfun1.2,0,Inf)$value
########################
#Assignment 2.1

surfun2.1W <- function(t) (1-pexp(t,1/2)^2)*(1-pweibull(t,1/2,1)^3) #survival fun. WRC
life2.1W = integrate(surfun2.1W,0,Inf)$value #expected life length, WRC

#plot death intenstiy, no redundant component.
plot(time,di1.1,col="BLACK",type="l",xlim=c(0,10),ylim=c(0,0.85),ylab="death intensities") 

di2.1W = numDeriv::grad(function(t) -log(surfun2.1W(t)),time) #death intensity WRC

#plot death intensity, WRC
par(new=TRUE)
plot(time,di2.1W,col="RED",type="l",xlim=c(0,10),ylim=c(0,0.85),axes=FALSE,xlab="",ylab="")

surfun2.1C <- function(t) (1-pweibull(t,1/2,1)^3)*(1-pgamma(t,2,1/2)) #survival function CRC
life2.1C = integrate(surfun2.1C,0,Inf) #expected life length CRC

di2.1C = numDeriv::grad(function(t) -log(surfun2.1C(t)),time) #death intensity CRC

#plot death intensity CRC
par(new = TRUE)
plot(time,di2.1C,col="BLUE",type="l",xlim=c(0,10),ylim=c(0,0.85),axes=FALSE,xlab="",ylab="")
legend("bottomright",inset=0.02,title="",c("WRC","CRC","normal"),cex=0.7,col=c("RED","BLUE","black"),pch=19)

#####################
#Assignment 2.2

surfun2.2 <- function(t,rho)(1-pexp(t,rho))*(1-pweibull(t,1/2,1)^3) #survival function

paramIntC <- Vectorize(function(rho) #the expression we want to find a zero to, CRC
{
  integral = integrate(function(t) surfun2.2(t,rho)-surfun2.1C(t),0,Inf)$value
})

rhos = seq(0,0.5,0.01)
valuesC = c()

for (i in rhos)
  valuesC = append(valuesC,paramIntC(i)) #evaluate paramIntC for different rho

plot(rhos,valuesC,col="BLUE",type="l",ylab="",xlab="rho") # we see that root is between 0.2 and 0.3
rootC = uniroot(paramIntC,c(0.2,0.3))$root # finds a zero to paramIntC

#########
paramIntW <- Vectorize(function(rho) #the expression we want to find a zero to, WRC
{
  integral = integrate(function(t) surfun2.2(t,rho)-surfun2.1W(t),0,Inf)$value
})

valuesW = c()
for(i in rhos)
  valuesW = append(valuesW,paramIntW(i)) # evaluate paramIntW for different rho


plot(rhos,valuesW,type="l",col="RED",ylab="",xlab="rho") # root between 0.2 and 0.4
rootW = uniroot(paramIntW,c(0.2,0.4))$root #finds a zero to paramIntC

######################
#Assignment 3

#survival fun,  x is supposed to be vector (mu,lambda)
survival3 <- function(t,x) (1-pweibull(t,1/3,1/x[1]))*(1-pweibull(t,1/3,1/x[2])^2) 

lifetime <- function(x) integrate(function(t) survival3(t,x),0,Inf,rel.tol = 1e-12)$value #expected life length

#equality constraintd
heq <- function(x,k) 
{
  h = rep(0,1)
  h[1] = 3/5-k+1/x[1]+2/x[2]
  h
}

#inequality constraint
hin <- function(x)
{
  h = rep(0,2)
  h[1] <- x[1]
  h[2] <- x[2]
  h
}

#first row are optimal mu and second row are optimal lambda. 
params = matrix(rep(0,20),nrow = 2,ncol=10,byrow=FALSE) 
maxLifetimes = rep(0,10)
for (k in 1:10)
{
  heqNew <- function(x) heq(x,k)
  ans = alabama::constrOptim.nl(c(7,70/(35*k-26)),function(x) -lifetime(x),NULL,
                          hin = hin , heq = heqNew) #solve the optimization problem
  params[c(1,2),k] = ans$par #save result in matrix
  maxLifetimes[k] = lifetime(ans$par) #calculate exp life length for optimal parameters.
}

#plot optimal parameters for different k.
plot(1:10,params[1,],ylim=c(0,10),col="RED",cex=0.65,xlab="k",ylab="mu and lambda",pch=19)
par(new=TRUE)
plot(1:10,params[2,],ylim=c(0,10),col="BLUE",xlab="",ylab="",axes=FALSE,cex=0.65,pch=19)
legend("topright",inset=0.02,title="",c("mu","lambda"),cex=0.8,col=c("RED","BLUE"),pch=19)

# plot expected life length against k and fit a linear model.
plot(1:10,maxLifetimes,col=("GREEN"),cex=0.8,pch=19,xlab="k",ylab="expected life length")
model <- lm(maxLifetimes ~ seq(1,10,1))
