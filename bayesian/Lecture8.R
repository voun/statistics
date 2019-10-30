# install.packages("LearnBayes")
library(LearnBayes)
data("hearttransplants")
e = hearttransplants$e
y = hearttransplants$y
ysum <- sum(y)
esum <- sum(e)
# Explore the data: 
plot(e,y)
print(ysum/esum)
plot(log(e),y/e,xlim=c(6,9.7),xlab="log(e)",ylab="y/e")

text(log(e),y/e,labels=as.character(y),pos=4)

###############################################################
# The model with equal lambdas: 
x <- seq(0, 0.002, length.out=1001)
plot(x, dgamma(x, ysum, esum), type="l")

# Exploring the fit of the model with equal lambdas 
# using cross-validation: 
# 
# Adjusted measurements of extreme-ness: 
# NOTE: in R, the last parameter is the 
# probability of NOT obtaining what is counted, instead of 
# the probability of obtaining what is counted. 
m <- pnbinom(y, ysum-y, 1 - e/esum)-
     0.5*dnbinom(y, ysum-y, 1 - e/esum)
qqplot((1:94-0.5)/94, m)
abline(0,1)

#################################
# The hierarchical model: 

# The loglikelihood becomes
z0 <- 1 #(In text: z0 = 0.53)
loglik <- function(theta) {
  sum(lgamma(y+exp(theta[1])) - lgamma(exp(theta[1])) + 
        (theta[1]-theta[2])*exp(theta[1]) - 
        (exp(theta[1])+y)*log(e + exp(theta[1]-theta[2]))) - 
    2*log(exp(theta[1]) + z0) + theta[1]
}

N <- 10000
result <- matrix(c(0, 0), N, 2, byrow=TRUE)
for (i in 2:N) {
  prop <- result[i-1,] + rnorm(2, 0, c(0.1, 0.1))
  accept <- exp(loglik(prop) - loglik(result[i-1,]))
  if (runif(1)<accept) result[i,] <- prop
  else result[i,] <- result[i-1,]
}


par(mfrow=c(2,2))
plot(result[,1], type="l")
plot(result[,2], type="l")
result <- result[-(1:1000),]
plot(result)
result <- exp(result)
plot(result)

# The probability of no deaths at hospital 23, with exposure 1: 
lambda24 <- rgamma(N, result[,1] + y[24], result[,1]/result[,2] + e[24])
print(mean(exp(-lambda24)))

# The probability that hospital 24 is better than hospital 25: 
lambda30 <- rgamma(N, result[,1] + y[30], result[,1]/result[,2] + e[30])
print(sum(lambda24<lambda30)/N)

#######################################
## Slice sampling
#######################################


#install.packages("mcsm")
library(mcsm)
data(challenger)

Nsim <- 1000
x <- challenger[, 2]
y <- challenger[, 1]
n <- length(x)
a <- b <- rep(0, Nsim)

plot(x,y)

# m <- mean(x)
# x <- x - m 
x = x-mean(x)
res <- glm(y ~ x, binomial()) 
print(res$coef)
a[1] <- res$coef[1]
b[1] <- res$coef[2]

for (t in 2:Nsim) {
  uni <- runif(n) * exp(y * (a[t - 1] + b[t - 1] * x))/(1 + 
                                                          exp(a[t - 1] + b[t - 1] * x))
  
  z <- log(uni/(1-uni))
  mina <- max(( z-b[t-1]*x)[y==1])
  maxa <- min((-z-b[t-1]*x)[y==0])
  a[t] <- runif(1, mina, maxa)
  
  minb <- -Inf
  maxb <- Inf
  if (sum((y==1)&(x>0))>0) 
    minb <- max(minb, max((( z-a[t])/x)[(y==1) & (x>0)]))
  if (sum((y==0)&(x<0))>0)
    minb <- max(minb, max(((-z-a[t])/x)[(y==0) & (x<0)]))
  if (sum((y==1)&(x<0))>0)
    maxb <- min(maxb, min((( z-a[t])/x)[(y==1) & (x<0)]))
  if (sum((y==0)&(x>0))>0) 
    maxb <- min(maxb, min(((-z-a[t])/x)[(y==0) & (x>0)])) 
  b[t] <- runif(1, minb, maxb)
}   
par(mfrow=c(2,2))
plot(a, type="l")
plot(b, type="l")
plot(a,b)

###############################
# Toy example: 
# Dealing with missing data: 
# x1...xn ~ Normal(mu, 1), but values over 2 are censored! 
# We use a flat prior on mu, but could also use a normal prior. 
###############################

data <- c(1, 1.4, 1.9, 0.3, 1.5, 1.7) 
# and then there are two observations censored by 2. 
N <- 1000

# method 1, using the censored likelihood and Random Walk MH: 
loglik <- function(mu) {sum(dnorm(data, mu, 1, log=T)) + 
    2*pnorm(2, mu, 1, lower.tail=F, log.p=T)}
result1 <- rep(0, N)
for (i in 2:N) {
  prop <- result1[i-1] + rnorm(1,0,0.1)
  accept <- exp(loglik(prop)-loglik(result1[i-1]))
  if (runif(1)<accept) result1[i] <- prop
  else result1[i] <- result1[i-1]
}

# method 2, using augmented data and Gibbs sampling: 
result2 <- matrix(c(0, 3, 3), N, 3, byrow=T)
nFullData <- length(data) + 2
for (i in 2:N) {
  augmentedData <- c(data, result2[i-1,2:3])
  result2[i,1] <- rnorm(1, mean(augmentedData), sd(augmentedData)/sqrt(nFullData))
  while((result2[i,2] <- rnorm(1, result2[i,1], 1))<=2) TRUE
  while((result2[i,3] <- rnorm(1, result2[i,1], 1))<=2) TRUE
}

par(mfrow=c(2,2))
plot(result1, type="l")
for (i in 1:3) plot(result2[,i], type="l")




