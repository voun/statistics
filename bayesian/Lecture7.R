#
# START with updated version of cars example, from lecture 6. 
#
data(cars)
?cars
plot(cars)
speed <- 21 # predict brake dist at speed = 21

N <- 10000
ff <- function(x) {sum(dnorm(cars[,2], 
                             x[1] + x[2]*cars[,1]+x[3]*cars[,1]^2,
                             x[4], log=TRUE))}
result <- matrix(c(0, 2, 0, 5), N, 4, byrow=T)
predict <- rep(0, N)
acceptances <- 0
for (i in 2:N) {
  prop <- result[i-1,] + rnorm(4, 0, c(0.2, 0.2, 0.01, 0.2)) 
  accept <- exp(ff(prop) - ff(result[i-1,]))
  if (runif(1) < accept) {
    result[i,] <- prop
    acceptances <- acceptances + 1
  } else 
    result[i,] <- result[i-1,]
  predict[i] <- rnorm(1, result[i,1] + result[i,2]*speed + 
                        result[i,3]*speed^2, result[i,4])
}
print(paste("Acceptance rate:", acceptances/N))

result <- result[-(1:1000),]
predict <- predict[-(1:1000)]

# Traceplots: 
par(mfrow=c(2,2))
for (i in 1:4) plot(result[,i], type="l")
par(mfrow=c(1,1))
quantile(predict,probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
plot(result[,2:3])

#########################################################

# Exploring the autocorrelation, for example with 
acf(result[,3])
ccf(result[,3], result[,4])
# Do THINNING, for example with 
result <- result[(1:100)*90,]
# After this, run traceplots and autocorrelation plots

#########################################################

# Multiple starting points: 
# First, compute several results, then compare their trace plots

# Output analysis with coda: 
library(coda)
res <- as.mcmc(result)
# Try functions mcmc(), mcmc.list()...
summary(res)
plot(res)
autocorr(res)
densplot(res)
# ...etc...
# For checking convergence with multiple starting points: gelman.diag(...)

#######################################################
# In the example above 
# how to find a good starting point: 

print(summary(lm(dist~speed, data=cars)))
# Gives intercept approx -17, slope approx. 4, 
# standard error approx 15. 

library(LearnBayes)
# How to find the laplace approximation: 
print(laplace(ff, c(-17, 4, 0, 15)))

# A possible choice for step lengths: 
startpoint <- laplace(ff, c(-17, 4, 0, 15))$mode
myprop <- sqrt(diag(laplace(ff, c(-17, 4, 0, 15))$var))/10

#NEW CODE: 
result <- matrix(startpoint, N, 4, byrow=T)
predict <- rep(0, N)
acceptances <- 0
for (i in 2:N) {
  prop <- result[i-1,] + rnorm(4, 0, myprop) 
  accept <- exp(ff(prop) - ff(result[i-1,]))
  if (runif(1) < accept) {
    result[i,] <- prop
    acceptances <- acceptances + 1
  } else 
    result[i,] <- result[i-1,]
  predict[i] <- rnorm(1, result[i,1] + result[i,2]*speed + 
                        result[i,3]*speed^2, result[i,4])
}
print(paste("Acceptance rate:", acceptances/N))

# Also possible to use the whole covariance matrix from laplace....
# To simulate from the multivariate normal, one may use 
# for example Choleski decomposition, with chol(...)

#################################################################
# Toy example of Gibbs sampling: A bivariate normal distribution 
# with expectation (mu1, mu2) and precision matrix P. 
mu <- c(2, 3)
P  <- matrix(c(2, 1, 1, 2), 2, 2)
N <- 10000
x <- rep(0, N)
y <- rep(0, N)
for (i in 2:N)
  if (i%%2==1) {
    x[i] <- x[i-1]
    y[i] <- rnorm(1, mu[2] - P[2,2]^(-1)*P[2,1]*(x[i]-mu[1]), 1/sqrt(P[2,2]))
  } else {
    y[i] <- y[i-1]
    x[i] <- rnorm(1, mu[1] - P[1,1]^(-1)*P[1,2]*(y[i]-mu[2]), 1/sqrt(P[1,1]))
  }    
plot(x,y, type="l")

#Using Gibbs sampling in example where data is normally distributed
#with expectation mu and precision tau, mu has a normal (0,1) distribution
#and tau has a Gamma distribution with parameters 3 and 4. 

N <- 1000
data <- c(4, 6, 7, 5, 5)
ndata <- length(data)
result <- matrix(c(5.5, 1), N, 2, byrow=TRUE)
for (i in 2:N) {
  result[i,1] <- rnorm(1, (ndata*result[i-1,2]*mean(data))/
                         (1+ndata*result[i-1,2]), 
                       1/sqrt(1+ndata*result[i-1,2]))
  result[i,2] <- rgamma(1, ndata/2+3, 0.5*sum((data-result[i,1])^2)+4)
}
plot(result)

