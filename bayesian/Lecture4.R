# Below, we illustrate a bivariate normal density with 
# mu = (3, 4) and 
# Sigma_11 = 2, Sigma_12 = 1, Sigma_21 = 1, Sigma_22 = 3

mu <- c(3,4)
Sigma <- matrix(c(2, 1, 1, 3), 2, 2)
Sigmainv <- solve(Sigma)
dens <- function(x1,x2) {det(2*pi*Sigma)^{-0.5}*
    exp(-0.5*c(x1,x2)%*%Sigmainv%*%c(x1,x2))}
x1 <- seq(-3, 3, length.out = 101)
x2 <- seq(-3, 3, length.out = 101)
res <- outer(x1, x2, Vectorize(dens))
par(mfrow=c(1,2))
image(x1, x2, res)
contour(x1, x2, res)

#################################################

# Beta-Binomial example
# 6 successes in 19 trials observed. 
# Probability of success $p$ has a flat prior on [0,1]. 
# What is the probability of 4 or more successes in 7 new trials? 

# Solution with simulation: 
# The posterior for p is Beta(7, 14)
# Simulating N = 10000 values for the answer: 
N <- 10000
sims <- pbinom(3, 7, rbeta(N, 7, 14), lower.tail=FALSE)
# An estimate, with a confidence interval: 
print(mean(sims))
print(c(mean(sims)- 1.96*sqrt(var(sims)/N), mean(sims) + 1.96*sqrt(var(sims)/N)))

# An illustration of how the accuracy depends on N:
n <- N/100
result <- matrix(0, 100, 3)
for (i in 1:100) {
  result[i,1] <- mean(sims[1:(n*i)])
  err         <- sqrt(var(sims[1:(n*i)])/(n*i))
  result[i,2] <- result[i,1] + 1.96*err
  result[i,3] <- result[i,1] - 1.96*err
}
matplot(result, col=c("black", "red", "red"), type="l")

# A solution using the formula for the posterior predictive
result <- sum(choose(7,4:7)*beta(7+4:7, 14+7-4:7)/beta(7, 14))
print(result)
abline(h=result)

# A solution using discretization
p <- seq(0, 1, length.out=1001)
prior <- dbeta(p, 7, 14)
predict <- pbinom(3, 7, p, lower.tail=FALSE)
# A simplified integration. Note that the steplength for p is 1/1000
res <- sum(prior*predict)/1000
print(res)

##################################################

# "Manual" simulation from a Binomial(21, 0.32) distribution: 
probs <- dbinom(0:21, 21, 0.32)
cums  <- cumsum(probs)
print(sum(runif(1) > cums))

# "Manual" simulation from a Poisson(7) distribution: 
probs <- dpois(0:100, 7)
cums  <- cumsum(probs)
print(sum(runif(1) > cums))
