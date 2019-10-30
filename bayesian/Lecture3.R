# We consider the Binomial likelihood where 3 successes are observed 
# in 4 trials, and the probability of success is p. 
#
# We plot three possible conjugate priors: 
par(mfrow=c(2,2))
p <- seq(0, 1, length.out = 1000)
plot(p, dbeta(p, 2.5, 2.5), type="l")
plot(p, dbeta(p, 11, 31), type="l")
plot(p, 0.5*dbeta(p, 2.5, 2.5) + 0.5*dbeta(p, 11, 31), type="l")

# We compute the updated weights when using the mixture prior:
# (Remember that the prior predictive is the Beta-Binomial in this case): 
w <- c(0,0)
0.5*choose(4, 3)*beta(2.5+3, 2.5+1)/beta(2.5,2.5) -> w[1]
0.5*choose(4, 3)*beta(11+3, 31+1)/beta(11,31) -> w[2]
print(w)
w <- w/sum(w)
print(w)

# Plot the prior, the likelihood, and the posterior in the same figure: 
# Prior: 
plot(p, 0.5*dbeta(p, 2.5, 2.5) + 0.5*dbeta(p, 11, 31), type="l")
# Likelihood: 
lines(p, dbinom(3, 4, p), col="red")
# Posterior: 
lines(p, w[1]*dbeta(p, 2.5+3, 2.5+1) + w[2]*dbeta(p, 11+3, 31+1), col="blue")

#############################

#Example: Normal distribution with data 
x <- c(3.1, 4.2, 2.9, 3.7, 3.9)
n <- length(x)
xbar <- mean(x)
print(xbar)

# Prior parameters: 
alpha  <- 1
beta   <- 1/3
lambda <- 1/3
mu0    <- 3
# Posterior parameters: 
alphapost  <- alpha + n/2
betapost   <- beta + 0.5*sum((x-xbar)^2) + n*lambda/(lambda+n)*(xbar-mu0)^2/2  
lambdapost <- lambda + n
mu0post    <- (lambda*mu0 + n*xbar)/(lambda + n)
print(paste(alphapost, betapost, lambdapost, mu0post))

par(mfcol=c(2,3))
mu <- seq(1, 5, length.out=101)
tau <- seq(0.3, 10, length.out=101)

# Plot of likelihood: 
likeli <- function(args) {prod(dnorm(x, args[1], 1/sqrt(args[2])))}
#likelihood <- outer(mu, tau, Vectorize(mylik))
grid = expand.grid(mu, tau)
z = apply(grid, 1, likeli)
z = array(z, c(101, 101))
#image(mu, tau, likelihood)
contour(mu, tau, z, levels = seq(0, 0.1, by= 0.005))

# Plot the prior: 
myprior <- function(args) {dgamma(args[2], alpha, beta)*dnorm(args[1], mu0, 1/(lambda*args[2]))}
#prior <- outer(mu, tau, Vectorize(myprior))
grid = expand.grid(mu, tau)
z = apply(grid, 1, myprior)
z = array(z, c(101,101))
#image(mu, tau, prior)
contour(mu, tau, z)

# Plot the posterior: 
mypost <- function(m,t) {dgamma(t, alphapost, betapost)*
    dnorm(m, mu0post, 1/(lambdapost*t))}
post <- outer(mu, tau, Vectorize(mypost))
image(mu, tau, post)
contour(mu, tau, post)