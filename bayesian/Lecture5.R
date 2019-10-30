########################################
# illustration of rejection sampling: 
########################################

# Simulate from Beta distribution with
alpha <- 2.3
beta  <- 5.2
maxv  <- (alpha-1)/(alpha+beta-2)
M     <- dbeta(maxv, alpha, beta)

# Simulation algorithm using rejection sampling: 
N <- 500
sample <- runif(N) #Original sample
u <- runif(N) #Used to find which to reject
keep <- (u*M < dbeta(sample, alpha, beta))
sampleFinal <- sample[keep]

# Make plots to compare with density:
plot(sample, M*u)
x <- seq(0, 1, length.out=1001)
lines(x, dbeta(x, alpha, beta))
points(sample[keep], M*u[keep], col="red")


####################################
# Importance sampling example: 
####################################
# Find the expectation of the Gamma(3,3) density function under 
# the normal distribution with expectation 4 and variance 1. 
x <- seq(-3, 7, length.out = 1001)
plot(x, c(0, rep(1, 1000)), type="n", ylab="")

f <- function(x) dnorm(x, 4, 1)
h <- function(x) dgamma(x, 3, 3)
lines(x, h(x))
lines(x, f(x), col="red")
lines(x, h(x)*f(x)*100, lty=2)
lines(x, dnorm(x, 2, 1), col="blue")

# Compute with numerical integration 
# (works in low dimensions, like in this example)
func <- function(x) f(x)*h(x)
print(integrate(func, -Inf, Inf))

N <- 100
print("Make 10 Monte Carlo estimates not using importance sampling:")
for (i in 1:10) {
  pts <- rnorm(N, 4, 1)
  res <- h(pts)
  print(mean(res))
}

print("Make 10 Monte Carlo estimates using importance sampling:")
for (i in 1:10) {
  pts <- rnorm(N, 2, 1)
  res <- h(pts)*f(pts)/dnorm(pts, 2, 1)
  print(mean(res))
}
##############################################
# Importance sampling resampling (SIR) 
##############################################
# Assume you want to sample from a density proportional to 
# f(x)*h(x) above. Then: 
N <- 100
pts <- rnorm(N, 2, 1)
wts <- h(pts)*f(pts)/dnorm(pts, 2, 1)
wts <- wts/sum(wts)
result <- sample(pts, 100, replace=TRUE, prob=wts)
hist(result)



