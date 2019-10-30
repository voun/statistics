
# We assume the augmented model with data x_1,...x_n, augmented variables
# z_1,...z_n ~ Bernoulli(0.5), and x_i | z_i ~ Normal(z_i*mu, 1)
# With a flat prior on mu. 

data <- c(0.1, 1.3, 0.9, 4.3, 4.9, 3.7, 2.1)

# EM algorithm (simplified, without checking for convergence):  
N <- 1000
mu <- rep(0, N)
for (i in 2:N) {
  p <- dnorm(data, mu[i-1], 1)/(dnorm(data, 0, 1) + dnorm(data, mu[i-1], 1))
  mu[i] <- sum(p*data)/sum(p)
}

par(mfrow=c(2,2))
plot(data)
plot(mu)
plot(log(mu[-1] - mu[-N]))
p <- dnorm(data, mu[N], 1)/(dnorm(data, 0, 1) + dnorm(data, mu[N], 1))
plot(data, p)

print(mu[N])

# For comparison, using numerical optimization: 
negloglik <- function(mu) {
   - sum(log(0.5*dnorm(data, 0, 1) + 0.5*dnorm(data, mu, 1)))
}
print(nlm(negloglik, 0))

