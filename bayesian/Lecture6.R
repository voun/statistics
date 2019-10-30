# Simulate from Beta distribution with
alpha <- 17
beta  <- 13
N <- 500
# Compare with density: 
x <- seq(0,1, length.out=1001)
plot(x, dbeta(x, alpha, beta), type="l")

# Simulation algorithm using rejection sampling: 
#maxv  <- (alpha-1)/(alpha+beta-2)
#M     <- dbeta(maxv, alpha, beta)
#u <- runif(N)
#sample <- runif(N)
#sample <- sample[u*M < dbeta(sample, alpha, beta)]
#length(sample)
#plot(sample, type="l")
#hist(sample)


#Use Metropolis Hastings, random walk
RWsample <- rep(0.5, N)
for (i in 2:N) {
  prop <- rnorm(1, RWsample[i-1], 0.2)
  # NOTE: The algorithm is correct both with and without the line below. 
  # prop <- prop - floor(prop)
  accept <- min(1, dbeta(prop, alpha, beta)/
                  dbeta(RWsample[i-1], alpha, beta))
  if (runif(1)<accept) 
    RWsample[i] <- prop 
  else 
    RWsample[i] <- RWsample[i-1]
}

#Explore result: 
hist(RWsample)
plot(RWsample, type="l")
print(sum(RWsample[-1]==RWsample[-N])) ##hur många gånger flyttar ej på oss



# Use Metropolis-Hastings, uniform independent proposal: 
MHsample <- rep(0.5, N)
for (i in 2:N) {
  prop <- runif(1)
  accept <- min(1, dbeta(prop, alpha, beta)/
                  dbeta(MHsample[i-1], alpha, beta))
  if (runif(1)<accept) 
    MHsample[i] <- prop 
  else 
    MHsample[i] <- MHsample[i-1]
}

#Explore result: 
length(MHsample)
hist(MHsample)
plot(MHsample, type="l")
print(sum(MHsample[-1]==MHsample[-N]))





# Apply to Bimodal density: 
# 0.5*betadensity(2,20) + 0.5*betadensity(20,2)
f <- function(x) {0.5*dbeta(x, 2, 20)+0.5*dbeta(x, 20, 2)}
plot(x, f(x), type="l")

# Use Metropolis-Hastings, uniform independent proposal: 
MHsample <- rep(0.2, N)
for (i in 2:N) {
  prop <- runif(1)
  accept <- min(1, f(prop)/f(MHsample[i-1]))
  if (runif(1)<accept) 
    MHsample[i] <- prop 
  else 
    MHsample[i] <- MHsample[i-1]
}

#Explore result: 
length(MHsample)
hist(MHsample)
plot(MHsample, type="l")
print(sum(MHsample[-1]==MHsample[-N]))

#Use Metropolis Hastings, random walk
RWsample <- rep(0.5, N)
for (i in 2:N) {
  prop <- rnorm(1, RWsample[i-1], 0.01)
  accept <- min(1, f(prop)/
                  f(RWsample[i-1]))
  if (runif(1)<accept) 
    RWsample[i] <- prop 
  else 
    RWsample[i] <- RWsample[i-1]
}

#Explore result: 
plot(RWsample, type="l")

############################
# Prediction
############################

# We would like to predict the probability for 7 successes in 12 new trials, 
# when the probability of success has the Beta distribution above. 
# In this toy example, the predictive distribution has been computed: 
print(choose(12, 7)*beta(alpha + 7, beta + 12-7)/beta(alpha, beta))

# We now estimate this using the Ergodic theorem: 
print(mean(dbinom(7, 12, RWsample)))

#########################
# A somewhat more realistic example
#########################

data(cars)
?cars
plot(cars)
# predict brake dist at speed = 21
speed <- 21

N <- 10000

f <- function(x) {prod(dnorm(cars[,2], 
                             x[1] + x[2]*cars[,1]+x[3]*cars[,1]^2,
                             x[4]))}

result <- matrix(c(0, 2, 0, 5), N, 4, byrow=T)
predict <- rep(0, N)
for (i in 2:N) {
#  prop <- result[i-1,] + rnorm(4, 0, c(0.2, 0.2, 0.01, 0.2)) 
  prop <- result[i-1,]+ rnorm(4, 0, c(1.4, 0.2, 0.01, 0.15))
  accept <- min(1, f(prop)/f(result[i-1,]))
  if (runif(1) < accept) result[i,] <- prop
  else result[i,] <- result[i-1,]
  predict[i] <- rnorm(1, result[i,1] + result[i,2]*speed + 
                        result[i,3]*speed^2, result[i,4])
}

#EXPLORE RESULT AND PREDICTION! 
