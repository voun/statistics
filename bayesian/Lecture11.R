
# A simple implementation of the forward-backward algorithm (written for 
# illustration, not for computational optimality)

# The starting probabilities for x_1: 
startprobs <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
M <- length(startprobs)

# A hidden Markov model, with each x_i having possible values 1,...,M, 
# and with transition probabilities 
# 1/3   increase by 1 (if possible)
# 1/3   decrease by 1 (if possible)
# remaining probability: stay at same value. 
# Matrix of transition probailities: 
transprobs <- diag(M)/3 + 
  rbind(cbind(0,diag(M-1)/3),0) + 
  rbind(0,cbind(diag(M-1)/3,0))
transprobs[1,1] <- transprobs[M,M] <- 2/3

# For the data y_i we assume
# y_i | x_i ~ Poisson(x_i)
y <- c(3, 5, 8, 8, 8 ,8, 4, 3, 4, 2, 1, 1, 1, 0, 1, 
       3, 2, 1, 3, 8, 12, 14, 11, 10)
N <- length(y)

# Compute and store forward probabilities: 
start <- startprobs*dpois(y[1], 1:M)
probsForward <- matrix(start/sum(start), N, M, byrow=T)
for (i in 2:N) {
  res <- dpois(y[i], 1:M)*probsForward[i-1,]%*%transprobs
  probsForward[i,] <- res/sum(res) # to improve numerics
}

# Compute and store backward probabilities: 
probsBackward <- matrix(1, N, M, byrow=T)
for (i in (N-1):1) {
  res <- transprobs%*%probsBackward[i+1,]*dpois(y[i+1], 1:M)
  probsBackward[i,] <- res/sum(res) # to improve numerics
}

# Marginal posterior probabilities
result <- probsForward*probsBackward
sumresult <- apply(result, 1, sum)
result <- result/sumresult 

# Expected value for x_i: 
expected <- apply(t(result)*(1:M), 2, sum)
plot(y)
points(expected, col="red")

############################################################################

# A simple implementation of the Viterbi algorithm, 
# for comparison with Forward Backward

# The starting probabilities for x_1: 
startprobs <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
M <- length(startprobs)

# A hidden Markov model, with each x_i having possible values 1,...,M, 
# and with transition probabilities 
# 1/3   increase by 1 (if possible)
# 1/3   decrease by 1 (if possible)
# remaining probability: stay at same value. 
# Matrix of transition probailities: 
transprobs <- diag(M)/3 + 
  rbind(cbind(0,diag(M-1)/3),0) + 
  rbind(0,cbind(diag(M-1)/3,0))
transprobs[1,1] <- transprobs[M,M] <- 2/3

# For the data y_i we assume
# y_i | x_i ~ Poisson(x_i)
y <- c(3, 5, 8, 8, 8 ,8, 4, 3, 4, 2, 1, 1, 1, 0, 1, 
       3, 2, 1, 3, 8, 12, 14, 11, 10)
N <- length(y)

maxprob <- matrix(startprobs, N, M, byrow=TRUE)
prev <- matrix(0, N, M)

for (i in 2:N) {
  for (j in 1:M) {
    rr <- maxprob[i-1,]*transprobs[,j]*dpois(y[i],1:M)
    prev[i,j] <- which.max(rr)
    maxprob[i,j] <- rr[prev[i,j]]
  }
}
result <- rep(which.max(maxprob[N,]), N)
for (i in (N-1):1) 
  result[i] <- prev[i+1,result[i+1]]

plot(y)
lines(result, col="red")
points(expected, col="green")

#########################################################

# A simple implementation of the Baum Welch algorithm (written for 
# illustration only)

# INITIAL VALUES: 
# Random starting probabilities for x_1 and transprobs: 
M <- 10
library(LearnBayes)
startprobs <- rdirichlet(1, rep(1, M))
transprobs <- matrix(rdirichlet(M, rep(1, M)), M, M, byrow=TRUE)

# For the data y_i we assume
# y_i | x_i ~ Poisson(x_i)
y <- c(3, 5, 8, 8, 8 ,8, 4, 3, 4, 2, 1, 1, 1, 0, 1, 
       3, 2, 1, 3, 8, 12, 14, 11, 10)
N <- length(y)

Niter <- 50
startpResults <- matrix(startprobs, Niter, M, byrow=T)
transpResults <- array(0, c(Niter, M, M))
transpResults[1,,] <- transprobs

for (it in 2:Niter) {
  
  # Run forward-backward with the parameters startpResults[it-1,]
  # and transpResults[it-1,,]
  
  # Compute and store forward probabilities: 
  start <- startpResults[it-1,]*dpois(y[1], 1:M)
  probsForward <- matrix(start/sum(start), N, M, byrow=T)
  for (i in 2:N) {
    res <- dpois(y[i], 1:M)*probsForward[i-1,]%*%transpResults[it-1,,]
    probsForward[i,] <- res/sum(res) # only to improve numerics
  }
  
  # Compute and store backward probabilities: 
  probsBackward <- matrix(1, N, M, byrow=T)
  for (i in (N-1):1) {
    res <- transpResults[it-1,,]%*%probsBackward[i+1,]*dpois(y[i+1], 1:M)
    probsBackward[i,] <- res/sum(res) # only to improve numerics
  }
  
  # Compute the updated parameters: 
  
  start <- probsForward[1,]*probsBackward[1,]
  startpResults[it,] <- start/sum(start)
  
  for (ct in 1:(N-1)) {
    mat <- transpResults[it-1,,]*matrix(probsForward[ct,],M,M)*
      t(matrix(dnorm(y[ct+1],1:M, std), M, M))*
      t(matrix(probsBackward[ct,], M, M))
    mat <- mat/sum(mat)
    transpResults[it,,] <- transpResults[it,,] + mat
  }
  ss <- apply(transpResults[it,,],1, sum)
  transpResults[it,,] <- transpResults[it,,]/ss
  
  if (it==Niter) {
    result <- probsForward*probsBackward
    sumresult <- apply(result, 1, sum)
    result <- result/sumresult 
    
    ## Expected value for x_i: 
    expected <- apply(t(result)*(1:M), 2, sum)
    par(mfrow=c(2,2))
    plot(y)
    lines(expected, col="red")
    image(round(transpResults[it,,],3))
    plot(transpResults[,2,1], type="l")
    plot(transpResults[,2,2], type="l")
    print(round(transpResults[it,,],3))
  }
}


