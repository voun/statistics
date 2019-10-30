
## Assignment 1
a = 0.8
b = 1/6
power1 <- function(v) cos(v*pi/11+7*pi/11)+1
power2 <-function(v) 7/4 + v*1/30-v^2/900
power <- function(v){
  if( v <= 4)
    return(0)
  if (v >= 4 & v < 15)
    return(power1(v))
  else if(v >= 15 & v < 25)
    return(power2(v))
  else
    return(0)
}

x = seq(0,30, length = 500)
plot(x, dgamma(x, a, b)*10, type="l", ylab="", ylim=c(0,2), xlab="v") #for scaling
lines(x, sapply(x, power), type="l", col="blue")

abline(v=4, col="red")
abline(v=25, col="red")

P = pgamma(25, a, b)-pgamma(4, a, b)

res = matrix(numeric(100*3), ncol=3)
samples1 = sapply(rgamma(10000, a, b), power)
for(i in 1:100){
  est = mean(samples1[1:(100*i)])
  sd = sqrt(var(samples1[1:(100*i)]))
  res[i,1] = est
  res[i,2] = est+1.96*sd/sqrt(100*i)
  res[i,3] = est-1.96*sd/sqrt(100*i)
}
matplot(res, col = c("black", "red", "red"), type ="l", xlab="n/100", ylim=c(0.25, 0.45))

avg.power = integrate(function(v) power1(v)*dgamma(v, a, b), 4, 15)$val+integrate(function(v) power2(v)*dgamma(v, a, b), 15, 25)$val

plot(x, sapply(x, function(v) power(v)*dgamma(v, a, b)), type="l", ylab="", ylim = c(0,0.07))
lines(x, dgamma(x, shape=3, scale=5), type="l", lt=2) ##choose Gamma(shape=3, scale=5) as instrumental density

imp.fun1 <- function(v) power(v)*dgamma(v, a, b)/dgamma(v, shape = 3, scale = 5)

res = matrix(numeric(100*3), ncol=3)
samples2 = sapply(rgamma(10000, shape=3, scale=5), imp.fun1)
for(i in 1:100){
  est = mean(samples2[1:(100*i)])
  sd = sqrt(var(samples2[1:(100*i)]))
  res[i,1] = est
  res[i,2] = est+1.96*sd/sqrt(100*i)
  res[i,3] = est-1.96*sd/sqrt(100*i)
}
matplot(res, col = c("black", "red", "red"), type ="l", xlab="n/100", ylim=c(0.25, 0.45))
## we see that the estimator converges much faster to the real parameter and that the CIs are narrower

sqrt(var(samples1))
sqrt(var(samples2))
#samples2 has much lower variance which explains why we have faster convergence
#this shows that importance sampling achieves a reduction of variance.

plot(x, sapply(x, function(v) power(v)^2*dgamma(v, a, b)), type="l", ylab="", ylim = c(0,0.07))
lines(x, dgamma(x, shape=4, scale=4), type="l", lt=2) ##choose Gamma(shape=4, scale=4) as instrumental density

imp.fun2 <- function(v) power(v)^2*dgamma(v, a, b)/dgamma(v, shape = 4, scale = 4)
variance = mean(sapply(rgamma(10000, shape = 4, scale = 4), imp.fun2))-mean(samples2)^2


## Assignment 2
notices = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
count = c(162, 267, 271, 185, 111, 61, 27, 8, 3, 1) #the number of observations is sum(count). Assume nbr of deaths
## every day is Poisson(lambda)
ml = sum(notices*count)/sum(count)
pred = sum(count)*exp(-ml)*ml^notices/factorial(notices)
plot(notices, count, type="p", col="red", xlab="death notices", ylab="day count", ylim=c(0,300))
lines(notices, pred, type="p", col="blue")
legend("topright", legend=c("actual count", "predicted count"), col=c("red", "blue"), lw=c(1,1))

N = 50000
res = matrix(c(0.5, ml, ml, as.integer(count/2)), nrow = N, ncol = 13, byrow=TRUE)
Y = count
for(i in 2:N){
  Z = res[i-1,4:13]
  res[i,1] = rbeta(1, sum(Z)+1, sum(Y-Z)+1)
  res[i,2] = rgamma(1, sum(notices*Z+1), sum(Z)+1)
  res[i,3] = rgamma(1, sum(notices*(Y-Z)+1), sum(Y-Z)+1)
  for(j in 4:13){
    prob = res[i,1]*exp(-res[i,2])*res[i,2]^(j-4)/(res[i,1]*exp(-res[i,2])*res[i,2]^(j-4)+(1-res[i,1])*exp(-res[i,3])*res[i,3]^(j-4))
    res[i,j] = rbinom(1, Y[j-3], prob)
  }
}

sample = res[-(1:2000),1:3]
par(mfrow = c(3,2))
for(i in 1:3){
  plot(sample[,i], type="l")
  hist(sample[,i], prob=TRUE, col="blue")
}

post.means = apply(sample, 2, mean)
p = post.means[1]
l1 = post.means[2]
l2 = post.means[3]

pred = sum(count)*(p*exp(-l1)*l1^notices/factorial(notices) + (1-p)*exp(-l2)*l2^notices/factorial(notices))     
plot(notices, count, type="p", col="red", xlab="death notices", ylab="day count", ylim = c(0,300))
lines(notices, pred, type="p", col="blue")
legend("topright", legend=c("actual count", "predicted count"), col=c("red", "blue"), lw=c(1,1))