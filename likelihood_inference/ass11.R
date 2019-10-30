options(digits=6)
## 2a)
norm.mix <- function(thetas, mus, sigmas, weights){
  weights[1]*dnorm(thetas, mus[1], sigmas[1]) + weights[2]*dnorm(thetas, mus[2], sigmas[2])+weights[3]*dnorm(thetas, mus[3], sigmas[3])
  
}
sample.norm.mix <- function(mus, sigmas, weights){ ##mixture of normal so easy to sample from!
  p = runif(1, 0, 1)
  if(p <= weights[1]){
    return(rnorm(1, mus[1], sigmas[1]))
  }
  else if(p >= weights[1] & p <= weights[1]+weights[2]){
    return(rnorm(1, mus[2], sigmas[2]))
  }
  else{
    return(rnorm(1, mus[3], sigmas[3]))
  }
}

weights = c(0.33, 0.57, 0.10)
mus = c(1.5163, 1.5197, 1.5203) 
sigmas = c(0.001, 0.001, 0.005)
thetas = seq(1.5, 1.55, length = 500)

#par(mfrow = c(1,4))
#plot(thetas, norm.mix(thetas, mus, sigmas, weights), type="l", main="prior", ylab="density")

## 2b)
x = seq(1.5, 1.55, length = 500)
#plot(x, norm.mix(x, mus, sqrt(sigmas^2+0.001^2), weights), type="l", ylab="density", main="prior pred")

## 2c)
x = 1.52083
mus.new = (x*1/0.001^2+mus*1/sigmas^2)/(1/0.001^2+1/sigmas^2)
sigmas.new = sqrt(1/(1/0.001^2+1/sigmas^2))
weights.new = weights*dnorm(x, mus, sqrt(sigmas^2+0.001^2))/sum(weights*dnorm(x, mus, sqrt(sigmas^2+0.001^2)))

plot(thetas, norm.mix(thetas, mus.new, sigmas.new, weights.new), type="l", ylab="density", main = "post")
sample = replicate(10000, sample.norm.mix(mus.new, sigmas.new, weights.new))
ci = quantile(sample, probs = c(0.025, 0.975))

## 2d)
mus.new.new = mus+sigmas^2/(sigmas^2+0.001^2)*(x-mus)
sigmas.new.new = sqrt(((sigmas^2+0.001^2)^2-sigmas^4)/(sigmas^2+0.001^2))
weights.new.new = weights.new
y = seq(1.5, 1.55, length = 500)
#plot(y, norm.mix(y, mus.new.new, sigmas.new.new, weights.new.new), type="l", ylab="density", main = "post pred")

## 2e)
LR <- function(xs, xc){
  mus.LR = mus+sigmas^2/(sigmas^2+0.001^2)*(xc-mus)
  sigmas.LR = sqrt(((sigmas^2+0.001^2)^2-sigmas^4)/(sigmas^2+0.001^2))
  weights.LR = weights.new
  
  norm.mix(xs, mus.LR, sigmas.LR, weights.LR)/norm.mix(xs, mus, sqrt(sigmas^2+0.001^2), weights)}

xs = 1.52083
xc = seq(1.5, 1.55, length = 500)
#plot(xc, sapply(xc, function(x) LR(xs, x)), type="l", ylab="", main = "LR")