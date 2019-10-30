#parametric

# x is vecor (k,theta)
loglikelihood <- function(x,data)
{
  n <-length(data)
  (x[1]-1)*sum(log(data))-(1/x[2])*sum(data)-n*lgamma(x[1])-n*x[1]*log(x[2])
}

hin <- function(x)
{
  h = rep(0,2)
  h[1] = x[1]
  h[2] = x[2]
  h
}
d <- rgamma(100,shape=2,scale=2)

params = alabama::constrOptim.nl(c(1,1),function(x) -loglikelihood(x,d),hin = hin)$par
shape = params[1]
scale = params[2]

theta = c()
for(i in 1:1000)
  theta <- append(theta,mean(rgamma(100,shape=shape,scale=scale)))
bias = mean(theta)-mean(d)
variance = var(theta)

h = hist(theta,xlab="x",col="BLUE")#theta hat stjerna

mle = c(shape,scale)

statistic <- function(x) mean(x)
ran <- function(d,mle) rgamma(100,shape=mle[1],scale=mle[2])
bs = boot::boot(d,statistic,ran.gen = ran,R = 1000,sim = "parametric",mle=mle)
CI = boot::boot.ci(bs,conf=0.95,type=c("basic"))
#####
loglikelihood <- function(x,data)
{
  n <-length(data)
  (x[1]-1)*sum(log(data))-(1/x[2])*sum(data)-n*lgamma(x[1])-n*x[1]*log(x[2])
}

hin <- function(x)
{
  h = rep(0,2)
  h[1] = x[1]
  h[2] = x[2]
  h
}
statistic <- function(x) mean(x)
ran <- function(d,mle,n) rgamma(n,shape=mle[1],scale=mle[2])

percentages <- c()
for(n in seq(10,100,10))
{
  counter <- 0
  for(i in 1:1000)
  {
    d <- rgamma(n,shape=2,scale=2)
    params = alabama::constrOptim.nl(c(1,1),function(x) -loglikelihood(x,d),hin = hin)$par
    mle = c(params[1],params[2])
    bs = boot::boot(d,statistic,ran.gen = function(d,mle) ran(d,mle,n),R = 1000,sim = "parametric",mle=mle)
    CI = boot::boot.ci(bs,conf=0.95,type=c("basic"))$basic
    if (CI[4] <= 4 && 4 <= CI[5])
      counter = counter+1;
  }
  percentages <- append(percentages,counter/1000)
}
plot(seq(10,100,10),percentages,col="RED",xlab="n",xlim=c(10,100),ylim=c(0.85,1),cex=0.85,pch=19)
par(new=TRUE)
plot(seq(0,110,10),rep(0.95,12),col="BLUE",xlim=c(10,100),ylim=c(0.85,1),xlab="",ylab="",axes=FALSE,type="l")



