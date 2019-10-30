ranGamma = rgamma(100,shape=2,scale=2)
hist(ranGamma,col="BLUE")

BSsamplemean <- function (x)
{
  theta <- c();
  for (i in 1:1000)
    theta = append(theta,mean(sample(x,100,replace=TRUE)))
  theta
}
bootSample = BSsamplemean(ranGamma)
h = hist(bootSample,xlab="x",col="BLUE")#theta hat stjerna

plot(seq(2.5,5,0.001),dgamma(seq(2.5,5,0.001),shape=200,scale=1/50),type="l",col="RED",xlab="x",ylab="density function")
hist(rgamma(1000,shape=200,scale=1/50)) # theta hat
bias = mean(bootSample)-mean(ranGamma)
variance = var(bootSample)

######################
statistic <- function(x,d) mean(x[d])
bs = boot::boot(ranGamma,statistic,1000)
hist(bs$t)
#######################
# CIs
bootSample = sort(bootSample)
t25 = bootSample[25]
t975 = bootSample[975]

### basic
lBasic = 2*mean(ranGamma)-t975
UBasic = 2*mean(ranGamma)-t25
### Normal
se = sqrt(variance)
lNormal = mean(ranGamma)-qnorm(0.975,0,1)*se
UNormal = mean(ranGamma)-qnorm(0.025,0,1)*se
### percentile
lPerc = t25
UPerc = t975

#######
bootCI = boot::boot.ci(bs,conf=0.95,type=c("norm","basic","perc"))

####
#studying reliability of boostrap small sample, n=10 but 1000 bootstrap samples

percentages = c()

for(n in seq(10,100,10))
{
  counter <- 0
  for(i in 1:1000)
  {
    ranGammaSmall = rgamma(n,shape=2,scale=2)
    bs = boot::boot(ranGammaSmall,statistic,1000)
    CI = boot::boot.ci(bs,conf=0.95,type=c("basic"))$basic
    if (CI[4] <= 4 && 4 <= CI[5])
      counter=counter+1
  }
  percentages <- append(percentages,counter/1000)
}
plot(seq(10,100,10),percentages,col="RED",xlab="n",xlim=c(10,100),ylim=c(0.85,1),cex=0.85,pch=19)
par(new=TRUE)
plot(seq(0,110,10),rep(0.95,12),col="BLUE",xlim=c(10,100),ylim=c(0.85,1),xlab="",ylab="",axes=FALSE,type="l")
###################

