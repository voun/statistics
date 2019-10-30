
statistic <- function(x,d) var(x[d]) # sample variance
percentages = c()

for(n in seq(10,100,10))
{
  counter <- 0
  for(i in 1:1000)
  {
    ranGammaSmall = rgamma(n,shape=2,scale=2)
    bs = boot::boot(ranGammaSmall,statistic,1000)
    CI = boot::boot.ci(bs,conf=0.95,type=c("basic"))$basic
    if (CI[4] <= 8 && 8 <= CI[5])
      counter=counter+1
  }
  percentages <- append(percentages,counter/1000)
  print(n)
}
plot(seq(10,100,10),percentages,col="RED",xlab="n",xlim=c(10,100),ylim=c(0.6,1),cex=0.85,pch=19)
par(new=TRUE)
plot(seq(0,110,10),rep(0.95,12),col="BLUE",xlim=c(10,100),ylim=c(0.6,1),xlab="",ylab="",axes=FALSE,type="l")
##very bad results!! no pivotality? do studentized CI!