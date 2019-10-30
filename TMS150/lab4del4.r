percentages <- c()
for(n in seq(10,100,10))
{
  print(n)
  counter <- 0
  for(i in 1:100)
  {
    ranGammaSmall = rgamma(n,shape=2,scale=2)
    sampleVar = var(ranGammaSmall)
    theta1 <- c()
    w <- c()
    for(j in 1:1000)
    {
      newSample = sample(ranGammaSmall,n,replace=TRUE)
      theta1 <- append(theta1,var(newSample))
      t <- c()
      for(k in 1:50)
        t <- append(t,var(sample(newSample,n,replace=TRUE)))
      se = sqrt(var(t))
      w = append(w,((var(newSample)-sampleVar)/se))
    }
    w = sort(w)
    std = sqrt(var(theta1))
    
    lower = sampleVar-std*w[975]
    upper = sampleVar-std*w[25]
    
    if (lower <= 8 && 8 <= upper)
      counter = counter+1;
  }
  percentages <- append(percentages,counter/100)
}
plot(seq(10,100,10),percentages,col="RED",xlab="n",xlim=c(10,100),ylim=c(0.85,1),cex=0.85,pch=19)
par(new=TRUE)
plot(seq(0,110,10),rep(0.95,12),col="BLUE",xlim=c(10,100),ylim=c(0.85,1),xlab="",ylab="",axes=FALSE,type="l")


