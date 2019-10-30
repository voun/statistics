


alpha = 0.1
k = 100*alpha
median <- rep(0,1000)
trimmedMean <- rep(0,1000)
meanVec <- c()
for (j in seq(1,1000))
{
  sample <- c()
  for (i in seq(1,100))
  {
    if (runif(1) <= 0.95)
      sample = append(sample,rnorm(1))
    else
      sample = append(sample,rcauchy(1,0))
  }
  sample <- sort(sample)
  median[j] <- median(sample)
  trimmedMean[j] <- mean(sample[(k+1):(100-k)])
  meanVec <- append(meanVec,mean(sample))
}
meanVec = sort(meanVec)
trimmedMean = sort(trimmedMean)
median = sort(median)

hist(meanVec, col = "blue",100)
hist(median, col = "blue",100)
hist(trimmedMean, col = "blue",100)

cat(" 25 and 975 of mean",meanVec[25]," ",meanVec[975])
cat(" 25 and 975 of trimmedmean",trimmedMean[25]," ",trimmedMean[975])
cat(" 25 and 975 of median",median[25]," ",median[975])

