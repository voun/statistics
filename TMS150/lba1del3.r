
sample <- c()
for (i in seq(1,100))
  if (runif(1) <= 0.95)
    sample = append(sample,rnorm(1))
  else
    sample = append(sample,rt(1,1))
hist(sample,col="blue",100)





  