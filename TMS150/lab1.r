v <- rgamma(100,scale=2,shape=2)
hist(v,col = "blue",main = "hist gamma")

var = var(v) #MLE
mean = mean(v)

v = sort(v)

x = (1:100)/100
y = pnorm(v,mean,sqrt(var))


plot(x,y)
lines(c(0,1),c(0,1))


testSample = rnorm(100000,mean,sqrt(var))
D=max(abs(pnorm(v,mean,sqrt(var))-(1:100)/100))
print(D)
test = ks.test(v,testSample,alternative="two.sided")
test



