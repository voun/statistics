
g <- Vectorize(function(t)
{
  if (t < 0)
    0
  else if( t>=0 && t<= 1/2)
    1
  else
    0
})

generatetimesf <- function()
{
  times <- c()
  sum = 0
  
  while( sum <= 10)
  {
    rand = rexp(1)
    times = append(times,rand)
    sum = sum+rand
  }
  times = times[1:(length(times)-1)]
}
generatetimesb <- function()
{
  times <- c()
  sum = 0
  while(sum <= 0.5)
  {
    rand = rexp(1)
    times = append(times,rand)
    sum = sum+rand
  }
  if(length(times) == 1)
    c()
  else
    times = times[1:(length(times)-1)]
}


X <- function(t,arrivaltimesf,arrivaltimesb)
{
  X = 0
  for(i in 1:length(arrivaltimesf))
    X = X+g(t-arrivaltimesf[i])
  if(length(arrivaltimesb) >=1)
    for(i in 1:length(arrivaltimesb))
      X = X+g(t+arrivaltimesb[i])
  X
}
t <- seq(0,10,length=1000)
arrivaltimesf = cumsum(generatetimesf())
arrivaltimesb = cumsum(generatetimesb())
Y = X(t,arrivaltimesf,arrivaltimesb)
plot(t,Y,cex=0.8,pch=19,col="BLUE")


#######################
count  = 0
for(i in 1:10000)
{
  arrivaltimesf = cumsum(generatetimesf())
  arrivaltimesb = cumsum(generatetimesb())*(-1)
  arrivaltimes = c(rev(arrivaltimesb),arrivaltimesf)
  high = length(arrivaltimes)
  low = high-2
  while(low >=1)
  {
    if ((arrivaltimes[high]-arrivaltimes[low]) <= 1/2)
    {
      count=count+1
      break
    }
    else
    {
      low = low-1;
      high = high-1;
    }
  }
}
phat = count/10000

CIlow = phat-1.96*sqrt(phat*(1-phat)/10000)
CIhigh = phat+1.96*sqrt(phat*(1-phat)/10000)

count = 0
for(i in 1:10000)
{
  arrivaltimesf = cumsum(generatetimesf())
  arrivaltimesb = cumsum(generatetimesb())
  t <- seq(0,10,length=300)
  Y = X(t,arrivaltimesf,arrivaltimesb)
  if (length(Y[Y>= 3]) >= 1)
    count = count+1;
}
phat = count/10000
#################

n = 11
f <- function(t)
{
  if (abs(t) <= 1) 1-t*t
  else 0
}
nums <- rnorm(12*n,0,sqrt(1/n))

int <- Vectorize(function(t)
{
  sum = 0
  for(k in floor(-(1+t)*n):ceiling((1-t)*n))
  {
    m = ceiling((k/n + 11)*n)
    sum = sum+f(k/n+t)*nums[m]
  }
  sum
})
t <- seq(0,10,length=10000)
X = int(t)
plot(t,X,type="l")


