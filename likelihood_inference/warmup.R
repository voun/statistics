##########

x <- seq(0,10,length = 500)
par(mfrow = c(1,2))
plot(x,dlnorm(x,1,0.5),type="l",col="blue")
plot(x,dlnorm(x,1,2),type="l",col="blue")
#########

x <- seq(0,10,length = 1000)
plot(x,dlnorm(x,1,0.5),type="l",col="blue",ylim=c(0,0.7),lty = 1)
lines(x,dlnorm(x,1,2),type="l",col="green",ylim=c(0,0.7),lty =2 )
legend("topright",legend=c("0.5","2"),col=c("blue","green"),lty = 1:2,cex=0.4,bty="n")
######

x <- rnorm(100,0,1)
hist(x,col="red",prob=TRUE)
mean(x)
y <- sort(x)
y[length(x)/2]
######

myfun <- function(t) pexp(t,2)*(1-pnorm(t+1,0,1))
myfun(c(1,2)) #returnerar en vektor eftersom utv??rderar var f??r sig
integrate(myfun,0,10)$value #kan h??mta ut olika grejjer fr??n integralen!
####
fun1 <- function(s,t) s*t+s-t^2-s^3
fun2 <- function(s){
  integrate(function(t) fun1(s,t),0,10)$value
}

sValues = -5:5
answers = c();
for(s in sValues)
  answers = append(answers,fun2(s))
plot(sValues,answers,type="l",col="blue",xlab="s",ylab="ans")
root = uniroot(fun2,lower=-5,upper=2)$root
#####

f <- function(t){
  if( t < 0)
    0
  else if (t >= 0 && t <= 1/2)
    1
  else
    0
}
interArrivalTimes <- function(){
  cumsum = 0
  times = c()
  while(cumsum <= 10){
    rand = rexp(1)
    cumsum = cumsum+rand
    times = append(times,rand)
  }
  times = times[1:length(times)-1]
}

nbrOfCarsIn10 = c()
for(i in 1:1000)
  nbrOfCarsIn10 = append(nbrOfCarsIn10,length(interArrivalTimes()))
hist(nbrOfCarsIn10)
mean(nbrOfCarsIn10)
##
rGamma = rgamma(100,2,2)

nonParamBootStrapMean <- function(x){
  
  theta = c()
  for(i in 1:1000)
    theta = append(theta,mean(sample(x,100,replace=TRUE)))
  theta
}

hist(nonParamBootStrapMean(rGamma))
##

data <- data.frame(
      gender = c("Male","Male","Female","Male"),
      height = 1:4,
      weight = c(831,525,831,420),
      Age = c(42,46,2,50)
)
#summary(data)
MAX = max(data$weight)
rowsWithMaxWeight = subset(data,weight == MAX) #kan ta subset av en data.frame som uppfyller n??got

####

theta = 3
n = 100
x <- rnorm(n,theta,theta)

xbar = mean(x)
xbarbar = sum(x*x)/n

est = -xbar/2+sqrt((xbar/2)^2+xbarbar)


fun <- function(t)
  (1/t)^n*exp(-1/(2*t*t)*sum((x-t)^2))

lol <- seq(0.5,7,length=1000)
plot(lol,sapply(lol,fun),type="l")

###

theta = 4.7
n = 100

x <- rnorm(n,theta,1)
y = max(x)

fun <- function(t)
  n*pnorm(y-t,0,1)^(n-1)*dnorm(y-t,0,1)
lol <- seq(0.2,6,length=1000)

plot(lol,sapply(lol,fun),type="l")

#####

a = 2
n = 300
data <- rgamma(n,shape=a,rate=3)

k = a/mean(data) +c(-1,1)*1.96*1/mean(data)*sqrt(a/n)
print(k)


####

phi <- function(p) log(p/(1-p))
phiinv <- function(p) exp(p)/(1+exp(p))
phat = 1/50

p =100* phiinv(phi(phat)+c(-1,1)*1.96/sqrt(100*phat*(1-phat)))


###

data = rexp(1000,3)
mean = mean(data)
var = var(data)

std = sqrt(var)
data = (data-mean)/std
par(mfrow = c(1,2))
hist(data,col="blue")
hist(data*std+mean,col="red")
##

arr = c("a","b","a","c")
which(arr == "a")
x = c(-1,4,3,0,4,0,1)
which(x == max(x))

##

x = rnorm(10,0,1)
x[-1]
x[-(2:8)]

##

x = rnorm(100,0,1)
d = data.frame(
            a = x,
            b = x+runif(100,0.5,1),
            c = jitter(x^2)
)

pairs(d) ##ger massa scatterplots! dvs punkter

##
data<-data.frame(a=rnorm(100,mean=3,sd=2),
                 b=rnorm(100,mean=4,sd=1),
                 c=rnorm(100,mean=6,sd=3),
                 d=rnorm(100,mean=10,sd=3.5))

boxplot(data,col=c("red","green","blue","purple")) ##l??ddiagram
## visar minsta, f??rsta kvartilen (25%), medianen, tredje kvartilen och st??rsta v??rdet. I boxen ??r 50% av all data


