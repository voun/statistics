###### Prob 1a)

dwei <- Vectorize(function(x,k,s){
  
  if (x < 0)
    0
  else 
    k/s*(x/s)^(k-1)*exp(-(x/s)^k)
  
}) 
x <- seq(0,7,length = 100)
k = c(0.5,1.0,1.5,5.0)
kStr = c("k=0.5","k=1.0","k=1.5","k=5.0")
s = c(1,1.5,2)

par(mfrow = c(2,2))
for(i in 1:4){
  plot(x,dwei(x,k[i],s[1]),col="blue",lty=1,ylim=c(0,2),type="l",ylab="y")
  title(kStr[i])
  lines(x,dwei(x,k[i],s[2]),col="green",lty=2,ylim=c(0,2),type="l",ylab="y")
  lines(x,dwei(x,k[i],s[3]),col="red",lty=3,ylim=c(0,2),type="l",ylab="y")
  legend("topright",legend=c("s=0.5","s=1.5","s=2"),col=c("blue","green","red"),lty=1:3,bty="n",cex=0.25)
}
### Prob 1b)
dwei <- Vectorize(function(x,k,s){
  
  if (x < 0)
    0
  else 
    k/s*(x/s)^(k-1)*exp(-(x/s)^k)
  
}) 
x <- seq(0,7,length = 100)
strings = c("k<1","k=1","k>1")
kLessThan1 = c(0.1,0.25,0.5,0.75)
kIs1 = c(1)
kLargerThan1 = c(1.5,3,6,15)
colours = c("black","blue","red","green")
values = list(kLessThan1,kIs1,kLargerThan1) #typ array av arrays!
par(mfrow = c(1,3))

for(i in 1:3){
  
  vec = unlist(values[i])
  ks = c()
  for(j in 1:length(vec)){
    
    if((vec[j])%%1 == 0) ##i.e is integer
      ks = append(ks,paste("k=",sprintf("%d",vec[j]),sep=""))
    else
      ks = append(ks,paste("k=",sprintf("%1.2f",vec[j]),sep=""))
    
    if (j ==1){
      plot(x,dwei(x,vec[j],1),col=colours[j],ylim=c(0,2),ylab="y",type="l",lty=1)
      title(strings[i])
    }
    else
      lines(x,dwei(x,vec[j],1),col=colours[j],ylim=c(0,2),ylab="y",type="l",lty=1)
  }
  legend("topright",legend=ks,col=colours[1:length(vec)],lty=rep(1,length(vec)),bty="n",cex=0.75)
}

### 

data <- read.csv("/Users/andreas/Documents/workspace/R/hospital.csv")
hist(data$stay) # kan vara poisson eftersom antar bara heltal
                # och vanligt med sm?? v??rden och v??ldigt ovanligt med stora
                # problemet ??r att med poisson s?? antar v??rdet 0 med
                # sannolikhet e^(-lambda)
print(any(data$stay == 0)) #men problemet ??r att ingen antar v??rdet 0.








