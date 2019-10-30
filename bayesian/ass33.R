
notices = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
count = c(162, 267, 271, 185, 111, 61, 27, 8, 3, 1)
iter = 2000

p_jfun <- function(j, params){
  p = params[1]
  l1 = params[2]
  l2 = params[3]
  
  return(p*exp(-l1)*l1^j/(p*exp(-l1)*l1^j+(1-p)*exp(-l2)*l2^j))
}

l1_start = 0.5
l2_start = 0.5
p_start = 0

stat_points = matrix(numeric(3*1101), ncol=3)

ind = 1
for(k1 in 0:9){
  for(k2 in 0:9){
    for(k3 in 0:10){
      start = c(p_start+0.1*k3, l1_start+k2, l2_start+k1);
      res = matrix(numeric(3*iter), ncol = 3)
      res[1,] = start
      for(i in 2:iter){
        
        p_js = p_jfun(0:9, res[i-1, ])
        
        p_new = sum(count*p_js)/sum(count)
        l1_new = sum(notices*count*p_js)/(1+sum(count*p_js))
        l2_new = sum(notices*count*(1-p_js))/(1+sum(count*(1-p_js)))
        
        res[i,1] = p_new
        res[i,2] = l1_new
        res[i,3] = l2_new
        
      }
      ind = ind+1
      stat_points[ind,] = res[iter,]
    }
  }
}

logpost <- function(par){
  p = par[1]
  l1 = par[2]
  l2 = par[3]
  return(sum(count*log(p*exp(-l1)*l1^notices/factorial(notices)+(1-p)*exp(-l2)*l2^notices/factorial(notices)))-l1-l2)
}

#only need to check 3 by symmetry
stat_points = matrix(numeric(3*3), ncol=3)
stat_points[1,] = c(0, 0, 2.155)
stat_points[2,] = c(0.261, 1.052198, 2.544021)
stat_points[3,] = c(0.5, 2.153, 2.153)

res = c(logpost(stat_points[1,]), logpost(stat_points[2,]), logpost(stat_points[3,]))
#so (p, l1, l2) = (0.262, 1.053, 2.543) is a global maximum to the posterior

ml = sum(notices*count)/sum(count)

plot(notices, count, type="p", col="red", main="poisson", xlab="death notices", ylab="day count", ylim=c(0,300))
lines(notices, 1096*dpois(notices, ml), col="blue", type="p")
legend("topright", legend=c("actual count", "predicted count"), col=c("red", "blue"), lw=c(1,1))

plot(notices, count, type="p", col="red", main="mixture of 2 poissons", xlab="death notices", ylab="day count", ylim=c(0,300))
lines(notices, 1096*(0.261*dpois(notices, 1.052198)+(1-0.261)*dpois(notices, 2.544021)), col="blue", type="p")
legend("topright", legend=c("actual count", "predicted count"), col=c("red", "blue"), lw=c(1,1))


