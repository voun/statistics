
count <- 0
vec = c();
b <- qchisq(0.975,df=99)
a <- qchisq(0.025,df=99)
for (j in 1:1000){
  v<- rgamma(100,scale=2,shape=2)
  left <- var(v)*99/b
  right <- var(v)*99/a
  vec = append(vec,right-left)
  if (left < 8 & right > 8)
  count = count+1
}
print(count/1000)
print(mean(vec))
  

  



