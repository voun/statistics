# Permutation/randomization tests

# unpaired two sample permutation test
# example of a two sample permutation/randomization test:
library(coin)
data(asat)
?asat
head(asat)
dim(asat)
boxplot(asat~group, data=asat)

# There seems to be a some effect of the treatment. 
# Or is this just due to chance?

# Create different vectors for each group
asat.contr <- asat$asat[asat$group=="Control"]
asat.treat <- asat$asat[asat$group=="Compound"]
n <- nrow(asat)
n1 <- length(asat.contr) #19 (first 19 observations in asat)

# it is suspected that the compound increases asat levels

# observed difference in group means:
(obs.diff <- mean(asat.contr)-mean(asat.treat))

# There seems to be a some effect of the treatment. 
# Or is this just due to chance?

# A first permutation test (one-sided):
#   H0: mu_contr = mu_treat 
#   Ha: mu_contr < mu_treat 
# at significance level alpha=0.01

# How can we create a null distribution?

# The rats were randomly assigned to treatment (compound) 
#   or control.
# If the treatment does nothing (the null hypothesis), 
#   then the asat value of each rat would remain the same, 
#   regardless of its treatment assignment. 
# Hence, we can create random reassignments of the rats to the 
#   two groups, and each time compute the difference in group 
#   means. This yields our null distribution. 

# number of times we randomly reassign rats to groups
reps <- 10000             

# vector to store the difference in group means 
#   under the random reassignments
res.asat <- numeric(reps) 

for (i in 1:reps){
  # change ordering of the asat values
  temp <- asat$asat[sample(c(1:n), replace=FALSE)]
  
  # then consider the first n1 observations to be in 
  #  the control group and the remaining observations 
  #  in the treatment group. 
  res.asat[i] <- mean(temp[1:n1])-mean(temp[(n1+1):n])
} 

hist(res.asat, xlim=c(-2,2))
abline(v=obs.diff, col="red")
# small values of difference are in favour of the alternative:
(pval <- (sum(res.asat<=obs.diff)+1)/(reps+1))

# We do not reject H0 in favor of Ha at alpha=0.01
# There is not enough evidence in the data to conclude that 
#   that the compound increases asat levels. 

######

# Now let's do a Wilcoxon test:
#   H0: F_1 = F_2  (F_1 for control, F_2 for treatment)
#   Ha: F_1 is shifted to the left

wilcox.test(asat.contr, asat.treat, alternative="less")

# We do not reject H0 in favor of Ha at alpha=0.05
# There is not enough evidence in the data to conclude that 
#   that the compound increases asat levels. 


#######################################################

# We now show that the Wilcoxon test can be 
#   viewed as a permutation test:
ranks <- rank(asat$asat)
(observed.sum.ranks <- sum(ranks[1:n1]))

# nr of different group assignments:
choose(34,19)

# we will randomly sample nrep permutations:
nrep <- 100000

wilcox.one.rep <- function(y, n1, n){
  ynew <- sample(y, n, replace=F)
  ranks.new <- rank(ynew)
  return(sum(ranks.new[1:n1]))
}

res.wilcox <- replicate(nrep, wilcox.one.rep(asat$asat, n1, n))

hist(res.wilcox)
abline(v=observed.sum.ranks)
(pval <- (sum(res.wilcox<=observed.sum.ranks)+1)/(nrep+1))


############################################

# More general permutation test:
# we do not have to take the rank sums as test statistic. 
# we can choose any test statistic we like. 
# consider for example the difference in group medians:

# observed difference in group means:
(observed.diff.medians <- median(asat.contr)-median(asat.treat))

median.diff.one.rep <- function(y, n1, n){
  ynew <- sample(y, n, replace=F)
  return(median(ynew[1:n1])-median(ynew[(n1+1):n]))
}

res.diff.medians <- replicate(nrep, median.diff.one.rep(asat$asat, n1, n))

hist(res.diff.medians,breaks=30)
abline(v=observed.diff.medians, col="red")
(pval <- (sum(res.diff.medians<=observed.diff.medians)+1)/(nrep+1))

#########

# Permutation tests are very flexible.
# You can use any statistic you like.
# You don't need any distributional assumptions.
# See R package coin
