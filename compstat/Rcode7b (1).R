# Panini: Monte Carlo test

# Background:
#   There are 682 different Panini cards. 
#   The cards are packaged in packs with 5 cards.
#   100 of such packs (so 500 cards) are packaged in a box.
#   Panini seems to say that cards are packaged at random. 
#   My children and their friends suspect that boxes contain 
#      "too few" duplicates: "I will buy a box to have many 
#       different cards, and then some separate packs to swap."

# Results from a box with 100 packs:
#   6 duplicates in 50 packs (250 cards)

# How likely is it to see this result (or more extreme ones) 
#   when the cards are indeed packaged at random with replacement?

# Formally:
# H0: cards are packaged at random with replacement
# We want to compute the p-value: the probability of observing our 
#   results (or more extreme ones, i.e., even fewer duplicates) 
#   when the null hypothesis is true. 
# So we need to know the distribution of the number of 
#   duplicates under H0. This is cumbersome to analyze analytically, 
#   but easy to simulate!

nr.cards <- 682    # number of unique cards
nsimul <- 100000   # number of simulations
npack <- 50        # number of packs 

# Function that simulates sampling cards at random with replacement.
# Returns the number of duplicates in npack packs.
simulate.duplicate <- function(){
  res <- sample(1:nr.cards, 5*npack, replace=T)
  return(5*npack - length(unique(res)))  # nr of duplicates
}

res.1 <- replicate(nsimul,simulate.duplicate())

summary(res.1)
hist(res.1, xlim=c(0,max(res.1)), prob=T)
abline(v=6, col="red")
(pval.1 <- (1+sum(res.1 <= 6))/(nsimul+1))
(rej.1 <- quantile(res.1, 0.05)-1)  # boundary of rejection region at alpha=0.05

# the p-value is extremely small
# our observed value of 6 lies far in the rejection region x<=31
# => we clearly reject H0 at alpha=0.05

# We will now illustrate how to do a power analysis. 
# Let Ha denote an alternative hypothesis. 
# Power = Prob(reject H0 | Ha is true)
#       = Prob(observing value in rejection region | Ha is true)
# To compute this, we need to specify Ha...
# Possible choice:
#   Ha: p*100% of the cards are sampled from the set of all cards 
#       without replacement, and the remaining cards are sampled 
#       from the set of all cards with replacement.
# We need the distribution of the number of duplicates under Ha, and 
#   we again obtain this via simulation. 

simulate.duplicate.p <- function(){
  nr.without <- floor(p*npack*5) # nr of cards drawn without replacement
  nr.with <- npack*5-nr.without  # nr of cards drawn with replacement
  res <- sample(1:nr.cards, nr.without, replace=F)
  res <- c(res, sample(1:nr.cards, nr.with, replace=T))
  return(5*npack - length(unique(res)))
}

# Simulated distributions:
p <- 2/3
res.2 <- replicate(nsimul,simulate.duplicate.p())
summary(res.2)
hist(res.2, xlim=c(0,max(res.2)),prob=T)
(power <- sum(res.2 <=31) / nsimul)

p1 <- hist(res.1)
p2 <- hist(res.2)                     
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,max(res.1,res.2)), main=paste("Power for p=", round(p,2), "equals", round(power,2)), freq=FALSE, ylim=c(0,.12)) 
plot( p2, col=rgb(1,0,0,1/4), add=T, freq=FALSE)
abline(v=rej.1)

# Note: even when p=2/3, it is extremely unlikely to observe <=6 
# duplicates. 

