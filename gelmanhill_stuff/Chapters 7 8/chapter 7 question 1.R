# Chapter 7 exercises

# 1. Discrete prob simlation: 

shots <- function(){
  xx <- rbinom(2, 1, .6) # should this not be xx<- rbinom(2, 1, .4)?
  count = 2
  while(!all(xx[c(count, count-1)]==0)) {
    xx <- c(xx, rbinom(1, 1, .6))
    count = count + 1
  }
  length(xx)
}

simz <- replicate(1000, shots())
mean(simz); sd(simz)
hist(simz)
plot(simz)

# In an attempt to answer my own question, I see what happens...
# In first response, using 0.6 for the probabilty, the simulation the likelihood of
# how many shots the shooter can make before missing 2 in a row
# In this answer, taketwo, the shooter is not as good. 
taketwo<- function() {
  miss<- rbinom(2, 1, .4)
  tally = 2
  while(all(miss[c(tally, tally-1)]==0)) {
    miss<- c(miss, rbinom(1, 1, .4))
    tally = tally + 1
  }
  length(miss)
}

n.sims<-replicate(1000, taketwo())
mean(n.sims); sd(n.sims)
hist(n.sims)
plot(n.sims)
