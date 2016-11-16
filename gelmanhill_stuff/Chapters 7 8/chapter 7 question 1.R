# Chapter 7 exercises

# 1. Discrete prob simlation: 

shots <- function(){
  xx <- rbinom(2, 1, .6)
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
