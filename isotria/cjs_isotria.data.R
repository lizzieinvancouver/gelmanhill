i<-as.matrix(read.csv("isotriach",header=F))
i<-i[-1,]
y<-structure(c(i), .Dim = c(255L, 31L))
nind <-
  255L
n_occasions <-
  31L