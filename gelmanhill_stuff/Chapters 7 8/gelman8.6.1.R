#Gelman and Hill Question 8.6.1 for STANLEYI 11/16/16
##a)
rm(list=ls()) 
install.packages("arm")
library("arm")
##define parementers
a=3
b.1<-0.1
b.2<-0.5
x.1<-1:100
sigma<-0.9## do I need this
n<-length(x.1) #do I need this?
x.2<-rbinom(n,1,.5)# not sure if I input this correctly

## not sure what to include for sigma so I followed 8.1
y<-a+b.1*x.1+b.2*x.2+rnorm(n,0,sigma) ##simulate vector y of fake data
lm.1<-lm(y~x.1+x.2)
display(lm.1)
#coefficient comparision- why is the coef.se for x.1 0??
b.1.hat<-coef(lm.1)[2]
b.1.se<-se.coef(lm.1)[2]
b.2.hat<-coef(lm.1)[3]
b.2.se<-se.coef(lm.1)[3]

##check if paremeters fall in 68 and 95 ci
cover.68b1<-abs(b.1-b.1.hat)<b.1.se
cover.95b1<-abs(b.1-b.1.hat)<2*b.1.se
cover.68b2<-abs(b.2-b.2.hat)<b.2.se
cover.95b2<-abs(b.2-b.2.hat)<2*b.2.se
cat (paste ("68% coverage: ", cover.68b1, "\n"))
cat (paste ("95% coverage: ", cover.95b1, "\n"))
cat (paste ("68% coverage: ", cover.68b2, "\n"))
cat (paste ("95% coverage: ", cover.95b2, "\n"))
##something is wrong-for this example to make sense b.1 should probably fall within the 68% interval.
# b)do the loopy
n.fake<-1000
cover.68b1<-rep(NA,n.fake)
cover.68b2<-rep(NA,n.fake)
cover.95b1<-rep(NA,n.fake)
cover.95b2<-rep(NA,n.fake)
for(s in 1:n.fake){
y<-a+b.1*x.1+b.2*x.2+rnorm(n,0,sigma)
lm.1<-lm(y~x.1+x.2)
b.1.hat<-coef(lm.1)[2]
b.1.se<-se.coef(lm.1)[2]
b.2.hat<-coef(lm.1)[3]
b.2.se<-se.coef(lm.1)[3]
cover.68b1[s]<-abs(b.1-b.1.hat)<b.1.se
cover.95b1[s]<-abs(b.1-b.1.hat)<2*b.1.se
cover.68b2[s]<-abs(b.2-b.2.hat)<b.2.se
cover.95b2[s]<-abs(b.2-b.2.hat)<2*b.2.se
}
cat (paste ("68% coverage: ", mean(cover.68b1), "\n"))
cat (paste ("95% coverage: ", mean(cover.95b1), "\n"))
cat (paste ("68% coverage: ", mean(cover.68b2), "\n"))
cat (paste ("95% coverage: ", mean(cover.95b2), "\n"))
## this seems to actually work, which from the problem means something is wrong
#T time--but things are so screwy the book says I should have  4 degrees of 
#freedom but that seems wrong for the amount of observations I generated
##I'm going try it but iits
n.fake<-1000
cover.68b1<-rep(NA,n.fake)
cover.68b2<-rep(NA,n.fake)
cover.95b1<-rep(NA,n.fake)
cover.95b2<-rep(NA,n.fake)
t.68<-qt (.84,n-2) ##why  n-2
t.95<-rep(.975,n-2)
for(s in 1:n.fake){
  y<-a+b.1*x.1+b.2*x.2+rnorm(n,0,sigma)
  lm.1<-lm(y~x.1+x.2)
  b.1.hat<-coef(lm.1)[2]
  b.1.se<-se.coef(lm.1)[2]
  b.2.hat<-coef(lm.1)[3]
  b.2.se<-se.coef(lm.1)[3]
  cover.68b1[s]<-abs(b.1-b.1.hat)<t.68*b.1.se
  cover.95b1[s]<-abs(b.1-b.1.hat)<t.95*b.1.se
  cover.68b2[s]<-abs(b.2-b.2.hat)<t.68*b.2.se
  cover.95b2[s]<-abs(b.2-b.2.hat)<t.95*b.2.se
}
cat (paste ("68% coverage: ", mean(cover.68b1), "\n"))
cat (paste ("95% coverage: ", mean(cover.95b1), "\n"))
cat (paste ("68% coverage: ", mean(cover.68b2), "\n"))
cat (paste ("95% coverage: ", mean(cover.95b2), "\n"))
#whoa this made it way worse

