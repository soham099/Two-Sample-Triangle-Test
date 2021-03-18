library(cramer)

d=50; m=20;n=20

library(MASS)
library(mvtnorm)

I<-diag(rep(1, d))
mu<-rep(0,d)

S_X <- I ; S_Y <- 1.5*I
p_val<- c()

for(q in 1:100){
  
  X<- mvrnorm(m, mu, S_X) ; Y<-mvrnorm(m, mu, S_Y)
  BF<- cramer.test(X, Y, kernel="phiCramer")
  p_val[q]<- BF$p.value
  
}

for(q in 1:500){
  
  X<- mvrnorm(m, mu, S_X) ; Y<-mvrnorm(m, mu, S_Y)
  Energy<- indep.test(X,Y, method = "mvI", R=500)
  p_val[q]<- Energy$p.value
  
}

sum(p_val<=0.05)/500
p_val
