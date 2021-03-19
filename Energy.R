library(energy)

d=200; m=20;n=20

library(MASS)
library(cramer)

I<-diag(rep(1, d))
mu_X<-rep(0,d) ; mu_Y<- mu_X + 0.15

S_X <- I ; S_Y <- 1.5*I
## S_X <- diag(c(rep(1, d/2), rep(2, d/2))) ; S_Y <- diag(c(rep(2, d/2), rep(1, d/2)))
## mu_X<-mu; mu_Y<-mu+0.25

p_val<- c()



for(q in 1:500){
  
  X<- mvrnorm(m, mu_X, S_X) ; Y<-mvrnorm(n, mu_Y, S_Y)
  Energy<- cramer.test(X,Y,kernel = "phiLog" )
  p_val[q]<- Energy$p.value
  
}

sum(p_val<=0.05)/500
