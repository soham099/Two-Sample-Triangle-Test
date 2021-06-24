

norm<-function(x){
  return( sqrt(sum(x^2))  )
}



Test_Stat<- function(X, Y, m, n, d){
  mu_FF <- 0
  
  for(i in 1:(m-1)){
    for(j in (i+1):m){
      mu_FF <- mu_FF + (norm(X[i,]-X[j,]))
    }
  }
  
  mu_FF <- mu_FF / choose(m,2)
  
  mu_FG <- 0
  
  for(i in 1:m){
    for (j in 1:n) {
      mu_FG <- mu_FG + (norm(X[i,]-Y[j,]))
    }
  }
  
  mu_FG <- mu_FG / (m*n)
  
  mu_GG <- 0
  
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      mu_GG <- mu_GG + (norm(Y[i,]-Y[j,]))
    }
  }
  
  mu_GG <- mu_GG / choose(n,2)
  
  mu_D_F<- c(mu_FF, mu_FG); mu_D_G <- c(mu_FG, mu_GG)
  
  T<- (norm(mu_D_F-mu_D_G))^2
  
  return(T)
}



BG_Test<- function(X, Y, m, n, d){
  
  U<- Test_Stat(X,Y,m,n,d)
  
  Z<- rbind(X, Y)
  
  ###################
  
  B<-200; N<- m+n; samp<- c()
  
  for(b in 1:B){
    perm <- sample(1:N,size=N, replace = FALSE)
    
    X_perm <- Z[perm[1:m],] ; Y_perm <- Z[perm[(m+1):N],]
   
    samp[b]<- Test_Stat(X_perm, Y_perm, m,n,d)
    
    }
  
  
  p_val <- (sum(samp>=U)+1)/(B+1)
  
  return(list("Test Statistic"=U , "p-value"=p_val ))
}
