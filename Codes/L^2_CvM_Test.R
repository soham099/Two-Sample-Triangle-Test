L2_CvM_Test<- function(X, Y, m, n, d){
  norm<-function(x){
    return( sqrt(sum(x^2))  )
  }
  
  ang<- function(x,y){
    return(acos( sum(x*y)/( norm(x)*norm(y) )   ))
  }
  
  h <- function(Xi, Xj, Yk){
    return(ang(Xi-Yk, Xj-Yk))
  }
  
  ### store values of kernel in a 3-dimensional tensor
  
  
  arr_XXX <- array(NA, dim=c(m,m,m))
  for(i in 1:m){
    for(j in 1:m){
      if(j!=i){
        for(k in 1:m){
          if(k!=j && k!=i){
            arr_XXX[i,j,k] <- h(X[i,], X[j,], X[k,])
          }
        }
      }  
    }
  }
  
  
  arr_XXY <- array(NA, dim=c(m,m,n))
  for(i in 1:m){
    for(j in 1:m){
      if(j!=i){
        for (k in 1:n) {
          arr_XXY[i,j,k] <- h(X[i,], X[j,], Y[k,])
        }
      }  
    }
  }
  
  
  arr_YYX <- array(NA, dim=c(n,n,m))
  for(k in 1:n){
    for(l in 1:n){
      if(l!=k){
        for (i in 1:m) {
          arr_YYX[k,l,i] <- h(Y[k,], Y[l,], X[i,])
        }
      }  
    }
  }
  
  arr_YYY <- array(NA, dim=c(n,n,n))
  for(i in 1:n){
    for(j in 1:n){
      if(j!=i){
        for(k in 1:n){
          if(k!=j && k!=i){
            arr_YYY[i,j,k] <- h(Y[i,], Y[j,], Y[k,])
          }
        }
      }  
    }
  }
  
  
  arr_XYX <- array(NA, dim=c(m,n,m))
  for(i in 1:m){
    for(j in 1:n){
      for(k in 1:m){
        if(k !=i){
          arr_XYX[i,j,k]<- h(X[i,], Y[j,], X[k,])
        }
      }
    }
  }
  
  arr_XYY <- array(NA, dim=c(m,n,n))
  for(i in 1:m){
    for(j in 1:n){
      for(k in 1:n){
        if(k !=j){
          arr_XYY[i,j,k]<- h(X[i,], Y[j,], Y[k,])
        }
      }
    }
  }
  
  
  
  
  ## U1
  
  T1 <- 0
  
  for(i in 1:m){
    for (j in 1:m) {
      if(j !=i){
        for(k in 1:n){
          
          T1 <- T1 + (arr_XXY[i,j,k]-pi/3)^2
        }
      }    
    }
    
  }
  
  
  comb <- function(n, r){
    return( factorial(n)/(factorial(r) * factorial(n-r) ))
  }
  
  
  
  
  T2 <- 0
  
  for(i in 1:m){
    
    for(k in 1:n){
      for(l in 1:n){
        if(l !=k){
          T2 <- T2 + (arr_YYX[k,l,i] - pi/3)^2
        }
      }
    }
    
  }
  
  U <-  (T1+T2)
  
  
  ### permutation
  
  B<- 200
  N<- m+n
  Z<-c()
  
  for(B in 1:200){
    perm <- sample(1:N,size=N, replace = FALSE)
    
    X_perm <- perm[1:m] ; Y_perm <- perm[(m+1):N]
    
    T1_perm <-0 ; T2_perm <-0
    
    for(i in 1:m){
      for(j in 1:m){
        if(j!=i){
          for(k in 1:n){
            if(X_perm[i]<=m && X_perm[j]<=m && Y_perm[k]>m ){
              T1_perm <- T1_perm + (arr_XXY[X_perm[i], X_perm[j], Y_perm[k]-m]-pi/3)^2
            }
            else if(X_perm[i]<=m && X_perm[j]<=m && Y_perm[k]<=m ){
              T1_perm <- T1_perm + (arr_XXX[X_perm[i], X_perm[j], Y_perm[k]]-pi/3)^2
            }
            else if(X_perm[i]>m && X_perm[j]>m && Y_perm[k]<=m){
              T1_perm <- T1_perm + (arr_YYX[X_perm[i]-m, X_perm[j]-m, Y_perm[k]]-pi/3)^2
            }
            else if(X_perm[i]>m && X_perm[j]>m && Y_perm[k]>m){
              T1_perm <- T1_perm + (arr_YYY[X_perm[i]-m, X_perm[j]-m, Y_perm[k]-m]-pi/3)^2
            }
            else if(X_perm[i]<=m && X_perm[j]>m && Y_perm[k]<=m){
              T1_perm <- T1_perm + (arr_XYX[X_perm[i], X_perm[j]-m, Y_perm[k]]-pi/3)^2
            }
            else if(X_perm[i]<=m && X_perm[j]>m && Y_perm[k]>m){
              T1_perm <- T1_perm + (arr_XYY[X_perm[i], X_perm[j]-m, Y_perm[k]-m]-pi/3)^2
            }
            else if(X_perm[i]>m && X_perm[j]<=m && Y_perm[k]<=m){
              T1_perm <- T1_perm + (arr_XYX[X_perm[j], X_perm[i]-m, Y_perm[k]]-pi/3)^2
            }
            else if(X_perm[i]>m && X_perm[j]<=m && Y_perm[k]>m){
              T1_perm <- T1_perm + (arr_XYY[X_perm[j], X_perm[i]-m, Y_perm[k]-m]-pi/3)^2
            }
          }
        }  
      }
    }
    
    
    for(i in 1:n){
      for(j in 1:n){
        if(j!=i){
          for(k in 1:m){
            if(Y_perm[i]<=m && Y_perm[j]<=m && X_perm[k]>m ){
              T2_perm <- T2_perm + (arr_XXY[Y_perm[i], Y_perm[j], X_perm[k]-m]-pi/3)^2
            }
            else if(Y_perm[i]<=m && Y_perm[j]<=m && X_perm[k]<=m ){
              T2_perm <- T2_perm + (arr_XXX[Y_perm[i], Y_perm[j], X_perm[k]]-pi/3)^2
            }
            else if(Y_perm[i]>m && Y_perm[j]>m && X_perm[k]<=m){
              T2_perm <- T2_perm + (arr_YYX[Y_perm[i]-m, Y_perm[j]-m, X_perm[k]]-pi/3)^2
            }
            else if(Y_perm[i]>m && Y_perm[j]>m && X_perm[k]>m){
              T2_perm <- T2_perm + (arr_YYY[Y_perm[i]-m, Y_perm[j]-m, X_perm[k]-m]-pi/3)^2
            }
            else if(Y_perm[i]<=m && Y_perm[j]>m && X_perm[k]<=m){
              T2_perm <- T2_perm + (arr_XYX[Y_perm[i], Y_perm[j]-m, X_perm[k]]-pi/3)^2
            }
            else if(Y_perm[i]<=m && Y_perm[j]>m && X_perm[k]>m){
              T2_perm <- T2_perm + (arr_XYY[Y_perm[i], Y_perm[j]-m, X_perm[k]-m]-pi/3)^2
            }
            else if(Y_perm[i]>m && Y_perm[j]<=m && X_perm[k]<=m){
              T2_perm <- T2_perm + (arr_XYX[Y_perm[j], Y_perm[i]-m, X_perm[k]]-pi/3)^2
            }
            else if(Y_perm[i]>m && Y_perm[j]<=m && X_perm[k]>m){
              T2_perm <- T2_perm + (arr_XYY[Y_perm[j], Y_perm[i]-m, X_perm[k]-m]-pi/3)^2
            }
          }
        }
      }
    }
    
    Z[B]<- T1_perm + T2_perm
    
  }
  p_val<- (sum(Z>=U)+1)/(B+1)
  
  return(list("Unscaled Test Statistic"=U , "p-value"=p_val ))
}