simJXmatYmatJYmat <- function (N,n,beta) { #Simulering
  JXmat <- matrix(NA, nrow = n, ncol = N)
  Ymat <- matrix(NA, nrow = n, ncol = N)
  
  for (j in 1:N) {
    jx <- c() #Initialisering
    jy <- c()
    y <- c()
    for (i in 1:n) {
      y[i] <- rexp(n=1, beta)
      x <- rexp(n=1, i*beta)
      jx[i] <- i*x
      jy[i] <- i*y[i]
    }
    JXmat[,j] <- jx
    Ymat[,j] <- y
    JYmat[,j] <- jy
  }
  return(JXmat,Ymat, JYmat)
}


simJXmatYmat <- function (N,n,beta) { #Simulering
  JXmat <- matrix(NA, nrow = n, ncol = N)
  Ymat <- matrix(NA, nrow = n, ncol = N)
  for (j in 1:N) {
    jx <- c() #Initialisering
    y <- rexp(n = n, 1/beta)
    for (i in 1:n) {
      jx[i] <- rexp(n=1, i*beta)
      jx[i] <- i*x[i]
    }
    JXmat[,j] <- jx
    Ymat[,j] <- y
  }
  return(JXmat,Ymat)
}

#-------------
A <- matrix(1:9, nrow=3,byrow=T)
B <- matrix(10:18, nrow=3,byrow=T)
C <- matrix(19:27, nrow=3,byrow=T)

A
B
C

rbind(apply(A,2,sum),apply(B,2,sum), apply(C,2,sum))



#----
for (k in 1:n) { #Simulate jx, y and jy vectors (notice that)
  y[k] <- rexp(n=1, 1/beta)
  x <- rexp(n=1, k*beta)
  jx[k] <- k*x
  jy[k] <- k*y[k]
}

