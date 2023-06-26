#Opgave 2.6
set.seed(314)

N <- 10^3
n <- c(20,200,2000)
beta <- 0.1

inbf <- function(n,beta) { #FisherInfoAfBeta
  2*n/(beta^2)
}

sim <- function (N,n,beta) { #Simulering
  MLE_n <- c() # Initialisering
  for (j in 1:N) {
    x <- c() #Initialisering
    y <- rexp(n = n, 1/beta)
    for (i in 1:n) {
      x[i] <- rexp(n=1, i*beta)
      x[i] <- i*x[i]
    }
    MLE_n[j] <- sqrt((sum(y))/(sum(x)))
  }
  MLE_n
}

meanvarsim <- function (N,n,beta) { #Middelværdi og varians af data
    c(mean(sim(N,n,beta)), var(sim(N,n,beta)))
}

difsim <- function(N, n, beta) { #Differenser
  q <- meanvarsim(N, n, beta)
  difmean <- (abs(q[1]-beta))
  difvar <- abs(q[2]-1/(inbf(n,beta)))
  rbind(difmean, difvar)
}


meanvarsim(N=N, n=n[1], beta = beta)
meanvarsim(N=N, n=n[2], beta = beta)
meanvarsim(N=N, n=n[3], beta = beta)

difsim(N, n[1], beta) #(1.082893e-03, 6.061009e-06) <-> n = 20
difsim(N, n[2], beta) #(2.150822e-04, 9.872272e-07) <-> n = 200
difsim(N, n[3], beta) #(1.961224e-06, 3.517521e-08) <-> n = 2000

fMLE_nk <- function(n,beta,k,x) { #Tæthed N(beta,fisher^(-1))
  dnorm(x, mean = beta, sd = sqrt(1/(inbf(n[k],beta))))
}

par(mfrow=c(1,3)) # Plotvindue til 3 plots

plottheplots <- function(N,n,beta,k,mea,vari) {
  data <- sim(N,n[k],beta)
  seqT <- seq(min(data),max(data), by = 1/(5*10^6*(max(data)-min(data))))
  f1T <- dnorm(seqT, mean=mea, sd=sqrt(vari))
  maxf1T <- max(f1T)+max(0.2,max(f1T)/100)
  hist(data, ylim = c(0,maxf1T), breaks = 15, prob = T)
  lines(seqT,f1T)
}

for (q in 1:3) {
  k=q
  plottheplots(N=N,n=n,beta=beta,q,mea=beta, vari=1/(inbf(n[q],beta)))
}


#Opgave 2.7
set.seed(314)

N <- 10^3
n <- c(20,200,2000)
beta <- 0.1



simJXmatYmatJYmat <- function (N,n,beta) { 
  JXmat <- matrix(NA, nrow = n, ncol = N) #Setup
  Ymat <- matrix(NA, nrow = n, ncol = N)
  JYmat <- matrix(NA, nrow = n, ncol = N)
  for (j in 1:N) {
    jx <- c() #Setup
    jy <- c()
    y <- c()
    for (k in 1:n) { #Simulate jx, y and jy vectors 
      #(notice that we use notation k to do this in R)
      y[k] <- rexp(n=1, 1/beta)
      x <- rexp(n=1, k*beta)
      jx[k] <- k*x
      jy[k] <- k*y[k]
    }
    JXmat[,j] <- jx #Having simulated n draws, we insert it into a matrix column
    Ymat[,j] <- y
    JYmat[,j] <- jy
  }
  #Sum the columns of each matrix (resulting in a vector of length N) that 
  #we then row-bind to the other column sums, and output.
  rbind(apply((JXmat),2, sum), apply((Ymat),2, sum), apply((JYmat),2, sum))
}

simLikeRatio <- function(N,n,beta) {
  #Grabbing the output of simJXmatYmatJYmat
  data <- simJXmatYmatJYmat(N,n,beta)
  sumJXmat <- data[1,]
  sumYmat <- data[2,]
  sumJYmat <- data[3,]
  
  likeratio <- c()
  for (k in 1:N) {
    #Breaking down the six calculated terms of the likelihood ratio test static
    first <- 2*n*log((n)/(sumJXmat[k])) #First term ...
    second <- 2*n*log((n)/(sumYmat[k]))
    third <- -2*n
    fourth <- -2*(n/(sumYmat[k]))*sumJYmat[k]
    fifth <- 2*sqrt(sumYmat[k])*sqrt(sumJXmat[k])
    sixth <- 2*sqrt((sumJXmat)/(sumYmat))*sumJYmat
    likeratio[k] <- first+second+third+fourth+fifth+sixth #collecting the terms
    }
   likeratio #Outputting the resulting vector of N likelihood ratio statics
}

par(mfrow=c(1,3)) # Plotvindue til 3 plots
likeratiosim <- simLikeRatio(N,n[3],beta)
hist(likeratiosim, breaks=30,prob=T)

rchisq(n = n[1],1)
hist(rchisq(n[2],1), breaks=15, prob=T)
