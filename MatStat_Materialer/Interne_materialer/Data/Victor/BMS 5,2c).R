set.seed(314)
kappa1 <- 0.1
kappa2 <- 0.5
kappa3 <- 1
kappa4 <- 5
kappa5 <- 10
kappa6 <- 100
kappavec <- c(kappa1,kappa2,kappa3,kappa4,kappa5,kappa6)

#One might also quantile transform to get data
library(circular)
n <- 100

#As we have shown in b) that Log-likelihood ratio is a pivot, is does not matter
# for the distribution of the log-likelihood ratio which theta we choose to simulate with
# so, choose theta=0.
theta <- 0
DataMat <- matrix(NA, nrow=n, ncol = 6)
for (i in 1:6) {
  DataMat[,i] <-  rvonmises(n, mu = theta, kappavec[i]) 
}

WELLORDER <- function(x,...) {
  x[order(x)]
}

Orderstat <- function(x,order,...) {
  x[order]
}
#The above doesn't work with apply - why?

ninetyfifthstat <- function(x,...) {
  x[95]
}
#Better way to avoid using for loops?

#order each column of DataMat
orderDM <- apply(DataMat, 2, WELLORDER)
orderDM[,4]


ninetyfifth <- apply(orderDM, 2, function(x) x[95]) #2 = applied to columns
ninetyfifth







