library(ggplot2)
library(gridExtra)

set.seed(314)

n <- 10^3 #outcomes
mu <- 4
delta <- 3
reprep <- 10^4 #number of repeated experiments

dat <- matrix(NA,n,reprep)

for (i in 1:reprep) {
  dat[,i] <- runif(n, min=mu-delta, max = mu+delta) 
}

datMax <- apply(dat, 2, max) #apply the max function to all columns (2) of dat
datMin <- apply(dat, 2, min)

muhat <- (datMin+datMax)/2
deltahat <- (datMax-datMin)/2

mean(muhat)
mean(deltahat)

p_1 <- qplot(muhat, geom = "histogram")
p_2 <- qplot(deltahat, geom = "histogram")

grid.arrange(p_1,p_2, ncol = 2)


#Verify c
mucheck <- apply(dat, 2, median)
mean(mucheck)


qplot(mucheck)

