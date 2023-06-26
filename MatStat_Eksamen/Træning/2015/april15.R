setwd("~/Teaching/Courses/Stat1/Eksamen/April15")

########

###### Opgave 1

mle <- function(n0,n) 2*(n-n0)/n 
logL <- function(theta,n0,n)  n0*log(1-theta/2) + (n-n0)*log(theta)

### For data
mle(3,15)
logL(1,3,15)
logL(mle(3,15),3,15)
LR <- 2 * (logL(mle(3,15),3,15) - logL(1,3,15))
LR
1-pchisq(5.78,1)

### Simulationsstudie:

simStudie <- function(trueTheta,n,M)
{
  LR <- rep(NA,M)
  
  for (i in 1:M)
  {
    N0 <- rbinom(1,n,1-trueTheta/2)
    est <- mle(N0,n)
    LR[i] <- 2 * (logL(est,N0,n) - logL(1,N0,n))    
  }
  mean(LR > 3.84)
}
  
simStudie(trueTheta=1, n=50, M=5000)
simStudie(trueTheta=1, n=250, M=5000)
simStudie(trueTheta=1.2, n=50, M=5000)
simStudie(trueTheta=1.2, n=250, M=5000)
simStudie(trueTheta=1.4, n=50, M=5000)

################

###### Opgave 2

Sigma <- matrix( c(1,2,0,2,4,0,0,0,9),3,3)
Sigma
det(Sigma)
 
C <- matrix(c(1,1,1,1,1,-1),2,3,byrow=TRUE)
C
C %*% Sigma %*% t(C)

A <- matrix(c(1,-.5),1,2)
A
A %*% Sigma[1:2,1:2] %*% t(A)

##############

###### Opgave 3
 
L2Data <- read.table("heste-L2.txt", header=TRUE)
dim(L2Data)
head(L2Data)
with(L2Data, plot(grp, L2))
with(L2Data, plot(grp, log(L2)))
 
model1 <- lm(L2 ~ grp, data=L2Data)
plot(fitted(model1), rstandard(model1), main="L2 som respons")
abline(h=0, lty=2)
#dev.print(pdf, "res1.pdf")

model2 <- lm(log(L2) ~ grp, data=L2Data)
plot(fitted(model2), rstandard(model2), main="log(L2) som respons")
abline(h=0, lty=2)
#dev.print(pdf, "res2.pdf")

model2A <- lm(log(L2) ~ grp-1, data=L2Data)
summary(model2A)

model3 <- lm(log(L2) ~ 1, data=L2Data)
anova(model3, model2)

halt <- rep("ja", 85)
halt[L2Data$grp=="Rask"] <- "nej"
halt <- factor(halt)
halt

model4 <- lm(log(L2) ~ halt, data=L2Data)
anova(model4, model2)
summary(model4)
confint(model4)
