### Stat1-eksamen, august 2017

setwd("~/Teaching/Courses/Stat1/Eksamen/Juni17")

##########

########### Opgave 1

n <- 10
t <- 1:n/n
x <-  c(0.88, 0.86, 0.62, 0.79, 0.66, 0.54, 0.88, 0.87, 0.73, 0.79)
t
x

hatTheta <- - n / sum(t*log(x))
hatTheta
n/hatTheta^2

SY <- - sum(t*log(x))
g1 <- qgamma(0.025, shape=n, scale=1)
g2 <- qgamma(0.975, shape=n, scale=1)
KI <- c(g1/SY, g2/SY)
KI

theta0 <- 4
ybar <- SY/n
LR <- 2*n* (-log(theta0) + theta0*ybar - log(ybar) - 1)
LR
1-pchisq(LR,1)

theta1 <- 3.1754
theta2 <- 11.1151
2*n* (-log(theta1) + theta1*ybar - log(ybar) - 1)
2*n* (-log(theta2) + theta2*ybar - log(ybar) - 1)

### Wald - som der ikke spørges om:
hatTheta + c(-1,1) * 1.96 * hatTheta/sqrt(n)


sim <- function(M,n,theta)
{
  t <- 1:n/n
  est <- rep(NA,M)
  for (i in 1:M)
  {
    x <- rbeta(n, shape1=t*theta, shape2=1)
    est[i] <- - n/sum(t*log(x))
  }
  res <- c(mean(est), sd(est), theta, theta/sqrt(n))
  print(res)
  hist(est)
  est
}

a <- sim(5000,10,5)
a <- sim(5000,25,5)
a <- sim(5000,250,5)

########### Opgave 2

Sigma <- matrix(c(5,-1,6,-1,6,-7,6,-7,13),3,3)
Sigma
det(Sigma)

C <- matrix(c(1,-1,0,0,1,2),2,3, byrow=TRUE)
C
C %*% Sigma %*% t(C)
det(C %*% Sigma %*% t(C))

D <- matrix(c(1,-1,-1),1,3)
D
D %*% Sigma %*% t(D)

########### Opgave 3

setwd("~/Teaching/Courses/Stat1/Eksamen/Juni17")
limData <- read.table("lim.txt", header=TRUE)
summary(limData)

### Spg 1

model1 <- lm(styrke ~ A + B + C, data=limData)
pdf("modelkontrol.pdf", width=14, height=7)
par(mfrow=c(1,2))
source("../April16/gfx.R")
plot(fitted(model1), rstandard(model1)); abline(h=0, lty=2)
qqnorm(rstandard(model1)); abline(0,1, lty=2)
dev.off()

### Spg 2

summary(model1)

### Spg 3

C <- matrix(c(0,1,-1,0),1,4)        
est <- C %*% coef(model1)
est
se <- sqrt(C %*% vcov(model1) %*% t(C))
se
est + c(-1,1) * qt(0.975,52) * se
est/se
2*pt(-est/se, df=52)

### Spg 4

newData <- data.frame(A=1, B=0, C=1)
predict(model1, newData, interval="p")

### Spg 5

limData <- transform(limData, AB=A*B)
model2 <- lm(styrke ~ A + B + C + AB, data=limData)
summary(model2)
confint(model2)

