### Stat1 eksamen, april 2017

############

### Opgave 1

f1 <- qf(0.025, df1=14, df2=14)
f2 <- qf(0.975, df1=14, df2=14)

x <- c(1.90, 3.64, 5.79, 4.36, 4.15, 0.04, 1.93)
y <- c(1.40, 0.74, 0.53, 0.41, 0.32, 0.25, 0.80)
n <- length(x)

Sx <- sum(x)
Sy <- sum(y)

c(sqrt(f1*Sx/Sy), sqrt(f2*Sx/Sy))

thetaHat <- sqrt(Sx/Sy)
fisher <- 2*n/thetaHat^2
thetaHat + c(-1,1)*1.96/sqrt(fisher)


############

### Opgave 2

### Sandsynlighederne for at ramme [0,2]
pnorm(sqrt(2)) - pnorm(-sqrt(2))
2*(pnorm(3,0,sqrt(7))- pnorm(sqrt(5),0,sqrt(7)))

#################

### Opgave 3

setwd("~/Teaching/Courses/Stat1/Eksamen/April17")
shData <- read.table("shootheight.txt", header=TRUE)

summary(shData)
stripchart(sh ~ group, data=shData)

model1 <- lm(sh ~ group, data=shData)

pdf("modelkontrol.pdf", width=14, height=7)
par(mfrow=c(1,2))
source("../April16/gfx.R")
plot(fitted(model1), rstandard(model1))
abline(h=0, lty=2)
qqnorm(rstandard(model1))
abline(0,1, lty=2)
dev.off()

model2 <- lm(sh ~ 1, data=shData)
anova(model2, model1)

summary(model1)
confint(model1)
model3 <- lm(sh ~ relevel(group,ref="control2"), data=shData)
summary(model3)
confint(model3)

model4 <- lm(sh ~ group-1, data=shData)
summary(model4)

C <- matrix(c(-0.5, -0.5, +0.5, +0.5), 1, 4)
est <- C %*% coef(model4)
est
se <- sqrt(C %*% vcov(model4) %*% t(C))
se
qt(0.975, df=66)
se * qt(0.975, df=66)
est + c(-1,1) * se* qt(0.975, df=66)

################

### Opgave 4

x <- c(1.74,  2.07, -1.20, -0.40, -1.97, -1.87, -2.16,  1.48, -1.00, -1.02)
n <- length(x)
SSx <- sum(x^2)
SSx/n
sigmaEst <- sqrt(mean(x^2))
LR <- -n*log(SSx/n) - n + SSx
# 2*sum(dnorm(x,0,sigmaEst, log=TRUE)) - 2*sum(dnorm(x,0,1, log=TRUE))
1-pchisq(5.95,1)

qchisq(0.025, df=10)
qchisq(0.975, df=10)

sim2 <- function(M=10, sigmaTrue=1, n=10)
{
  res <- matrix(NA,M,3)
  for (i in 1:M)
  {
    x <- rnorm(n,0,sigmaTrue)
    SSx <- sum(x^2)
    sigmaEst <- sqrt(mean(x^2))
    LR <- -n*log(SSx/n) - n + SSx
    res[i,1:2] <- c(sigmaEst^2, LR)
    res[i,3] <- (SSx < qchisq(0.025,n)) | (SSx > qchisq(0.975,n))
  }
  res
  print(mean(res[,2]>3.84))
  print(mean(res[,3]))
}

a <- sim2(M=10000, sigmaTrue=sqrt(1))
a <- sim2(M=10000, sigmaTrue=sqrt(0.5))
a <- sim2(M=10000, sigmaTrue=sqrt(1.5))
