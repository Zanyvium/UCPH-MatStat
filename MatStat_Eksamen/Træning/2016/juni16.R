### Stat1, juni 2016
### Helle Sørensen

setwd("~/Teaching/Courses/Stat1/Eksamen/Juni16")

#################

### Opgave 1

t <- seq(0.1, 1, 0.1)
x <- c(1,3,0,1,0,1,6,4,3,2)
n <- length(x)

# betaTilde, incl SE
beta1 <- mean(x/t) 
sqrt(beta1/n^2 * sum(1/t))

# MLS, dvs betaCheck, incl SE og KI
beta2 <- sum(x) / sum(t)
sqrt(beta2/sum(t))
beta2 + c(-1,1) * 1.96 * sqrt(beta2/sum(t))

# Log-likelihooden som den er angivet i opgaven
l <- function(beta) - sum(x)*log(beta) + sum(t)*beta

# Test for hypotese
LR <- 2* (l(2) - l(beta2))
1 - pchisq(LR,1)

# Simulationsstudie
M <- 2000
trueBeta <- 3
n <- 20
betaTilde <- rep(NA,M)
betaCheck <- rep(NA,M)

for (i in 1:M)
{
  t <- (1:n) / n
  x <- rpois(n,trueBeta*t)
  betaTilde[i] <- mean(x/t) 
  betaCheck[i] <- sum(x) / sum(t)
}

hist(betaTilde)
hist(betaCheck)
plot(betaTilde, betaCheck)
c(mean(betaTilde), sd(betaTilde))
c(mean(betaCheck), sd(betaCheck))

sqrt(trueBeta / n^2 * sum(1/t))  # Sand SD for betaTilde
sqrt(trueBeta/sum(t))            # Sand SD for betaCheck

##################

### Opgave 2

Sigma <- matrix(0,4,4)
Sigma[1,1] <- Sigma[3,3] <- 1
Sigma[1,2] <- Sigma[2,1] <- Sigma[3,4] <- Sigma[4,3] <- 3
Sigma[2,2] <- Sigma[4,4] <- 9
Sigma            
det(Sigma)

A <- matrix(c(1,1,1,0,0,0,0,1),2,4)
A
A %*% Sigma %*% t(A)
det(A %*% Sigma %*% t(A))

##################

### Opgave 3

rotteData <- read.table("rottevaekst.txt", header=TRUE)
head(rotteData)
summary(rotteData)

plot(vaekst ~ dosis, data=rotteData)
plot(logvaekst ~ dosis, data=rotteData)

m0 <- lm(vaekst ~ dosisGrp, data=rotteData)
m1 <- lm(logvaekst ~ dosisGrp, data=rotteData)

pdf("resplots.pdf", width=14, height=7)
source("../April16/gfx.R")
par(mfrow=c(1,2))
plot(fitted(m0), rstandard(m0), xlab="Fittede værdier", ylab="Standardiserede residualer", main="Respons: vækst"); 
abline(h=0, lty=2)
plot(fitted(m1), rstandard(m1), xlab="Fittede værdier", ylab="Standardiserede residualer", main="Respons: logvækst"); 
abline(h=0, lty=2)
dev.off()

summary(m1)
confint(m1)

m1a <- lm(logvaekst ~ relevel(dosisGrp, ref="d2"), data=rotteData)
summary(m1a)
confint(m1a)

m2 <- lm(logvaekst ~ dosis, data=rotteData)
summary(m2)
3*confint(m2)

rotteData$dosisKvd <- rotteData$dosis^2
m3 <- lm(logvaekst ~ dosis + dosisKvd, data=rotteData)

C <- matrix(c(0,3,9),1,3)
est <- C %*% coef(m3)
est
se <- sqrt(C %*% vcov(m3) %*% t(C))
se
est + c(-1,1)* qt(0.975,26)* se

library(gmodels)
estimable(m3, C, conf.int=0.95)

anova(m3,m1)

newData <- data.frame(dosis=2.25, dosisKvd=2.25^2)
predict(m3, newData, interval="p")
exp(predict(m3, newData, interval="p"))
