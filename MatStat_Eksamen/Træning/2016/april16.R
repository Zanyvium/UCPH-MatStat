setwd("~/Teaching/Courses/Stat1/Eksamen/April16")

##########

### Opgave 1

t <- c(0.0, 0.6, 1.2, 1.8, 2.4, 3.0)
x <- c(3.0,  1.6,  2.8,  9.2, 17.5, 31.4)

plot(t,x)
plot(t, log(x))

n <- 6
s2 <- 1.606481
b <- 1.15625068

### 1.1

sum((x-exp(b*t))*exp(b*t)*t)               # Første koordinat i S(b,s2)
n/2/s2 - sum((x-exp(b*t))^2) / 2 / s2^2    # Anden koordinat i S(b,s2)

### 1.3

s22 <- 1.606
b2 <- 1.156

i11 <- sum(t^2*exp(2*b2*t))/s22
i11
se <- sqrt(1/i11)
se

b2 + c(-1,1)*1.96*se  ## Konfidensinterval

### 1.4

z <- log(x)
summary(lm(z ~ t-1))


# Alternativt:
sum(log(x)*t) / sum(t^2)
A <- matrix(t,6,1)
gamma <- solve(t(A)%*%A) * t(A) %*% z
tau2 <- sum((z - A %*% gamma)^2) / 5
tau <- sqrt(tau2)
se2 <- tau * sqrt(solve(t(A)%*%A))


###############

### Opgave 2

Sigma <- matrix(c(1,2,1,2,9,1,1,1,4),3,3)
Sigma
# det(Sigma)

### 2.1

D <- matrix(c(1,-1,1,1,2,-3),2,3, byrow=TRUE)
D
D %*% Sigma %*% t(D)
det(D %*% Sigma %*% t(D))

### 2.2 (check)

c1 <- -5
c3 <- 2
B <- matrix(c(c1,1,0,0,c3,1),2,3)
B
B %*% Sigma %*% t(B)

c1 <- 5
c3 <- -2
B <- matrix(c(c1,1,0,0,c3,1),2,3)
B
B %*% Sigma %*% t(B)

##############

### Opgave 3

hydrolyse <- read.table("hydrolyse.txt", header=TRUE)

plot(log(serin)~tid, data=hydrolyse, pch=as.numeric(foder), ylab="x = log(serin)", main="Hydrolyse", cex=1.5)

modelA <- lm(log(serin) ~ foder*tid, data=hydrolyse)
modelB <- lm(log(serin) ~ foder+tid, data=hydrolyse)
modelC <- lm(log(serin) ~ foder, data=hydrolyse)
modelD <- lm(log(serin) ~ tid, data=hydrolyse)
modelE <- lm(log(serin) ~ 1, data=hydrolyse)



### 3.1

plot(fitted(modelA), rstandard(modelA), main="Residualplot")
abline(h=0, lty=2)
qqnorm(rstandard(modelA), main="QQ-plot for stand. residualer")
abline(0,1, lty=2)

### 3.2

anova(modelB, modelA)

### 3.3

summary(modelB)
confint(modelB)

summary(lm(log(serin) ~ relevel(foder, ref="majs") + tid, data=hydrolyse))
confint(lm(log(serin) ~ relevel(foder, ref="majs") + tid, data=hydrolyse))

modelB2 <- lm(log(serin) ~ foder+tid-1, data=hydrolyse)
summary(modelB2)

### 3.4

30*coef(modelB)
30*confint(modelB)

newData <- data.frame(foder="soja", tid=40)
predict(modelB, newData, interval="p")

### 3.5

psi <- matrix(c(rep(.2,5),50), 6)
psi
t(psi) %*% coef(modelB2) 
t(psi) %*% vcov(modelB2) %*% psi
sqrt(t(psi) %*% vcov(modelB2) %*% psi)
t(psi) %*% coef(modelB2)  + c(-1,1)*qt(0.975, df=44)* sqrt(t(psi) %*% vcov(modelB2) %*% psi)

##############

### Opgave 4

x <- c(10,2,3,0,0,1,1,0,2,7,2,5)
n <- 12
sum(x)

### 4.2

thetaHat <- n / (n+sum(x))
thetaHat

### 4.3

theta0 <- 0.4
logL <- function(theta) -n*log(theta) - sum(x) * log(1-theta) 

LR <- 2*logL(theta0) - 2*logL(thetaHat) 
LR
1-pchisq(LR,1)

n <- 20
theta0 <- 0.4
trueTheta <- 0.4
M <- 5000
LR <- rep(NA,M)

for (i in 1:M)
{
  simx <- rgeom(n,trueTheta)
  logL <- function(theta) -n*log(theta) - sum(simx) * log(1-theta)
  thetaHat <- n / (n+sum(simx))
  LR[i] <- 2*logL(theta0) - 2*logL(thetaHat) 
}
mean(LR>3.84)

