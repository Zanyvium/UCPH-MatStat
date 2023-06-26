set.seed(314)
n <- 10^4
beta <- 1

LocationCauchyScale1 <- function(n, alpha) {
  rcauchy(n = n, location = alpha, scale = 1)
}

MeanScoreCauchyScale1 <- function(n, alpha) {
  X <- LocationCauchyScale1(n,alpha)
  Score <- 2*(X-alpha)/(1+(X-alpha)^2)
  mean(Score)
}


alphaval <- seq(from = -15, to = 15, by = 0.01)
meanscores <- rep(NA, length(alphaval))

for (i in 1:(length(meanscores))) {
  meanscores[i] <- MeanScoreCauchyScale1(n=n,alpha=alphaval[i])
}

#How does the vectorize function work?

plot(meanscores)

MinMaxInterval <- function(x) {
  c(min(x),max(x))
}
MinMaxInterval(meanscores)

#It seems reasonable that E(S(X,theta)) = 0 <---------------------------------

#how to use the apply functions in construction of our own functions

MeanInformationCauchyScale1 <- function(n,alpha) {
  X <- LocationCauchyScale1(n,alpha)
  Informationfunc <- ((X-alpha)^2-1)/((1+(X-alpha)^2)^2)
  mean(Informationfunc)
}

VarScoreCauchyScale1 <- function(n, alpha) {
  X <- LocationCauchyScale1(n,alpha)
  Score <- 2*(X-alpha)/(1+(X-alpha)^2)
  var(Score)
}

MeanScoreSquareCauchyScale1 <- function(n, alpha) {
  X <- LocationCauchyScale1(n,alpha)
  Score <- 2*(X-alpha)/(1+(X-alpha)^2)
  mean(Score^2)
}


meaninformation <- rep(NA, length(alphaval))

for (i in 1:(length(meaninformation))) {
  meaninformation[i] <- MeanInformationCauchyScale1(n=n,alpha=alphaval[i])
}

meanscoressquare <- rep(NA, length(alphaval))

for (i in 1:(length(meanscoressquare))) {
  meanscoressquare[i] <- MeanScoreSquareCauchyScale1(n=n,alpha=alphaval[i])
}

varscores <- rep(NA, length(alphaval))

for (i in 1:(length(varscores))) {
  varscores[i] <- VarScoreCauchyScale1(n=n,alpha=alphaval[i])
}

plot(meaninformation)
plot(varscores)
plot(meanscoressquare)

plot(meaninformation - varscores)
plot(meaninformation - meanscoressquare)

#It is a necessary condition that both of these were to be approx. 0 for Barlett's identities to hold, and as they
#seem squed from 0, we conclude that the identities don't seem to apply, thus, as we know the family of Cauchy-measures to be smooth, and dominated
# by the lebesgue-measure, we get that the family is not locally stable by simulation.






