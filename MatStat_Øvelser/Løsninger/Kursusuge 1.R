###'''''''''''''''''''''''''''''''''''''''''''''''###
###' Opgaver uge 1                               '###
###' Jens Baalkilde Andersen                     '###
###'''''''''''''''''''''''''''''''''''''''''''''''###



#' Div
setwd("~/Documents/Uni/MatStat 2021/R")

#'''''''''''''''''''''''''''''''''''''''''''''''#
#' HS.1                                        '#
#'''''''''''''''''''''''''''''''''''''''''''''''#

#' 1. Indlæs data
library(MASS)
cats
head(cats)
dim(cats)
summary(cats)

#' 2. Ny variabel pct
cats <- transform(cats, pct = Hwt/(1000 * Bwt))
head(cats)

#' 3. To nye datasæt
maleData <- subset(x = cats, subset =  Sex == "M", select = c(Bwt, Hwt, pct) )
head(maleData)

femaleData <- subset(x = cats, subset =  Sex == "F", select = c(Bwt, Hwt, pct) )
head(femaleData)




#'''''''''''''''''''''''''''''''''''''''''''''''#
#' HS.2                                        '#
#'''''''''''''''''''''''''''''''''''''''''''''''#

#' 1. Scatterplot + lineær regressionsmodel
with(maleData, plot(Bwt, Hwt))
linreg <- lm(Hwt~Bwt, data=maleData)
abline(coef(linreg))

##' Opskriv model:
##' Y_i = alpha + beta * X_i + e_i
##' Y_i er Hwt for kat i
##' alpha, beta er intercept og hældning
##' X_i er Bwt for kat i
##' e_i ~ N(0, sigma^2)


#' 2. Modelkontrol
fit <- fitted(linreg)
res <- rstandard(linreg)

plot(fit,res)
abline(h=0, lty=2)

##' Ud fra residualerne kan vi se, om der er et mønster i variansen af
##' fejlleddene e_i fra modellen. 
##' Vi kan ikke se, om normalfordelingsantagelsen er opfyldt
##' Det ser meget pænt ud.

qqnorm(res)
qqline(res)

x <- rnorm(97)

qqnorm(x)
qqline(x)

##' Her kan vi se om normalfordelingsantagelsen er opfyldt.
##' Der er et lille hak på midten, og enderne afviger mere fra linjen
##' end resten. Generelt ser det dog fint ud.

#' 3. Estimater for samtlige parametre i modellen
summary(linreg)

##' alpha: -1.1841
##' beta: 4.3127
##' sigma: 1.557

##' Forventet forskel for to katte med forskel i Bwt på 0.5
0.5 * coef(linreg)["Bwt"] # = 2.156339

#' 4. 95% konfidensinterval fra formel

##' t-fraktil
t <- qt(p = 0.975, df = 95)

##' Konf.interval
4.3127 + c(-1,1) * t * 0.3399
coef(linreg)["Bwt"] + c(-1,1) * t * 0.3399

#' 5. Brug "confint"
confint(linreg)

#' 6. Finde passende designmatrix

##' På tavlen.
DesignMatrix <- matrix(c(rep(1,97),maleData$Bwt),ncol=2)
model.matrix(linreg)



#'''''''''''''''''''''''''''''''''''''''''''''''#
#' HS.6                                        '#
#'''''''''''''''''''''''''''''''''''''''''''''''#

#' X_1,...,X_10 ~ N(0,1) iid.
#' X_bar = 1/10 *(X_1 + ... + x_10)

#' 1. Angiv fordeling af X_bar

##' Eksempel 20.10 i Measure theory
##' X_bar ~ N(0, 1/10 * 1)

#' 2. Eftervis at fordelingsresultatet er sandt ved simulering.

set.seed(2020)

##' Initialisering
gns <- rep(NA,5000)

##' Selve simulationerne
for (i in 1:5000){
  x <- rnorm(n = 10, mean = 0, sd = 1)    # Simuler 10 N(0,1)-udfald
    gns[i] <- mean(x)                     # Beregn gennemsnittet af x-værdierne
  }


##' Undersøgelse af de simulerede værdier
hist(gns, prob=TRUE,breaks=50)                       # Normeret histogram
f <- function(x) dnorm(x, mean=0, sd=sqrt(1/10))  # Den relevante tæthed
plot(f,-1,1,add=TRUE)

qqnorm(gns)
abline(0,sqrt(1/10))
qqline(gns, col = "red")


#' 3. Lad X_tilde betegne medianen af X_1,...,X_10. Undersøg fordeling ved simulation.

##' Initialisering
medians <- rep(NA, 5000)

##' Simulation
for (i in (1:5000)){
  x <- rnorm(n = 10,mean = 0,sd = 1)
  medians[i] <- median(x)
  }

##' Visualisering
hist(medians, probability = T)
mean(medians) # 0.0086
var(medians)  # 0.1379

plot(gns,medians)
