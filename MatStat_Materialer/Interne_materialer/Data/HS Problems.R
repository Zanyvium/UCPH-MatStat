#HS1 ----
#1
library(MASS)
cats
head(cats)
dim(cats) #144 rows (not including header), 3 columns
summary(cats)

#2
cats<-cats
cats<-transform(cats, pct=cats$Hwt/1000*1/(cats$Bwt)*100)
#Note that we are to do two rescalings;
#first, as bwt is in kg, and hwt is in g, we need to take the 
#relation heartweight/bodyweight in the same units, in this case
#we recalculate heartweight in kg, by deviding Hwt by 1000.

#-> Also, we have to multiply 100 onto the result of the quotient, 
# as the quotient describes the relation directly, and we want
#a result in procent.
cats





#3
femaleData <- subset(cats, cats$Sex=="F") #Remember that a test is 
#conduced with '==', and NOT '='.
femaleData

maleData <- subset(cats, cats$Sex=="M") 
maleData #Note that the location of the variables in the original
#dataset follow along in the new subsetted dataset, in this case
#manifesting itself by the first entry of maleData occupying
#"row 48"

maleData[1,]

#HS2 ----
#1
#plot(cats$Bwt, cats$Hwt)
#Vi opskriver D6.1 fra introduktion til statistik

with(maleData, plot(Bwt, Hwt))
linreg <- lm(Hwt~Bwt, data=maleData)
# Vi har Bwt til at være den uafhængige,
# og Hwt til at være den afhængige variabel,
# ergo vi laver regression således at vi forsøger at lave 
# Hwt = alpha + beta Bwt
#ergo: Hwt=0.004313*Bwt-0.001184

#2.
fit <- fitted(linreg)
rst<-rstandard(linreg)

plot(fit, rst)
abline(0,0)

# Bemærk her, at vi har plottet standardresidualerne.
# Vi ser (fra side 133 i IS) at vi af plottet af
# standard residualerne mod de ~~forventede/prædikterede~~
# værdier idet punkterne lader til at være spredt
# ret ligelidt på begge sidder af 0, og at spreder
# sig nogenlunde om 0 som ville forventes af stdnormal
# fordeling.
# vi ser f.eks. at;
mean(rst) #0.00045
var(rst) # 1.015
# - Hvilket i hvert fald passer til forventning om 
# middelværdi 0 og varians 1. - spørgsmålet er nu om
# normalfordelingsantagelsen passer...


qqnorm(rst)
qqline(rst)
abline(a = 0, b = 1) #?????

# Af side 68 ses at, idet vi forventer at stdresidualerne er N(0,1)
# fordelt, skal fraktilplottet ligge pænt om linjen med hældning = spredning, 
#skæring = middelværdi

#3:
summary(linreg)
linreg$coefficients[2]
alpha <- linreg$coefficients[[1]]
beta <- linreg$coefficients[[2]]
#vi ser at alpha = -1.1841
#beta = 4.3127.
#------------|



#------------|

#Fordi en lineær funktion er lineær, vil vi se
# at for \Delta Bwt=0.5kg vil 
# \Delta Hwt = beta*\Delta Bwt = 0.5*4.3127 = 2.15635 g.


#4.
#Fra sætning 6.9 fås at for at få et 1-alpha^* konfidensinterval for beta
# skal vi regne på størrelsen
#beta^+-t_{n-2,1-alpha^*/2}SE(beta)
#Bemærk at vi kan finde SE(beta) i R ved "std error for Bwt" 
#(0.3399) i dette tilfælde.
#------------|
# summary(linreg)$coefficients["Bwt","Std. Error"]
# vcov(linreg)
# diag(vcov(linreg))
# sqrt(diag(vcov(linreg)))
# sqrt(diag(vcov(linreg)))[2]
seb<-sqrt(diag(vcov(linreg)))[[2]] #~0.3399
#Note also that we might extract se(alpha^) with sqrt(diag(vcov(linreg)))[[1]]
#------------|
n<-nrow(maleData)
n #97
?qt
#note alpha*=0.05 so alpha*/2=0.025 so 1-alpha*=0.975
lKI<-beta-qt(0.975, n-2)*seb #3.637904
uKI<-beta+qt(0.975,n-2)*seb #4.987454

#So we get the 95% confidence interval (lKI,uKI) =~ (3.638, 4.987)


#5.
confint(linreg)
?confint
#We try to extract the interval;
confint(linreg)[2,]
as.numeric(confint(linreg)[2,])
confint(linreg)[2,1] #lKI
confint(linreg)[2,2] #uKI

#6.
#Se hæfte. 


#HS3 ----
# Se R-Markdown

mean(maleData$Bwt) #2.9
mean(maleData$Hwt) #11.32268
mean(maleData$pct) #0.3894547

mean(femaleData$Bwt) #2.359574
mean(femaleData$Hwt) #9.202128
mean(femaleData$pct) #0.3915119

#-

var(maleData$Bwt) #0.2185417
var(maleData$Hwt) #6.46323
var(maleData$pct) #0.002858461

var(femaleData$Bwt) #0.07506938
var(femaleData$Hwt) #1.843256
var(femaleData$pct) #0.002630804


#---

#3.
#Vi kan jo fremhæve specifikt at;
mean(maleData$pct) #0.3894547
mean(femaleData$pct) #0.3915119

var(maleData$pct) #0.002858461
var(femaleData$pct) #0.002630804

#Vi ser at der nogenlunde varianshomogenitet - Helt præcist
# er det ud fra data ikke umiddelbart muligt at afkræfte
#en antagelse om at der skulle forekomme varians homogenitet.

# Vi skal per den statistiske model (og opgaven) tjekke betingelserne 
# for pct variablerne differentieret i forhold til køn.

#Så;
NHistDensityDraw<-function(varname) { #Generalize to other distributions
  
  #Draws normaldistribution density on top of a histogram.
  
  seqT<-seq(min(varname),max(varname), by = 1/(5*10^4*(max(varname)-min(varname)))) #T?ller fra min til max af Tid
  
  f1T<-dnorm(seqT,mean=mean(varname),sd=sd(varname)) #opretter t?thed
  maxf1T <- max(f1T)
  hist(varname, prob=1, ylim=c(0,maxf1T))
  
  lines(seqT,f1T) #Tegner t?thed #hvorfor virker dette ikke.
}
hist(femaleData$pct, prob=T)

NHistDensityDraw(femaleData$pct)
#Det kunne godt være

qqnorm(femaleData$pct)
abline(mean(femaleData$pct), sd(femaleData$pct))


hist(maleData$pct, prob=T)


NHistDensityDraw(maleData$pct)
#Det kunne godt være

qqnorm(maleData$pct)
abline(mean(maleData$pct), sd(maleData$pct))

# Ud fra QQ-plotene lader det ikke umiddelbart til at vi kan afkræfte modellen.
# HVORDAN MED UAFHÆNGIGHEDSANTAGELSEN????!!!!!! - Hankattehjerter og hunkattehjerter
# kan da sagtens være afhængige/korrelerede på en eller anden måde - Kap 4 vs. Kap 5

#4. 
mean(femaleData$pct)
mean(maleData$pct)

forvforskelfm<-mean(femaleData$pct)-mean(maleData$pct)
forvforskelfm
?t.test

as.numeric(t.test(femaleData$pct, maleData$pct, paired = F, var.equal = T)[[4]])

# Bemærk at vi har paired = F, idet vi ikke måler "samme population to forskellige tidspunkter"
# og vi har også klargjort vores antagelse om varianshomogenitet - TEORI FOR HVAD t.test gør i begge tilfælde, tak!
# 



#Nej, vi ser at vi ved t.testen at 0 ligger pænt meget i midten af 95% konfidensintervallet
#så vi kan umiddelbart sige, at der ikke er belæg for at procentdelen af hjertevægt i forhold til kropsvægt
#er forskellige mellem hun- og han- katte.


#5.
#(-0.01648249, 0.02059684)


fit <- lm(pct ~ Sex, data=cats) #FORS!

qweqwe <- lm(pct ~ Sex-1, data = cats)
confint(qweqwe)
summary(fit)
confint(fit) #Fors!
str(fit)
summary(fit)

#.....



#6. 


#HS4 ----



#HS5 ----


#HS6 ----





#HS7 ----






#HS8 ----







#HS9 ----







#HS10 ----







#HS11 ----











































































































































































