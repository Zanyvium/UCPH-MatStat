#NRH 1.1, sub3.----

set.seed(314)
N<-10^3
alpha<-5
sigma<-3
antalsaet<-100


X<-rnorm(N,alpha,sd=sigma)
alphahat<-1/N*sum(X)


Q<-matrix(NA, nrow = N, ncol = antalsaet)

for (i in 1:antalsaet) {
  Q[,i]<-rnorm(N,alpha,sd=sigma)
}
alphahatstor<-rep(NA,antalsaet)
for (i in 1:antalsaet) {
  alphahatstor[i]<-1/N*sum(Q[,i])
}
alphahatstor
hist(alphahatstor, prob = 1)

NHistDensityDraw<-function(varname) { #Generalize to other distributions
  
  #Draws normaldistribution density on top of a histogram.
  hist(varname, prob=1)
  seqT<-seq(min(varname),max(varname), by = 1/(5*10^4*(max(varname)-min(varname)))) #T?ller fra min til max af Tid
  
  f1T<-dnorm(seqT,mean=mean(varname),sd=sd(varname)) #opretter t?thed
  lines(seqT,f1T) #Tegner t?thed #hvorfor virker dette ikke.
}

NHistDensityDraw(alphahatstor) #Tegner empirisk normaltæthed henover data (altså alphahatstor)

#Variansen i fordelingen af alphahat er et udtryk for hvor usikre vi er 
#idet vi estimerer alphahat, og idet 1/N->0 for N->inf er det at
#alphahat er fordelt som N(alpha, sigma^2/N) sige noget om, at vores
#usikkerhed om estimatet alpha er dalende jo mere data vi får.


QT<-rnorm(10,0,1)
median(QT)
QT
QT[order(QT)]
QT5<-QT[order(QT)][5]
QT6<-QT[order(QT)][6]
(QT5+QT6)/2


















