#poblacio
mu<-95.3 #mitjana
sigma<-5.7 #desviació tipica

curve(dnorm(x,mean=mu,sd=sigma),xlim=c(70,120))

set.seed(123)
rnorm(4,mu,sigma) #mostra aleatoria de tamany 4

pnorm(90,mu,sigma) #prob que valgui 90

Y<- function(i)(sum(rnorm(4,mu,sigma)))
Y1000000<- sapply(1:1000000,Y)
hist(Y100000)
mean(Y100000) #valor esperat de la suma mostral

#en teoria, la mitja de la suma mostral de n=4 és...
4*mu
#variança de la suma mostral és...
4*sigma^2

Y<- function(i)(sum(rnorm(100,mu,sigma))) #sumar 100 mostres
Y100000<- sapply(1:100000,Y)
hist(Y100000)
var(Y100000)

#en teoria
100*sigma^2

#c)
1-pnorm(103,mu,sigma)

Y<- function(i)(rnorm(1,mu,sigma))
Y100000<- sapply(1:100000,Y)
hist(Y100000)
mean(Y100000>103)

#d)
Xbar<- function(i)(mean(rnorm(4,mu,sigma))) #mitjana de 4 mostres
Xbar100000<- sapply(1:100000,Xbar)
hist(Xbar100000)
mean(Xbar100000<98)

#e)
Ssq<- function(i)(var(rnorm(100,mu,sigma))) #variança de 100 mostres
Ssq100000<- sapply(1:100000,Ssq)
hist(Ssq100000)
mean(Ssq100000>32)
1-pchisq((100-1)*32/sigma^2,100-1)

hist(Ssq100000*(100-1)/sigma^2)
curve(dchisq(x,100-1),add=TRUE,lwd=2,col="red")
