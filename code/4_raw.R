#Problem 1a
rm(list=ls())
u<-5
var <- 3^2/5
var
sd <- sqrt(var)
sd
mean(2<u & u<5.1)

#Problem 2
rm(list=ls())
n <- 20
p <- 0.7
Mean <- n*p
Mean
Var <- n*p*(1-p) 
Var
sd=sqrt(Var)
sd
1-pnorm(15,mean=14,sd=2.04939/sqrt(100))

#p3
rm(list=ls())
require(mvtnorm)
N = 10000

V<-rep(NA,N)
for (i in 1:N){A<-rmvnorm(50,mean = c (9,10),sigma = matrix(c(3,2,2,5),nrow=2))
B<-apply(A,2,mean)
X<-B[1]
Y<-B[2]
V[i]<-(X+0.5<Y)
}
mean(V)
mean(V)+c(-1,1)*1.96*sqrt(var(V)/10000)


#p4
rm(list=ls())
x1<-rchisq(10000,df=10)
x2<-rgamma(10000,shape = 1,scale =2 )
x3<-rt(10000,df=3)
y<-(sqrt(x1)*(x2)+(4*(x3)^2))
mean(y)


#p5
rm(list=ls())
j <- double(); n <- 10000
an <- sqrt(2*log(n)) - 0.5*(log(log(n))+log(4*pi))*(2*log(n))^(-1/2)
bn <- (2*log(n))^(-1/2)
 
for (h in 1:1000) j[h] <- (max(rnorm(n))-an)/bn
plot(density(j),ylim=c(0,0.5))
f<-function(x){exp(-x)*exp(-exp(-x))}
curve(f,range(density(j)$x),add=TRUE,col = "green")
curve(dnorm,add=TRUE,col = "violet")
