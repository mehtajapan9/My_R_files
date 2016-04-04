#problem 1
a <- function (x) 2*exp(-2*(x-1))
a.x<-function (x) a(x)*(1<x & x<4)
integrate(a,lower=1,upper=4)


#Problem 2(a)
b<- function(x) (2^x/factorial(x))*exp(-2)
b(1)

#problem2(b)
c<- function(x) (2^x/factorial(x))*exp(-2)
sum(c(0:3))

#problem 4(a)
dbinom(0,3,0.25)+dbinom(1,3,0.25)+dbinom(2,3,0.25)

#problem4(b)
Y<-c(0:3)
EY<-sum(Y*dbinom(Y,size=3,p=0.25))
EY

#4(c)
Y<-c(0:3)
EY<-sum(Y*dbinom(Y,size=3,p=0.25))
VY<-sum((Y-EY)^2*dbinom(Y,3,0.25))
VY


#5
pchisq(4,3)-pchisq(1,3)

#(b)
m<-(3)
Ex<-m
Ex

#c
vx<-2*m
vx

#d
xe<-rchisq(100000,3)
mean((1<xe)&(xe<4))


#7(a)
pnorm(1.6,mean=1.6,sd=0.4)-pnorm(1,mean=1.6,sd=0.4) 
#(b)
f<-rnorm(500000,1.6,0.4)
mean((1<f)&(f<1.6))
#(c)
dbinom(2,size=5,prob=0.4331928) 

#8
EX <- integrate(function(x) x * df(x, df1=2, df2=5), lower=0, upper=Inf)$value 
print(EX) 
EY <- integrate(function(y) y * df(y, df1=10, df2=5), lower=0, upper=Inf)$value 
print(EY) 
VarX <- integrate(function(x) (x - EX)^2*df(x, df1=2, df2=5), lower=0, upper=Inf)$value 
print(VarX) 
VarY <- integrate(function(y) (y - EY)^2*df(y, df1=10, df2=5), lower=0, upper=Inf)$value 
print(VarY)


#(b)
Fvar <- function(m,n) (2*n^2*(m+n-2))/(m*(n-2)^2*(n-4)) 
print(Fvar(2,5)) 
print(Fvar(10,5))