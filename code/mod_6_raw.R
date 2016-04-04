#1(a)
data(golub,package="multtest")
gol.fac <- factor(golub.cl, levels=0:1,labels=c("ALL","AML"))
mainH4<- grep("H4/j", golub.gnames [,2])
mainAPS<- grep("APS", golub.gnames [,2])
t.test(golub[mainH4,gol.fac=="ALL"],mu= -0.9, alternative = "greater")




#1(b)
length(golub[2972,gol.fac=="ALL"])
length(golub[2972,gol.fac=="AML"])
t.test(golub[2972,gol.fac=="ALL"],golub[2972,gol.fac=="AML"])




#1(c)
t.test(golub[2972,gol.fac=="ALL" ],golub[2989,gol.fac=="ALL"],alternative="less",paired=T)




#1(d)
H4ALL<-length(golub[2972, gol.fac=="ALL"])
APSALL<-length(golub[2989, gol.fac=="ALL"])
H4AML<-length(golub[2972,gol.fac=="AML"])
APSAML<-length(golub[2989, gol.fac=="AML"])
PAPSALL<-golub[2989,gol.fac=="ALL"]
PH4ALL<-golub[2972, gol.fac=="ALL"]
plow<- sum(PH4ALL<PAPSALL)
binom.test(x=plow,n=H4ALL,p=0.5,alternative="greater")




#1(e)
H41ALL<- sum(golub[2972, gol.fac=="ALL"] > -0.6)
H42ALL<-length(golub[2972, gol.fac=="ALL"])
binom.test(x=H41ALL, n=H42ALL, p=0.5, alternative="less")




#1(f)
myH4ALL<-sum(golub[mainH4,gol.fac=="ALL"]>-0.6)
myH4AML<-sum(golub[mainH4,gol.fac=="AML"]>-0.6)
myALLH4<- length(golub[mainH4, gol.fac=="ALL"])
myAMLH4 <- length(golub[mainH4, gol.fac=="AML"])
prop.test(x=c(myH4ALL,myH4AML), n=c(myALLH4,myAMLH4), alternative="two.sided")




#2(b)
pbinom(89,size=2000,prob=0.05)




#3
g<-matrix(rnorm(10000*20, mean=3, sd=4), ncol=20)
h<-function(x) (mean(x)-3)/sd(x)*sqrt(length(x))
l<- apply(g,1,h)
m<-mean(l<qt(0.3,df=19))
v<- mean(l<qt(0.4,df=19))
vm<- (v-m)
vm
vm+c(-1,0,1)*qnorm(0.975)*sqrt(vm*(1-vm)/10000)





#4(a)
data(golub,package="multtest")
gol.fac <- factor(golub.cl, levels=0:1,labels=c("ALL","AML"))
p1 <- apply(golub, 1, function(x) t.test(x ~ gol.fac)$p.value)
bonferroni<-p.adjust(p=p1,method="bonferroni")
fdr<-p.adjust(p=p1,method="fdr")
sum(pt<0.05)
sum(bonferroni<0.05)
sum(fdr<0.05)





#4(b)
data(golub,package="multtest")
gol.fac <- factor(golub.cl, levels=0:1,labels=c("ALL","AML"))
pv <- apply(golub, 1, function(x) t.test(x ~ gol.fac)$p.value)
pv <- apply(golub, 1, function(x) t.test(x ~ gol.fac)$p.value)
pfdr<-p.adjust(p=pv,method="fdr")
oAML<-order(pfdr, decreasing=FALSE)
golub.gnames[oAML[1:3],2]

#P5
#Wald
wci <- function(X,n,confd=0.05){
z <- qnorm(1-confd/2)
p <- X/n
    return (c(p,(p + c(-1,1) * z * sqrt((p*(1-p))/n))))
  }
#Wilson
wilci <- function(X,n,confd=0.05){
  z <- qnorm(1-confd/2)
  p <- X/n
  return (c(p,((1/(1+z^2/n)) * (p + (z^2/(2*n)) + c(-1,1)*z*sqrt((p*(1-p))/n+z^2/(4*n^2))))))
}
#Agresti
agrestici <- function(X,n,confd=0.05){
  z <- qnorm(1-confd/2)
  N <- n + z^2
  P <- (X+z^2/2)/N
  return (c(P,(P + c(-1,1)*z*sqrt((P*(1-P))/N))))
}

#5(b)
myn <- rbinom(n=1,size=40,p=0.2)
mynwald <- wci(myn,40)
mynwilson <- wilci(myn,40)
mynagresti <- agrestici(myn,40)
s <- rbinom(n=10000,size=40,p=0.2)
Wald <- NULL
Wilson <- NULL
Ag <- NULL
for(i in s){
Wald <- rbind(Wald,wci(i,40))
Wilson <- rbind(Wilson,wilci(i,40))
Ag <- rbind(Ag,agrestici(i,40))
}
#ci
Waldm <- mean(0.2 > Wald[,2] & 0.2 < Wald[,3])
Wilsonm <- mean(0.2 > Wilson[,2] & 0.2 < Wilson[,3])
Agm <- mean(0.2 > Ag[,2] & 0.2 < Ag[,3])
print("Estimated coverage")
print(paste("Wald ci",Waldm,sep=" = "))
print(paste("Wilson ci",Wilsonm,sep=" = "))
print(paste("Agresti ci",Agm,sep=" = "))


