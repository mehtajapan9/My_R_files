#P1 (b)
rm(list=ls())
o<-c(1.636,0.374,0.534,3.015,0.932,0.179)
nloglik<-function(exp)-sum(log(dexp(o,rate=exp)))
optim(par=1,nloglik)

#P3(a)
rm(list=ls())
data(golub, package="multtest")
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL","AML"))
M <- grep("Zyxin", golub.gnames[,2])
My_ALL <- golub[2124,gol.fac=="ALL"]
My_ALL
My_AML <- golub[2124,gol.fac=="AML"]
My_AML
Res_All  <- length(My_ALL)
Res_Aml  <- length(My_AML)
Res_All
Res_Aml


n <- 1000
ALL_1 <- rep(NA, n)
ALL_2 <- rep(NA, n)
for (e in 1:n){
  J_ALL <- My_ALL[sample(1:Res_All, replace=TRUE)]
  ALL_1[e] <- mean(J_ALL)
  ALL_2[e] <- var(J_ALL)
}

AML_1 <- rep(NA, n)
AML_2 <- rep(NA, n)
for (f in 1:n){
  
  
  J_AML <-My_AML[sample(1:Res_Aml, replace=TRUE)]
  AML_1[f] <- mean(J_AML)
  AML_2[f] <- var(J_AML)
}

#Bootstrap ci
Mean_all <- quantile(ALL_1,c(0.025, 0.975))
Mean_all
Var_all  <- quantile(ALL_2,c(0.025, 0.975))
Var_all
Mean_aml <- quantile(AML_1,c(0.025, 0.975))
Mean_aml
Var_all  <- quantile(AML_2,c(0.025, 0.975))
Var_all






#3b
rm(list=ls())
#aml
data(golub, package="multtest")
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL","AML"))
x_row <- grep("Zyxin", golub.gnames[,2])
x_AML <- golub[2124,gol.fac=="AML"]
v <- length(x_AML)
mean_AML<- mean(x_AML)+qt(c(0.025,0.975),df=v-1)*sd(x_AML)/sqrt(v)
mean_AML
Var_AML<-(v-1)*sd(x_AML)^2/qchisq(c(0.975,0.025),df=v-1)
Var_AML






#all
data(golub, package="multtest")
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL","AML"))
y_row <- grep("Zyxin", golub.gnames[,2])
y_ALL <- golub[2124,gol.fac=="ALL"]
r<- length(y_ALL)
mean_ALL<- mean(y_ALL)+qt(c(0.025,0.975),df=r-1)*sd(y_ALL)/sqrt(r)
mean_ALL
var_ALL<-(r-1)*sd(y_ALL)^2/qchisq(c(0.975,0.025),df=r-1)
var_ALL






#3(c)
rm(list=ls())
data(golub, package="multtest")
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL","AML"))
zrow <- grep("Zyxin", golub.gnames[,2])
zALL <- golub[2124,gol.fac=="ALL"]
zAML <- golub[2124,gol.fac=="AML"]

gALL <- length(zALL)
gAML <- length(zAML)

p <- 1000

ALL.x1 <- rep(NA, p)
ALL.x2 <- rep(NA, p)
for (i in 1:p){
  dALL <- zALL[sample(1:gALL, replace=TRUE)]
  ALL.x1[i] <- median(dALL)
}

AML.x1 <- rep(NA, p)
AML.x2 <- rep(NA, p)
for (i in 1:p){
  dAML <- zAML[sample(1:gAML, replace=TRUE)]
  AML.x1[i] <- median(dAML)
}

ALLmed<-quantile(ALL.x1,c(0.025,0.975))
ALLmed
AMLmed<-quantile(AML.x1,c(0.025,0.975))
AMLmed







#Problem 4:
#1
nsim<-1000
lambda<-10
mydata<-matrix(rpois(50*nsim,lambda),nrow=nsim)
mylambda<-(apply(mydata,1,mean))
tdis<- qt(.05,49) * sqrt(mylambda/50)
l=mylambda+tdis
H=mylambda-tdis
sum(l<lambda & lambda<H)/1000






#2
nsim <- 1000
lambda<-10
mydata1 <- matrix(rpois(50*nsim,lambda),nrow=nsim)
mylambda1 <- (apply(mydata1,1,mean))
low = 49 *(mylambda1)/qchisq(.95,49)
High = 49 *(mylambda1)/qchisq(.05,49)
sum(low<mylambda1 & mylambda1<High)/1000






# 4b
rm(list=ls())
#lam=0.1 
nsim<-1000
lambda0.1<-0.1
data0.1<-matrix(rpois(50*nsim,lambda0.1),nrow=nsim)
new0.1<-(apply(data0.1,1,mean))
t<- qt(.05,49) * sqrt(new0.1/50)
low=new0.1+t
High=new0.1-t
sum(low<lambda0.1 & lambda0.1<High)/1000






#lambda = 0.1 part 2 
rm(list=ls())
nsim <- 1000
lambda0.1 <- 0.1
data0.1 <- matrix(rpois(50*nsim,lambda0.1),nrow=nsim)
new0.1 <- (apply(data0.1,1,mean))
low = 49 *(new0.1)/qchisq(.95,49)
High = 49 *(new0.1)/qchisq(.05,49)
sum(low<lambda0.1 & lambda0.1<High)/1000






#lambda=1 (part1)
rm(list=ls())
nsim<-1000

lambda1<-1
data1<-matrix(rpois(50*nsim,lambda1),nrow=nsim)
new1<-(apply(data1,1,mean))
t<- qt(.05,49) * sqrt(new1/50)
low=new1+t
High=new1-t
sum(low<lambda1 & lambda1<High)/1000






#lambda=1 & Formula 2
rm(list=ls())
nsim <- 1000
lambda1<- 1
data1 <- matrix(rpois(50*nsim,lambda1),nrow=nsim)
new1 <- (apply(data1,1,mean))
low = 49 *(new1)/qchisq(.95,49)
High = 49 *(new1)/qchisq(.05,49)
sum(low<lambda1 & lambda1<High)/1000






#lambda=10 & Formula 1
rm(list=ls())
nsim<-1000
lambda10<-10
data10<-matrix(rpois(50*nsim,lambda10),nrow=nsim)
new10<-(apply(data10,1,mean))
t10<- qt(.05,49) * sqrt(new10/50)
low=new10+t10
High=new10-t10
sum(low<lambda10 & lambda10<High)/1000






#lambda=10 & Formula 2
rm(list=ls())
nsim <- 1000
lambda10 <- 10
data10 <- matrix(rpois(50*nsim,lambda10),nrow=nsim)
new10 <- (apply(data10,1,mean))
low = 49 *(new10)/qchisq(.95,49)
High = 49 *(new10)/qchisq(.05,49)
sum(low<lambda10 & lambda10<High)/1000

