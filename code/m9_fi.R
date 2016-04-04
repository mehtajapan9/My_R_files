#1
data(golub,package = "multtest")
grep("GRO2 GRO2",golub.gnames[,2])
grep("GRO3 GRO3",golub.gnames[,2])
GRO2<-golub[2714,]
GRO3<-golub[2715,]

#1a
cor<-cor.test(GRO2,GRO3)
cor

#1b
ci<-cor.test(GRO2,GRO3,conf.level =.90)
ci

#1c
nboot<-2000
corr.boot<-matrix(0,nrow=nboot,ncol=1)
data<-cbind(GRO2,GRO3)
for(i in 1:nboot) {
  dat.star<-data[sample(1:nrow(data),replace=TRUE),]
  corr.boot[i,]<-cor(dat.star[,1],dat.star[,2])
 }
quantile(corr.boot[,1],c(0.05,0.95))

#1d
nboot<-2000
corr.boot<-matrix(0,nrow=nboot,ncol=1)
data<-cbind(GRO2,GRO3)
for(i in 1:nboot) {
  dat.star<-data[sample(1:nrow(data),replace=TRUE),]
  corr.boot[i,]<-cor(dat.star[,1],dat.star[,2])
}
quantile(corr.boot[,1],c(0.025))


#2a
data(golub,package = "multtest")
grep("Zyxin",golub.gnames[,2])
Zyxin<-golub[2124,]
mydata<-apply(golub,1,function(x) cor.test(x,Zyxin)$estimate)
mydata2<-sum(mydata< -0.5)
mydata2

#2b
data(golub,package = "multtest")
grep("Zyxin",golub.gnames[,2])
Zyxin<-golub[2124,]
mydata<-apply(golub,1,function(x) cor.test(x,Zyxin)$estimate)
myorder<-order(mydata,decreasing = FALSE)
mygenes<-golub.gnames[myorder[1:5],2]
mygenes

#2c
myt<-apply(golub,1,function(x) cor.test(x,Zyxin,alternative = "less")$p.value)
tres<-sum(myt<0.05)
tres

fdr<-p.adjust(p=myt,method = "fdr")
g<-sum(fdr<0.05)
g


#3a
data(golub,package = "multtest")
GRO2<-golub[2714,]
GRO3<-golub[2715,]
jl<-lm(GRO3 ~ GRO2)
summary(jl)

#3b
data(golub,package = "multtest")
GRO2<-golub[2714,]
GRO3<-golub[2715,]
jl<-lm(GRO3 ~ GRO2)
confint(jl,level = 0.95)

#3c
data(golub,package = "multtest")
GRO2<-golub[2714,]
GRO3<-golub[2715,]
jl<-lm(GRO3 ~ GRO2)
jd<-data.frame(GRO2 = 0)
predict(jl,jd,interval = "prediction",level = 0.80)

#3d
data(golub,package = "multtest")
GRO2<-golub[2714,]
GRO3<-golub[2715,]
jl<-lm(GRO3 ~ GRO2)
plot(jl,which = 2)
shapiro.test(resid(jl))
plot(jl,which = 1)


#4a and b
data(stackloss)
str(stackloss)
JM<-as.data.frame(stackloss[,c('Air.Flow','Water.Temp','Acid.Conc.','stack.loss')])
MN<-lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=JM)
summary(MN)

#4c
JMD2<-data.frame(Air.Flow=60,Water.Temp=20,Acid.Conc.=90)
predict(MN,JMD2,interval="confidence",level=0.90)
predict(MN,JMD2,interval="prediction",level=0.90)

