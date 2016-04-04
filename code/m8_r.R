#module 8
#p 1 (a)
library(ALL)
data(ALL)
myALL<-ALL[,ALL$BT%in%c("B","B1","B2","B3","B4")]
j<-exprs(myALL)["109_at",]
anova(lm(j~myALL$BT))


#p1(b)
summary(lm(j~myALL$BT))

#p1(d)
pairwise.t.test(j,myALL$BT,p.adjust.method = 'fdr')

#p1(e)
shapiro.test(residuals(lm(j~myALL$BT)))

library(lmtest)
bptest(lm(j~myALL$BT),studentize = FALSE)

#2a
library(ALL)
data(ALL)
myALL<-ALL[,ALL$BT%in%c("B","B1","B2","B3","B4")]
j<-exprs(myALL)["109_at",]
kruskal.test(j~myALL$BT)
v<-apply(exprs(myALL), 1 , function(x)kruskal.test(x ~ myALL$BT)$p.value)
myfdr<-p.adjust(p=v,method = "fdr")
sum(myfdr<0.05)

#2b
names(sort(v)[1:5])



#3a
library(ALL)
data(ALL)
myALL2<-ALL[,ALL$BT%in%c("B","B1","B2","B3","B4")]
f<-exprs(myALL2)["38555_at",]
u<-myALL2$BT
i<-myALL2$sex
anova(lm(f~u*i))

install.packages("lmtest")
#3b
shapiro.test(residuals(lm(f~u*i)))

library(lmtest)
bptest(lm(f~u*i),studentize = FALSE)


#4a
data(ALL,package="ALL")
library(ALL)
ALLnew <- ALL[,ALL$BT %in% c("B1","B2","B3")]
u<- exprs(ALLnew)["1242_at",] 
group<-ALLnew$BT[,drop=T] 
perm.test<- function(u, group) {
  n<-length(u) 
  gm<-by(u,group,mean) 
  ng<-length(gm)
  T.obs<-(1/(ng-1))*sum((gm-mean(gm))^2)
  n.perm <- 2000 
  T.perm <- rep(NA, n.perm) 
  for(i in 1:n.perm) {
    data.perm <- sample(u, n, replace=F)     
    gm<-by(data.perm,group,mean) 
    ng<-length(gm)
    T.perm[i] <-(1/(ng-1))*sum((gm-mean(gm))^2) 
  }
  mean(T.perm>=T.obs) 
}

#4B
perm.test(u, group) 

