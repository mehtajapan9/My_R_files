#1a
data(golub, package = "multtest")
gol.fac <- factor(golub.cl,levels=0:1, labels= c("ALL","AML"))
grep("CCND3 Cyclin D3",golub.gnames[,2])

myd<- golub[1042,]
myc1<- hclust(dist(myd, method="euclidian"), method="single")
plot(myc1,labels=gol.fac)

mycw <- hclust(dist(myd, method="euclidian"), method="ward.D2")
plot(mycw,labels=gol.fac)
par(mfrow=c(1,2))
plot(myc1,labels=gol.fac,cex=0.4)
plot(mycw,labels=gol.fac,cex=0.4)
myc2 <- cutree(myc1,2)
mycw2 <- cutree(mycw,2)
table(gol.fac, myc2)
table(gol.fac, mycw2)

#1b
mycls <- kmeans(myd, centers=2)
table(gol.fac, mycls$cluster)

#1d
mycls$centers
f1<- mycls$centers
myn <- length(mycls$cluster)
mynboot<-1000
mycb <- matrix(NA,nrow=mynboot, ncol=2)
for (i in 1:mynboot){
  myd2 <- myd[sample(1:myn,replace=TRUE)]
  cls <- kmeans(myd2, centers=f1)
  mycb[i,] <- cls$centers
}
apply(mycb,2,mean)
quantile(mycb[,1],c(0.025,0.975))
quantile(mycb[,2],c(0.025,0.975))

#1e
K <- (1:30)
SSE<- rep(NA,length(K))
for (k in K) {SSE[k] <- kmeans(myd, centers=k, nstart = 10)$tot.withinss}
plot(K, SSE, type='o', xaxt='n'); axis(1,at = K, las= 2)

#2a
data(golub, package = "multtest")
gol.fac <- factor(golub.cl,levels=0:1, labels= c("ALL","AML"))
onco <- agrep("^oncogene",golub.gnames[,2])
anti <- agrep("^antigen",golub.gnames[,2])
oa <- unique(c(onco,anti))
mygf1 <-factor(c(rep("oncogene",length(onco)),rep("antigen",length(anti))))

#2b
library(cluster)
myd3 <- data.frame(golub[oa,])
clmean<- kmeans(myd3, centers=2) # for kmeans
clmed<- pam(myd3 , k=2) # for k-medeoids
onco1<- table(mygf1, clmean$cluster);#oncogenes comparision
anti1<- table(mygf1, clmed$cluster);#antigens comparision
onco1
anti1

#2c
fisher.test(onco1)
fisher.test(anti1)

#2d
mysl<- hclust(dist(myd3, method="euclidian"), method="single")
plot(mysl,labels=mygf1, cex=0.62) # Eucledian, Single
mycl1<- hclust(dist(myd3, method="euclidian"), method="complete")
plot(mycl1,labels=mygf1, cex=0.62) # Eucledian, Complete


#3
install.packages('ISLR')
library(ISLR)
ncidata<-NCI60$data
ncilabs<-NCI60$labs
table(NCI60$labs)

#3a
K<-(1:30)
SSE<-rep(NA,length(K))
for (k in K) {SSE[k]<-kmeans(ncidata,centers = k,nstart = 10)$tot.withinss}
plot(K,SSE,type = 'o',xaxt='n');axis(1,at=K,las=2)

#3b
library(cluster)
jmcls<-pam(as.dist(1-cor(t(ncidata))),k=7)
table(factor(ncilabs),jmcls$cluster)
