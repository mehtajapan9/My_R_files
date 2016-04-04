#1a
library("ALL")
data(ALL)
myd <- exprs(ALL)
ALL.fac <- factor(ALL$BT %in% c("B","B1","B2","B3","B4"), labels=c("1","2"))

#1b
myg1<-myd[1,]
myg2<-myd[2,]
myg3<-myd[3,]
par(mfrow=c(1,3))
hist(myg1,main = "gene 1")
hist(myg2,main = "gene 2")
hist(myg3,main = "gene 3")

#1c
myg1<-myd[1,]
myg2<-myd[2,]
myg3<-myd[3,]
myg4<-myd[4,]
myg5<-myd[5,]
pairs(cbind(myg1,myg2,myg3,myg4,myg5))

#1d
install.packages("scatterplot3d")
require(scatterplot3d)
par(mfrow=c(1,1))
genes<-rbind(exprs(ALL[c("39317_at","32649_at","481_at")]))
scatterplot3d(t(genes),color = ALL.fac)

#1e
myc<- kmeans(t(genes),centers=2,nstart=10)
table(ALL.fac,myc$cluster)
myc2<- kmeans(t(genes),centers=3,nstart=10)
table(ALL.fac,myc2$cluster)

#1f
myPCA<- prcomp(myd, scale=TRUE)
summary(myPCA)

#1g
par(mfrow=c(1,1))
biplot(myPCA,cex=0.5)

#1h
g1<- order(myPCA$x[,2], decreasing=T)
dimnames(myd)[[1]][[g1[1]]];
dimnames(myd)[[1]][[g1[2]]];
dimnames(myd)[[1]][[g1[3]]]
dimnames(myd)[[1]][[g1[12623]]];
dimnames(myd)[[1]][[g1[12624]]];
dimnames(myd)[[1]][[g1[12625]]]

#1i
annotation(ALL)
library(hgu95av2.db)
chon<-as.list(hgu95av2CHR)
ghon<-as.list(hgu95av2GENENAME)
chon[g1[1]]
ghon[g1[1]] 
chon[g1[12625]]
ghon[g1[12625]] 

#2a
id<-iris[1:4]
mymd<-mean(id[,1])
mysd<-sd(id[,1])
mysl<-NULL
for (i in 1:150){mysl[i] <- (id[i,1]-mymd)/mysd}
mymd<- mean(id[,2])
mysd<- sd(id[,2])
mysw<- NULL
for (i in 1:150){mysw[i] <- (id[i,2]-mymd)/mysd}
mymd<- mean(id[,3])
mysd<- sd(id[,3])
mypl<- NULL
for (i in 1:150){mypl[i] <- (id[i,3]-mymd)/mysd}
mymd<- mean(id[,4])
mysd<- sd(id[,4])
mypw<- NULL
for (i in 1:150){mypw[i] <- (id[i,4]-mymd)/mysd}
sca<- cbind(mysl, mysw, mypl, mypw)
myx<- data.frame(mysl, mysw, mypl, mypw)

#2b
crs<- cor(sca)
crsu<- cor(id)
crs
crsu
all.equal(crs,crsu)

#2c
diseu <- dist(t(sca), method="euclidean")
dissqeu<-diseu^2
dissqeu
d.cr<-as.dist(1-cor(crs))
d.cr
f.pr<-dissqeu/d.cr
f.pr

#2d
unPCA <- prcomp(id, scale=FALSE)
unPCA
sPCA <- prcomp(sca, scale=FALSE)
sPCA

#2e
summary(unPCA)
summary(sPCA)

#2f
dPCA <- sca
pca <- ncol(dPCA)
n <- nrow(dPCA)
nboot<-1000
dev <- array(dim=c(nboot,pca))
for (i in 1:nboot) {
  dat.star <- dPCA[sample(1:n,replace=TRUE),]
  dev[i,] <- prcomp(dat.star, scale=FALSE)$sdev
  pca2<-dev[,2]
  prop<-pca2^2/sum(dev[i,]^2)
}
quantile(prop,c(0.05,0.95))
