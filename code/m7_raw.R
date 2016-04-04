#p1(a)
data(golub, package = "multtest")
gol.fac<-factor(golub.cl,levels = 0:1, labels = c("ALL","AML"))

#wilcox two sample 
w<-NULL
for (k in 1:3051){
  w[k] = wilcox.test(golub[k,]~gol.fac,paired = F,alternative = "greater")$p.value}
cox<-w<0.05
sum(cox)
wf<-p.adjust(p=w,method = "fdr")
sum(wf<0.05)


#1(b)
data(golub, package = "multtest")
gol.fac<-factor(golub.cl,levels = 0:1, labels = c("ALL","AML"))
myAML<-order(w,decreasing = FALSE)
golub.gnames[myAML[1:3],2]

#after
data(golub, package = "multtest")
gol.fac<-factor(golub.cl,levels = 0:1, labels = c("ALL","AML"))
myAML2<-order(wf,decreasing = FALSE)
golub.gnames[myAML2[1:3],2]

#large diff
data(golub, package = "multtest")
gol.fac<-factor(golub.cl,levels = 0:1, labels = c("ALL","AML"))
ALLm<-apply (golub[,gol.fac=="ALL"],1,mean)
AMLm<-apply (golub[,gol.fac=="AML"],1,mean)
dmean=ALLm-AMLm
d<-order(dmean,decreasing="TRUE")
golub.gnames[d[1:3],2]


#p2
data(golub, package = "multtest")
gol.fac<-factor(golub.cl,levels = 0:1, labels = c("ALL","AML"))
h<-apply(golub[,gol.fac=="AML"],1,function(x) shapiro.test(x)$p.value)
sum(h>0.05)
hfdr<-p.adjust(p=h,method="fdr")
sum(hfdr>0.05)
sum(hfdr<0.05)



#p3
data(golub, package = "multtest")
gol.fac<-factor(golub.cl,levels = 0:1, labels = c("ALL","AML"))
grep("HOXA9 Homeo box A9",golub.gnames[,2])
grep("CD33",golub.gnames[,2])
wilcox.test(x=golub[1391,gol.fac=="ALL"],y=golub[808,gol.fac=="ALL"],ppaired = T ,alternative = "two.sided")


#p4
library(datasets)
str(UCBAdmissions)
ucba<-c("dpt=A","dpt=B","dpt=C","dpt=D","dpt=E","dpt=F")
for (r in 1:6){
  print(ucba[r])
        myucba<-matrix(c(UCBAdmissions[1,1,r],UCBAdmissions[2,1,r],
UCBAdmissions[1,2,r],UCBAdmissions[2,2,r]),nrow = 2,
dimnames = list("Admit"=c("Admitted","Rejected"),"Gender"=c("Female","Male")))
print(myucba)
print(chisq.test(myucba))
print(fisher.test(myucba))
}


#p5
install.packages('gtools')
library(gtools)
data(golub,package = "multtest")
gol.fac<-factor(golub.cl,levels=0:1,labels= c("ALL","AML"))
g<-grep("CD33",golub.gnames[,2])
g
g2<-golub[g,]
b<-length(g2)
b
s<-var(g2[gol.fac="ALL"])/var(g2[gol.fac=="AML"])

perm = 2000
test.perm=NULL
for(i in 1:perm){
  data.permtest=sample(g2,b,replace = F)
  test.perm[i]=var(data.permtest[gol.fac=="ALL"])/var(data.permtest[gol.fac=="AML"])
}
mean(test.perm<=s)
