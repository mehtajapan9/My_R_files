#1(a)
source("http://bioconductor.org/biocLite.R")
biocLite("annotate")
library("annotate")
biocLite("ArrayExpress")
library(ArrayExpress)
biocLite("expresso")
library(expresso)
library(ArrayExpress);library(affy)
getAE('E-MEXP-1551', path = 'C:/yeast',  type = "full")

yeast.raw <-  ReadAffy(celfile.path= 'C:/yeast' )
yeast.raw <- ArrayExpress('E-MEXP-1551')
y1<- expresso(yeast.raw,bgcorrect.method="mas",
                      normalize.method="quantiles",
                      pmcorrect.method="pmonly",
                      summary.method="medianpolish")
y2 <- exprs(y1)

#1b
apply(y2[1:5,], 1, mean)

#1c
str(y2)

#2a
annotation(yeast.raw)
biocLite("yeast2.db")

#2b
library(yeast2.db)
g1 <- get("1769308_at", env = yeast2GO)
library(annotate)
g2 <- getOntology(g1 ,"MF")
length(g2)


#2c
biocLite("GO.db")
library(GO.db)
y <- get("1769308_at", env = yeast2GO)
mygo <- getOntology(go1769308_at, "MF")
mygop <- getGOParents(mygo)
p <- sapply(mygop,function(y) y$Parents)
p
length(p)

#2d
mygoc <- getGOChildren(mygo)
mychil <- sapply(mygoc,function(a) a$Children)
mychil
length(unlist(mychil))


#3a
biocLite("genefilter")
library("genefilter")
library("ALL")
data(ALL)
library("limma")
myp <- exprs(ALL)[,(ALL$BT %in% c("B2","B3"))]
myf <- droplevels(ALL$BT[ALL$BT %in% c("B2","B3")])
myf1 <- function(x) (wilcox.test(x ~ myf, exact = F)$p.value < 0.001)
myf2 <- function(x) (t.test(x ~ myf)$p.value < 0.001)
Wil <- genefilter(myp, filterfun(myf1))
Welch <- genefilter(myp, filterfun(myf2))

#3b and c
x <- apply(cbind(Wil,Welch), 2, as.integer)
vc <- vennCounts(x, include="both")
vennDiagram(vc)
a

#3d
annotation(ALL)
biocLite("hgu95av2.db")
library(hgu95av2.db)
goonco <- function(term) {
  GTL <- eapply(GOTERM, function(x) {grep(term, x@Term, value=TRUE)})
  G1 <- sapply(GTL, length)
  names(GTL[G1>0])
}
oncoid<- goonco("oncogene")
oncoid

#3e
p5 <- Wil & Welch
p6 <- myp[p5,]
p7 <- hgu95av2GO2ALLPROBES$"GO:0090402"
print(sum(p7 %in% rownames(p6)))


#4a
library("limma")
library("ALL")
data(ALL)
library("genefilter")

myALL <- ALL[,which(ALL$BT %in% c("B1","B2","B3"))]

#4b
JALL <- factor(myALL$BT)
des <- model.matrix(~ 0 + JALL)
colnames(des) <- c("B1","B2","B3")
fit <- lmFit(myALL, des); fit <- eBayes(fit)
print(topTable(fit, number=5,adjust.method="fdr"), digits=4)
sum(topTable(fit,number=Inf,adjust.method="fdr")$adj.P.Val<0.05)
print( topTable(fit, coef=3, number=5, adjust.method="fdr"), digits=4)

#4c
jmc<- makeContrasts(B1-B2,B2-B3, levels=factor(myALL$BT))
fit2 <- contrasts.fit(fit, jmc);fit2 <- eBayes(fit2)
myfdr<-topTable(fit2,number=Inf,adjust.method="fdr")$adj.P.Val
sum(myfdr<0.01)
print(topTable(fit2, number=5,adjust.method="fdr"), digits=4)
