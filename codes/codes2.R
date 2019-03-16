library(readr)
data <- read_csv("Wholesale customers data.csv", 
                 col_types = cols(X10 = col_skip(), X11 = col_skip(), 
                                  X12 = col_skip(), X9 = col_skip()))

x=matrix(c(data$Channel,data$Region,data$Fresh,data$Milk,data$Grocery,data$Frozen,data$Detergents_Paper,data$Delicassen), ncol=8,byrow=FALSE)
dd=as.dist(1-cor(t(x)))
hc=hclust(dd, method ="average")
plot(hc,hang = -1,main="Average Linkage with Correlation-Based Distance ", xlab="", sub ="")

hc=hclust(dd, method ="single")
plot(hc,hang = -1,main="Single Linkage with Correlation-Based Distance ", xlab="", sub ="")

hc=hclust(dd, method ="complete")
plot(hc,hang = -1,main="Complete Linkage with Correlation-Based Distance ", xlab="", sub ="")

hc=hclust(dd, method ="centroid")
plot(hc,hang = -1,main="Centroid Linkage with Correlation-Based Distance ", xlab="", sub ="")



par(mfrow=c(1,3))

c=cutree(hc,h=0.9)
x.mod=cbind(x,c)
x.mod

clust1=x[which(x.mod[,7]==1),]
clust1

clust2=x[which(x.mod[,7]==2),]

clust3=x[which(x.mod[,7]==3),]

boxplot(clust1[,1])


par(mfrow=c(3,3))
#text(1,"For Fresh Products")
boxplot(clust1[,1],main="Cluster 1",sub="FRESH")
boxplot(clust2[,1],main="Cluster 2",sub="FRESH")
boxplot(clust3[,1],main="Cluster 3",sub="FRESH")

boxplot(clust1[,2],main="Cluster 1",sub="MILK")
boxplot(clust2[,2],main="Cluster 2",sub="MILK")
boxplot(clust3[,2],main="Cluster 3",sub="MILK")


boxplot(clust1[,3],main="Cluster 1",sub="GROCERY")
boxplot(clust2[,3],main="Cluster 2",sub="GROCERY")
boxplot(clust3[,3],main="Cluster 3",sub="GROCERY")

boxplot(clust1[,4],main="Cluster 1",sub="FROZEN")
boxplot(clust2[,4],main="Cluster 2",sub="FROZEN")
boxplot(clust3[,4],main="Cluster 3",sub="FROZEN")

boxplot(clust1[,5],main="Cluster 1",sub="DETERGENTS")
boxplot(clust2[,5],main="Cluster 2",sub="DETERGENTS")
boxplot(clust3[,5],main="Cluster 3",sub="DETERGENTS")

boxplot(clust1[,6],main="Cluster 1",sub="DELICATESSEN")
boxplot(clust2[,6],main="Cluster 2",sub="DELICATESSEN")
boxplot(clust3[,6],main="Cluster 3",sub="DELICATESSEN")


x1=cbind(x.orig,c)

par(mfrow=c(1,2))
clust11=x1[which(x1[,9]==1),]


clust12=x1[which(x1[,9]==2),]

clust13=x1[which(x1[,9]==3),]

par(mfrow=c(2,3))
hist(clust11[,1],main="Channel: Cluster 1",sub=length(clust11[,1]))
hist(clust12[,1],main="Channel: Cluster 2",sub=length(clust12[,1]))
hist(clust13[,1],main="Channel: Cluster 3",sub=length(clust13[,1]))

hist(clust11[,2],main="Region: Cluster 1",sub=length(clust11[,2]))
hist(clust12[,2],main="Region: Cluster 2",sub=length(clust12[,2]))
hist(clust13[,2],main="Region: Cluster 3",sub=length(clust13[,2]))





hcd = as.dendrogram(hc)
plot(hcd, type="triangle")

install.packages("ape")
library(ape)
plot(as.phylo(hc), type = "fan")

rect.hclust(hc, k = 7, border = 2:9)


x=x[,3:8]
sub=cutree(hc,k=3)
table(sub)

install.packages("factoextra")
library(factoextra)

fviz_cluster(list(data = x, cluster = sub))


install.packages("dendextend")
library("dendextend")
hc1=hc.average
d1=as.dendrogram (hc)
d2=as.dendrogram (hc1)

tanglegram(d1, d2)
