#rcodes project

library(readr)
data <- read_csv("Wholesale customers data.csv", 
                 col_types = cols(X10 = col_skip(), X11 = col_skip(), 
                                  X12 = col_skip(), X9 = col_skip()))
View(data)

#removed columns which had details of the columns


x=matrix(c(data$Channel,data$Region,data$Fresh,data$Milk,data$Grocery,data$Frozen,data$Detergents_Paper,data$Delicassen), ncol=8,byrow=FALSE)
x.new=x[,3:8]



par(mfrow=c(2,4))
hist(x[,1],main="Histogram of Channel",sub="Horeca = 1, Other = 2")
hist(x[,2],main="Histogram of Region",sub="Lisbon = 1, Oporto = 2, Other = 3")
boxplot(x[,3],main="Boxplot of Fresh Products")
boxplot(x[,4],main="Boxplot of Milk Products")
boxplot(x[,5],main="Boxplot of Grocery Products")
boxplot(x[,6],main="Boxplot of Frozen Products")
boxplot(x[,7],main="Boxplot of Det_Paper Products")
boxplot(x[,8],main="Boxplot of Del_ Products")



library(MASS)
parcoord(x.new,var.label = TRUE)

x.orig=x
x=x.new
x[,3:8]=scale(x[,3:8])
#performing clustering
#default distance is euclidean
hc.complete =hclust(dist(x), method="complete")
hc.average =hclust(dist(x), method ="average")
hc.single=hclust(dist(x), method ="single")
hc.centroid=hclust(dist(x), method ="centroid")

#making plots
par(mfrow=c(2,2)) 
plot(hc.complete,main="Complete Linkage")
plot(hc.average, main="Average Linkage")
plot(hc.single, main="Single Linkage")
plot(hc.single, main="Centroid Linkage")

cutree(hc.complete,7)
cutree(hc.single,7)
cutree(hc.average,7)
hist(cutree(hc.complete,7))
hist(cutree(hc.single,7))
hist(cutree(hc.average,7))


xsc=scale(x)
plot(hclust(dist(xsc), method ="complete"), main="Hierarchical Clustering with Scaled Features")
plot(hclust(dist(xsc), method ="average"), main="Hierarchical Clustering with Scaled Features")
plot(hclust(dist(xsc), method ="single"), main="Hierarchical Clustering with Scaled Features")

#for correlation
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method ="complete"), main="Complete Linkage with Correlation-Based Distance ", xlab="", sub ="")
plot(hclust(dd, method ="average"),hang = -1,main="Average Linkage with Correlation-Based Distance ", xlab="", sub ="")
plot(hclust(dd, method ="single"), main="Single Linkage with Correlation-Based Distance ", xlab="", sub ="")

hcd = as.dendrogram(hc)
plot(hcd, type="triangle")

install.packages("ape")
library(ape)
plot(as.phylo(cut(hc,400)), type = "fan")
