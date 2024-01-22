### CLUSTER ANALYSIS ##### 
###https://www.youtube.com/watch?v=Hyqr4m7tle0
###scaling data ##
library(readxl)
clus <-  data_ge
View(clus)
inf.m <- scale(clus[,-c(1:3)]) # remove columns not needed eg. genotypes
inf.m
d <- dist(inf.m)
options(max.print =10000000 )
d

###### k means ###
k2 <- kmeans(inf.m, 2)
k2
k2$cluster

### factoextra ##
install.packages("factoextra")
library(ggplot2)
library(factoextra)
fviz_nbclust(inf.m, kmeans, method = "wss") #methods of finding optimum clusters
fviz_nbclust(inf.m, kmeans, method = "silhouette") #mthods of finding optimum clusters

###a. Nb clust for findng the optimum number of clusters ###
install.packages("NbClust")
library(NbClust)

NbClust(data= inf.m, diss=NULL, distance = "euclidean", min.nc=1, max.nc=2, 
             method = "kmeans", index = "all", alphaBeale = 0.1) # this tells the number of clusters

####Set the number of clusters according to number above, say 5 ##
setk <- kmeans(inf.m, 2)

## Seeing cluster membership ##
setk$cluster
#Plotting data set of clusters ##
plot(clus,cols=setk$cluster)

###b. Finding cluster number using Heirachical clustering ###
### Nb Clust using squared eculidean distance D2 ##
NbClust(data= inf.m, diss=NULL, distance = "euclidean", min.nc=2, max.nc=6, 
        method = "ward.D2", index = "all", alphaBeale = 0.1) # this tells the number of clusters
wardse <- hclust(d,"ward.D2")
plot(wardse)
##Plotting dendogram ##
plot(wardse, hang = -1)
plot(wardse, hang = -1, labels = clus$Genotype) # plots with names of genotypes

##Ploting dendrograms with 3 cluster divided by borders ###
rect.hclust(wardse, k=3, border = "blue")
rect.hclust(wardse, k=3, border = 2:5)
## Findinf the clustre membership ##
hcm <- cutree(wardse, k=3) # amend k the valus of k as appropriate
#List of membership of hirachical slustering ##
hcm
### inter and intra cluster distances ###
install.packages("clv")
library(clv)
## k-means ##
kid<- cls.scatt.data(inf.m, setk$cluster)
print(kid)
## h-clust ##
hid<- cls.scatt.data(inf.m, hcm)
print(hid)
## create excel sheet of intra and inter cluster distances#

## finding cluster means of average scores ##
aggregate(clus[, -1:-2], list(setk$cluster), mean)
## remove columns containing genotype, mean values of all variables show

