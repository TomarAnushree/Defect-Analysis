setwd(choose.dir())
library(readxl)

#Preprocessing of data
data<-read_excel('Defect analysis sample Sheet - Copy.xlsx')
data<-data[-c(1,2),]
colnames(data)<-as.character(data[1,])
data<-data[-1,]
data<-data[,-c(12:17)]

#changing the data type of variable before applying PCA on mixed data
data$Shift<-as.factor(data$Shift)
data$`Line no.`<-as.factor(data$`Line no.`)
data$Product<-as.factor(data$Product)
data$`Defect Name`<-as.factor(data$`Defect Name`)
data$`Section No.`<-as.factor(data$`Section No.`)
data$`Primary Cause`<-as.factor(data$`Primary Cause`)
data$`Root Cause Category`<-as.factor(data$`Root Cause Category`)
data$`Operator Name`<-as.factor(data$`Operator Name`)
data$`Hour no.`<-as.numeric(data$`Hour no.`)
data$`No. of Occurance`<-as.numeric(data$`No. of Occurance`)
data$Date<-as.Date(data$Date,format = "%d.%m.%Y")

#To cluster categorical data by k-mean clustering method we use PCA(Principle Component Analysis)Scores
#PCA of mixed data (ordinary PCA and multiple correspondence analysis.)
library(PCAmixdata)

#Root Cause Category is excluded from analysis
#numerical data
X.quanti <- splitmix(data[,-11])$X.quanti

#categorical data
X.quali <- splitmix(data[-11])$X.quali
pca<-PCAmix(X.quanti,X.quali,ndim=62)
#pca<-PCAmix(X.quanti,X.quali,ndim=4,graph=FALSE)

pca

#a matrix containing the eigenvalues, the percentages of variance and the cumulative percentages of variance.
pca$eig
#eigenvalue>1 mean more variance 
#in our  analysis first 5 PC explain only 13% of the variance and till dim-62 70% of variance
#As we know that total variation in the data represents the information contains in the data.and PCA is used 
#to identify the PC along which the variation in the data is maximal.



#ind-a list containing the results for the individuals (observations):
#$coord: factor coordinates (scores) of the individuals
pcScores<-as.data.frame(pca$ind$coord)#upto 62 dim

#K-means cluster
km<-kmeans(pcScores,5,nstart = 25, iter.max = 10) 
str(km)
library(animation)
km<-kmeans.ani(pcScores,5)

#custom mode function
mode<-function(x){uniqv<-unique(x)
uniqv[which.max(tabulate(match(x,uniqv)))]}

g2 = aggregate(data[-1],list(km$cluster),mode)
final2<-data.frame(Cluster=g2[,1],Freq=as.vector(table(km$cluster)),g2[,-1])


final2_membership<- data.frame(km$cluster,data) # append cluster membership
View(final2_membership)


#cluster stability:
library(factoextra)
km.5 <- eclust(pcScores, "kmeans", k = 5, graph = TRUE)#0.04
fviz_silhouette(km.5)

km.5$silinfo
#silhauette value range -1 to +1, high value indicates similarity within the cluster.

#computing Dunn index and other cluster validation statistics
library(fpc)
km_stats<-cluster.stats(dist(pcScores),km$cluster)
#dun index
km_stats$dunn#0.12

clust_stats<-cluster.stats(d=dist(pcScores),as.numeric(data$`Root Cause Category`),km$cluster)
#corrected random index
clust_stats$corrected.rand#0.0044
#VI
clust_stats$vi#3.02


#cross tabulation between k-means and the responce variable
#calculate purity
freq<-table(data$`Root Cause Category`,km$cluster)
sum(apply(freq, 2, max)) / nrow(pcScores)#0.2852


#Cluster of mixed data
library(clustMixType)
data1<-as.data.frame(data[,-11])
cluster<-kproto(data1,5)#lambda 2.39

clprofiles(cluster, data1)#


#cluster purity
freq<-table(data$`Root Cause Category`,cluster$cluster)
sum(apply(freq, 2, max)) / nrow(data)#0.2837209

g2 = aggregate(data[-1],list(cluster$cluster),mode)
final2<-data.frame(Cluster=g2[,1],Freq=as.vector(table(km$cluster)),g2[,-1])


final2_membership<- data.frame(cluster$cluster,data) # append cluster membership
View(final2_membership)

#cluster obtained by two approaches are almost same in term of Purity














#Reference:https://cran.r-project.org/web/packages/PCAmixdata/vignettes/PCAmixdata.html
##www.sthda.com (PCA,Cluster stability )
##https://stats.stackexchange.com (how to calculate purity?)







