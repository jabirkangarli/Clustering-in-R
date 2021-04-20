

# # Introduction

# This dataset was populated from destination reviews published by 249 reviewers of holidayiq.com till October 2014. Reviews are split into 6 categories among destinations 
# across South India were considered and the count of reviews in each category for every reviewer (traveler) is captured.
# 
# Attribute Information:
#   
#   Attribute 1 : Unique user id
# 
# Attribute 2 : Number of reviews on stadiums, sports complex, etc.
# 
# Attribute 3 : Number of reviews on religious institutions
# 
# Attribute 4 : Number of reviews on beach, lake, river, etc.
# 
# Attribute 5 : Number of reviews on theatres, exhibitions, etc.
# 
# Attribute 6 : Number of reviews on malls, shopping places, etc.
# 
# Attribute 7 : Number of reviews on parks, picnic spots, etc.


#  #Data
library(tidyverse) 
library(cluster)
library(factoextra)
library(dplyr)
library(NbClust)

bm <- read.csv("~\\db\\buddymove_holidayiq.csv")
head(bm)

## 1. Selection of objects and variables
bm_reviews <- bm[,2:7]
head(bm_reviews)

## 2. Basic statistical applications

# Applying basic statistics before implementing k means to check to scale the data or not.

stats <- data.frame(
  Min = apply(bm_reviews, 2, min),      # minimum 
  Med = apply(bm_reviews, 2, median),   # median 
  Mean = apply(bm_reviews, 2, mean),    # mean
  SD = apply(bm_reviews, 2, sd),        # standard deviation
  Max = apply(bm_reviews, 2, max)       # maximum
)
stats <- round(stats, 1)
head(stats)

# As we can see, the minimum and maximum value of the sport variable is less than the rest. 
# Therefore, we have to scale the data.

bm_scaled <- scale(bm_reviews)
head(bm_scaled)

stats<- data.frame(
  Min = apply(bm_scaled, 2, min),               # minimum
  Med = apply(bm_scaled, 2, median),            # median
  Mean = apply(bm_scaled, 2, mean),             # mean
  SD = apply(bm_scaled, 2, sd),                 # Standard deviation
  Max = apply(bm_scaled, 2, max)                # maximum
)
stats <- round(stats, 1)
head(stats)

# Clustering

# Initializing total within sum of squares error: wss

wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  kmn <- kmeans(bm_scaled, centers = i, nstart = 20)
# Saving the total within sum of squares to wss variable
  wss[i] <- kmn$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

#  Based on the plot, we can say that the elbow where the quality improves slowly as the k (number of clusters) increases,
#  which means since the model complexity increases, the quality of the model is no longer improving. 

# Let's do it with fviz_nbclust() to see the optimum number of clusters.


fviz_nbclust(bm_scaled, kmeans, method="wss") 

# Elbow method minimizes total
# within-cluster sum of squares (wss). 

# Silhouette measures the quality of a cluster, i.e., how well each point lies within its cluster.

fviz_nbclust(bm_scaled, kmeans, method="silhouette")

# Determining the number of clusters by using gap statistics
gapstatkmeans <- clusGap(bm_scaled,FUN = kmeans,K.max = 25,B = 100)
 
# According to gap statistics we can say that the optimal number of clusters is 2.


# 2. Choice of classification methods

### 2.1 K-means

# Let's choose 2 clusters and execute it.
k <- 2
kmean=kmeans(bm_scaled,centers = 2,nstart = 25)

# Visualization of the cluster plot
fviz_cluster(kmean, data=bm_scaled)

# Now, let's check the number of observations in each cluster
kmean$size

# Total SSE of the clusters and each cluster respectively. SSE stands for Sum of Squared Error
print(kmean$tot.withinss)

print(kmean$withinss)

for(i in 1:3)
{
  print(i)
  print(which(kmean$cluster==i))
}


# In cluster one, the users who has given more reviews about nature and picnic are clustered together. 
# It is obvious that people enjoy nature prefer to spend more time with family having picnics. 
# For instance, for cluster points 73,84,94 the nature and picnic values are high than the rest. 
# So they are clustered together.
# Something I found out interesting was the users who rated can be mothers/women of the family. 
# I say so because the ratings on Religious,Shopping and Picnic are specifically high.

# In cluster two, the sports ratings play a role. If we can see the clusters more in detail, 
# we can find the users who prefer watching movies and sports on tv than outdoors.

### 2.2 Hierarchical clustering

# Now we will upload the data once again, and execute operations on Hierarchical clustering.
set.seed(1122)
DataSet <- read.csv("buddymove_holidayiq.csv")
DataSet
SubSet<-sample_n(DataSet, 50)
rownames(SubSet)<-SubSet$User.Id
SubSet<-SubSet[2:7]
SubSet

# We have to scale the data as before.
x<-scale(SubSet)
head(x)

### 2.2.1 Linkage methods comparisons.

# By using linkage methods, we will compare the results of them.

# Complete linkage
complete_linkage<- eclust(x, "hclust", hc_method = "complete",k=1)
fviz_dend(complete_linkage, show_labels=T, palette="jco")

# The number of singleton clusters: 19

# Single linkage
single_linkage<- eclust(x, "hclust", hc_method = "single",k=1)

fviz_dend(single_linkage, show_labels=T, palette="jco",main='Single Linkage')

# The number of singleton cluster pairs: 15

# Average linkage
average_linkage<- eclust(x ,"hclust", hc_method ="average",k=1)

fviz_dend(average_linkage ,show_labels=T,palette="jco", main='Average Linkage')

#The total number of singleton cluster pairs 18

# Complete Linkage: The number of singleton cluster pairs: 19
# Single Linkage: The number of singleton custer pairs : 15
# Average Linkage: The total number of singleton cluster pairs: 18 


# According to the assumption I take, the single linkage has the smallest number of singleton pairs and 
# I consider the purest.
#cutree(single_linkage,)
cutree(single_linkage,h=1.7)
plot(single_linkage)
abline(h=1.7,col="red")

complete_linkage2<- eclust(x, "hclust", hc_method = "complete",k=2)
fviz_dend(complete_linkage2, show_labels=T, palette="jco")

single_linkage2<- eclust(x, "hclust", hc_method = "single",k=2)
fviz_dend(single_linkage2, show_labels=T, palette="jco")

average_linkage2<- eclust(x, "hclust", hc_method  = "average",k=2)
fviz_dend(average_linkage2, show_labels=T, palette="jco")

# Statistics of methods:

complete_statastics <- fpc::cluster.stats(dist(x), complete_linkage2$cluster)
complete_statastics$avg.silwidth

single_statastics <- fpc::cluster.stats(dist(x), single_linkage2$cluster)
single_statastics$avg.silwidth

average_statastics <- fpc::cluster.stats(dist(x), average_linkage2$cluster)
average_statastics$avg.silwidth

# According to the average silhouette index, the average linkage is the best.

#install.packages("NbClust")

library(NbClust)
NbClust(x,method = "complete")

NbClust(x,method = "single")

NbClust(x,method = "average")

plot(silhouette(cutree(complete_linkage2,3),dist(x)))

plot(silhouette(cutree(average_linkage2,5),dist(x)))

# The one based on purity, lowest number of singleton nodes gives us single_linkage to be the best. 
# The clustering performed with nbclust gave us a good silhouette index for complete_linkage. 
# And we can see in the plot, the nbclust gave an elbow- shaped drop in 3 clusters for complete and 3 for single, 
# and 5 for average. The higher the silhouette  index, the good structure is present for the clusters.

# I think the Complete linkage will be suited for the dataset since it clusters properly and gives us a higher good structure.

# CLARA 

df <- read.csv("buddymove_holidayiq.csv")
df<-df[2:7]
df

# let's scale the data
df_scaled<-scale(df)
head(df_scaled)

# try out with 2 clusters
clara_flex<-eclust(df_scaled, "clara", k=2) 
summary(clara_flex)
fviz_cluster(clara_flex)
fviz_silhouette(clara_flex)

# Since we have the silhouette results, we can say that the one is close to 1 has the highest quality which means here the first cluster has highest quality.
# And we have a lot of errors/ probable mistakes in the cluster number 2. When the silhouette width for particular object is close to zero,
# this is like a boundary line. We are not sure how to assign this. On the other hand, when the index is less than zero, it means that we did 
# mistake. 

# Another approach - 2 clusters and 6 samples specified 
clara_clust<-clara(df_scaled, 2, metric="euclidean", stand=FALSE, samples=6,
                   sampsize=50, trace=0, medoids.x=TRUE,
                   rngR=FALSE, pamLike=FALSE, correct.d=TRUE)
class(clara_clust)
clara_clust

# Visualization of the output
fviz_cluster(clara_clust, geom="point", ellipse.type="norm") 
fviz_cluster(clara_clust, palette=c("#00AFBB", "#FC4E07", "#E7B800"), ellipse.type="t", geom="point", pointsize=1, ggtheme=theme_classic())
fviz_silhouette(clara_clust)

# According to the average Silhouette measure, we can say that first cluster is the best.

# ------------------------------------------------------------------------------------
#   Sources:
#   
#   1. .*Jacek Lewkowicz. Unsupervised Learning. Presentation from the classes.*
#   
#   2. .*Introduction to statistical data analysis with R. Matthias Kohl. Furtwangen University.*
#   
#   3.  *https://cran.r-project.org/web/packages/clusterSim/clusterSim.pdf*
#   
#   

