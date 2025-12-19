setwd("D:\\Introduction to statistical learning with applications in R")

# Load NCI60 cancer cell line microarray data
# 6,830 gene expression measurements on 64 cancer cell lines.

library(ISLR)
?NCI60
nci.labs <- NCI60$labs
nci.data <- NCI60$data

# cell lines x genes 
fix(nci.data)
dim(nci.data)


# cell line labels
# Each cell line is labeled with a cancer type
fix(nci.labs)
length(nci.labs)

table(nci.labs)

# Perform PCA on nci.data after scaling columns (genes)
pca.out <- prcomp(nci.data, scale=TRUE)

# max number of PC is min(n, p) 
# sometimes written as min(n-1, p)
dim(pca.out$rotation)

cols <- rainbow(length(labs))
names(cols) <- unique(nci.labs)

# R looks at each element of nci.labs and fetches the value in cols with the same name.
plot.cols <- cols[nci.labs]

par(mfrow=c(1,2))

# PC1 vs PC2
plot(pca.out$x[,1:2],
     col = plot.cols, pch = 19,
     xlab = "PC1", ylab = "PC2")


# PC1 vs PC3
plot(pca.out$x[,c(1,3)],
     col = plot.cols, pch = 19,
     xlab = "PC1", ylab = "PC3")

# we can see points (cell lines) with same color that is same cancer type are clustered together
# all blues are close to each other, all greens are close to each other etc

# a summary of the proportion of variance explained by PCs
summary(pca.out)

# the variance explained by the first few principal components.
# pca.out$sdev ** 2
plot(pca.out)

# Scree plot
# Proportion of variance explained by each PC
pve <- (pca.out$sdev ^ 2) / sum(pca.out$sdev ^ 2) * 100

# plot PVE and cumulative PVE of each PC
par(mfrow =c(1,2))
plot(pve, ylab="PVE", xlab="Principal Component", col="blue", type="o")
plot(cumsum(pve), ylab="Cumulative PVE", xlab="Principal Component", col="red", type="o")

# Clustering

# Standardize data to have 0 mean and 1 sd
sd.data <- scale(nci.data)

par(mfrow=c(1,3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab ="", sub ="", ylab ="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab ="", sub ="", ylab ="")
plot(hclust(data.dist, method="single"), labels=nci.labs, main="Single Linkage", xlab ="", sub ="", ylab ="")

# choice of linkage affects clustering results
# complete (default method) and average give us more balanced clusters. so they are preferred over single linkage.


par(mfrow=c(1,1))
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab ="", sub ="", ylab ="")

# mostly we see observation (here cell line) with same cancer type are clustered together

# lets use linkage = complete
# number of clusters = 4
hc.out <- hclust (dist(sd.data))
hc.out

hc.clusters <- cutree(hc.out, 4)
hc.clusters

# All the leukemia cell lines fall in cluster 3
# breast cancer cell lines are spread out over three different clusters (1 2 and 4)
table(hc.clusters, nci.labs)

# plot the cut on the dendogram
# look at the dendogram and see where on y we make a line so we get 4 clusters
plot(hc.out, labels = nci.labs)
abline (h=139, col ="red")

# K-Means with 4 clusters
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster

table(km.clusters, nci.labs)

# hierarchical clustering vs K-Means
# Cluster 4 in K-means clustering is identical to cluster 3 in hierarchical clustering
table(km.clusters, hc.clusters)


# perform hierarchical clustering on the first few principal component score vectors
# hclust on first 5 PC
hc.out <- hclust (dist(pca.out$x [ ,1:5]))

plot(hc.out, labels = nci.labs)

table(cutree(hc.out, 4), nci.labs)

# perform K-Means clustering on the first few principal component score vectors
set.seed(2)
km.out <- kmeans(pca.out$x [ ,1:5], 4, nstart = 20)
km.out

km.clusters <- km.out$cluster
km.clusters

km.out$tot.withinss
table(km.clusters, nci.labs)

plot(pca.out$x[,1], pca.out$x[,2],
     col = km.clusters,
     pch = 19,
     xlab = "PC1",
     ylab = "PC2",
     main = "K-means Clusters on PCA (PC1 vs PC2)",
     cex.main=0.9)

