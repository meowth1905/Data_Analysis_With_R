setwd("D:\\Introduction to statistical learning with applications in R")

library(ISLR)
library(MASS)

head(USArrests)
dim(USArrests)

row.names(USArrests)
names(USArrests)

# USArrests is a data.frame
class(USArrests)

# check if all columns are numeric
sapply(USArrests, is.numeric)

# apply can be used on matrix or data.frame with all numeric columns
# to briefly examine data, check their means
# means of all 4 variables are different
apply(USArrests, 2, mean)

# check variance of variables
apply(USArrests, 2, var)

?USArrests
# units of UrbanPop is different to other 3 variables
# UrbanPop measures the percentage of the population in each state living in an urban area
# other three: number of rapes/murders/assualts in each state per 100,000 individuals.

# PCA 

# Assault  has the largest mean and variance
# If we dont scale variables before PCA
# Most of PCs will be driven mainly by Assault  
# it is important to standardize the variables to have mean zero and standard deviation one before performing PCA

# By default prcomp() centers the variables to have mean 0
# scale=TRUE scale variable to have sd=1
pca.out <- prcomp(USArrests, scale=TRUE)
names(pca.out)

# mean of variables prior standardization
pca.out$center

# sd of variables prior standardization
pca.out$scale

# principal component loadings
pca.out$rotation

# we see there are 4 principal components
# min(n-1, p) principal components
# min(49, 4) so 4 PCs and 4 PC loadings

# principal component scores
pca.out$x
dim(pca.out$x) 

# Scatter plot of PC1 vs PC2
plot(pca.out$x[,1], pca.out$x[,2],
     xlab = "PC1", ylab = "PC2",
     pch = 19, col = "blue")

# Add state names
text(pca.out$x[,1], pca.out$x[,2],
     labels = rownames(USArrests),
     pos = 4, cex = 0.6)

# in a biplot we can also show loading along with scores
# which variables contribute most to each principal component
biplot(pca.out, scale=0)

# Flipping the signs of all the loadings and scores for a component doesn’t change the meaning
pca.out$rotation <- -pca.out$rotation
pca.out$x <- -pca.out$x
biplot(pca.out, scale=0, cex=0.6)

# other ways to plot
# 1.
library(factoextra)
library(ggrepel)

fviz_pca_biplot(pca.out, repel = TRUE)

# 2. 
library(ggplot2)
library(ggrepel)

scores <- as.data.frame(pca.out$x)
scores$state <- rownames(scores)

loadings <- as.data.frame(pca.out$rotation)
loadings$varname <- rownames(loadings)

arrow_scale <- 5

ggplot() +
  # Points for states
  geom_point(data = scores,
             aes(PC1, PC2),
             color = "blue") +
  
  # Labels for states
  geom_text_repel(data = scores,
                  aes(PC1, PC2, label = state),
                  size = 3) +
  
  # Arrows for variable loadings
  geom_segment(data = loadings,
               aes(x = 0, y = 0,
                   xend = PC1 * arrow_scale,
                   yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "red") +
  
  # Labels at the tips of arrows
  geom_text_repel(data = loadings,
                  aes(x = PC1 * arrow_scale,
                      y = PC2 * arrow_scale,
                      label = varname),
                  color = "red",
                  size = 4) +
  
  theme_minimal() +
  labs(title = "PCA Biplot",
       x = "PC1",
       y = "PC2")

# from biplots we can see
# PC1 is strongly associated with overall crime rates (Murder, Assault, Rape).
# States with higher values on PC1 tend to have higher crime rates.
# UrbanPop contributes more strongly to PC2 than to PC1.

# States far to the right (high PC1) generally have higher crime rates overall, 
# and you’ll see them tending toward red if their assault rates are also high.
# States higher/lower on PC2 differ more by urbanization than by crime severity

df <- data.frame(pca.out$x, Assault=USArrests$Assault, State = rownames(USArrests))

ggplot(df, aes(PC1, PC2, color=Assault, label=State)) +
  geom_point(size = 2) +
  geom_text(vjust = -0.5, size = 3) +
  scale_color_gradient(low = "forestgreen", high = "red") + theme_minimal()


# sd of PCs
pca.out$sdev

# variance explained by each PC
pca.out$sdev ^ 2

# PVE
# proportion of variance explained by each PC
# first PC explains about 62.0% of variance in the data
# second PC explains about 24.7% of variance in the data
pca.out$sdev ^ 2 / sum(pca.out$sdev ^ 2)
pve <- round(pca.out$sdev ^ 2 / sum(pca.out$sdev ^ 2), 4)
pve

# plot PVA
plot(pve, ylab="Proprtion of Variance Explained", ylim=c(0,1), xlab="Principal Component", type="b")

# plot cumulative PVE
# first 2 PCs explain > 80% of variance in the data
# all 4 PCs will explain 100% of variance in the data
plot(cumsum(pve), ylab="Proprtion of Variance Explained", ylim=c(0,1), xlab="Principal Component", type="b")

cumsum(c(1,2,3,4))


# Clustering
# K-Means Clustering

# dataset with 2 true clusters 
# rows 1-25 obs will be centered around (5, -5)
# rows 26-50 obs will be centered around (0, 0)
set.seed(2)
x=matrix (rnorm (50*2) , ncol =2)
x[1:25, 1] <- x[1:25, 1] + 5
x[1:25, 2] <- x[1:25, 2] - 5

colnames(x) <- c("col1", "col2")
plot(x[, "col1"], x[, "col2"])

?kmeans
# centers = number of clusters (k).
# nstart = number of random initializations to try; higher values give more reliable clustering.
# default is 1

# 2 clusters
# 20 different random initializations. 
# Each initialization runs until convergence, and then R picks the best clustering among those 20
km.out <- kmeans(x, 2, nstart = 20)
names(km.out)

# cluster assignment of each observation
km.out$cluster

# plot clusters
plot(x, col=km.out$cluster + 2, main="K-Means Clustering Results with K=2", xlab ="", ylab="", pch =19, cex.main=0.9)

km.test <- kmeans(x, 2)
plot(x, col=km.test$cluster + 2, main="K-Means Clustering Results with K=2", xlab ="", ylab="", pch =19, cex.main=0.9)


# K‑means starts by choosing random initial cluster centers
# Different random starts can lead to different clustering results because the algorithm may converge to different local minima.
# use set.seed() 
# so that the initial cluster assignments in Step 1 (of K means algorithm) can be replicated, and the K-means output will be fully reproducible.

# we don't know how many so lets try 3
set.seed(3)
km.out <- kmeans(x, 3, nstart=20)
km.out
plot(x, col=km.out$cluster, main="K-Means Clustering Results with K=3", xlab ="", ylab="", pch =19, cex.main=0.9)

# sum of within‑cluster variances (squared distances of points to their cluster centers) across all clusters
# k-means algorithm try to minimize this
km.out$tot.withinss

# with nstart=1 (default) km.out$tot.withinssis higher

km.out <- kmeans(x, 3, nstart=1)
km.out$tot.withinss

# always recommended to use high values for nstart like 20 or 50
# otherwise undesirable local minima may be obtained 

# Clustering
# Hierarchical Clustering

set.seed(3)
x=matrix (rnorm (50*2) , ncol =2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 3

# dist() measure Euclidean distance
hc.complete <- hclust(dist(x), method="complete")
hc.single <- hclust(dist(x), method="single")
hc.average <- hclust(dist(x), method="average")

# plot the dendrograms
par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", sub ="", cex =.9)
plot(hc.single, main="Single Linkage", xlab="", sub ="", cex =.9)
plot(hc.average, main="Average Linkage", xlab="", sub ="", cex =.9)

# to determine which cluster each observation is assigned 
?cutree

# desired number of groups = 2
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

# to scale before clustering 
par(mfrow=c(1,2))
plot(hc.complete, main="Complete Linkage without scaling x", xlab="", sub ="", cex =.9)

x.scaled <- scale(x)
plot(hclust(dist(x.scaled), method="complete"), main="Complete Linkage with scaling x", xlab="", sub ="", cex =.9)

# correlation based distance 
# only work for 3D points so atleast 3 features

x=matrix (rnorm (30*3) , ncol =3)

# cor(x) measure correlation between columns
# for clustering observations, take cor(t(x))

dd = as.dist(1 - cor(t(x)))

par(mfrow=c(1,1))
plot(hclust (dd, method ="complete"), main=" Complete Linkage with Correlation-Based Distance ", xlab="", sub ="")
