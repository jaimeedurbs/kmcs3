library(factoextra)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(cluster)
library(reshape2)

head(read.csv("cod.csv"))

df <- read.csv("cod.csv")
df <- df[,-1]
df <- na.omit(df)

cormat <- round(cor(df), 2)
head(cormat)

################################################################################
# Here we are just creating a heatmap of correlations to see if any patterns
# immediately stick out or for stuff to keep an eye on later in the process

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
################################################################################

################################################################################
#Now for the beans and potatoes. We begin by scaling the data since we don't
#want the clustering algorithm to depend on an arbitraty variable unit.

data <- scale(df)
head(data)

#The first step in the process is to decide how many clusters we want generated
#in the final output. The k-means algorithm will randomly select k objects from
#the data to serve as initial centroids. From here, each remaining object is
#assigned to its closest centroid as measured by Euclidean distance between each
#object and the cluster mean. After this, the algorithm finds the new mean value
#for each cluster. Each observation is checked again and reassigned if there is
#a new closest centroid. This process is repeated until the cluster assignments
#stop changing. The process by which assignments stop changing is called
#"convergence". This process is summarised:

#1. The analyst specifies the number of clusters
#2. K objects are randomly selected as initial cluster centers
#3. Each observation is assigned to its closest centroid based on Euclidean
#distance from the object to the centroid.
#4. For each k cluster, the centroid is updated after calculating the new mean
#vale of each data point within the cluster.
#5. The total within sum of square is minimized by iterating steps 3 and 4 until
#the cluster assignments stop changing or the max number of iterations is
#reached. In R, 10 iterations is default, but this can be set by the analyst.


#Below shows 4 different k means calculations using 2, 3, 4, and 5 centroids.
#While this shows us where different clustering can occur, there is no
#indication as to what the optimal number of clusters are.
k2 <- kmeans(data, centers = 2, nstart = 25)
k3 <- kmeans(data, centers = 3, nstart = 25)
k4 <- kmeans(data, centers = 4, nstart = 25)
k5 <- kmeans(data, centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = data) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = data) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = data) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = data) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

#There are a few methods for determinining the optimal number of clusters below.


#Determine optimal number of clusters
set.seed(123)

#1 Elbow method
#This method uses the following algorithm:
#1. Vary the amount of clusters, k, and compute the clustering algorithm
#2. Calculate the total within-cluster sum of squares (wss) for each k
#3. Plot the curve of wss based on k
#4. Where the curve "elbows" or bends significantly is indication of the optimal
#number of clusters to use for the analysis.

fviz_nbclust(data, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)

#From this method, we can see that 2 is the optimal number of clusters.

#2 Silhouette method
#This method analyzes the quality of clustering, which is to say it determines
#how well each object fits within its assigned cluster.
#Higher aerage silhouette widths indicate good clustering.
#Similar to the elbow method, this method varies the value of k and finds the
#value that maximizes the average silhouette.

fviz_nbclust(data, kmeans, method = "silhouette")

#Again, this method returns 2 as the optimal number of clusters for this data.

#Final analysis
set.seed(123)
final <- kmeans(data, 2, nstart = 25)
print(final)

#Visualize results
fviz_cluster(final, data = data)

df %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
