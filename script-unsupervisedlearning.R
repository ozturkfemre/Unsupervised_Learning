################################################################################

#################
### Libraries ###
#################

library(readr)
library(cluster)
library(clusterSim)
library(factoextra)
library(corrplot)
library(stats)
library(clustertend)
library(psych)
library(NbClust)
library(clValid)
library(plotly)
library(fpc)
library(mclust)
library(dbscan)
library(tidyverse)
library(ggpubr)

################################################################################

###################
### Data Import ###
###################

# dataset
wdbc <- read_csv("wdbc.data", col_names = FALSE)

wdbc <- wdbc[1:12]

# changing colnames
colnames <- c("ID", "Diagnosis", "radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave.points", "symmetry", "fractal.dimension" )
names(wdbc) <- colnames

df2 <- wdbc[2:12]
#removing ID and label column
data <- wdbc[3:12]


################################################################################

##############################
### Descriptive Statistics ###
##############################

sum(is.null(data))
# there is no na in the dataset.

# getting descriptive statistics for each variable
summary(data)

# When we check the descriptive statistics of the dataset:
# Mean of the radius variable is higher then than the median. This means that radius variable is right-skewed. When we compare the values of the third quantile and maximum value, it can be referred that there might be some outliers in the variable. However, it is best to make the final judgement with boxplot analysis.
# Mean of the texture variable is higher then than the median. This means that texture variable is right-skewed. When we compare the values of the third quantile and maximum value, it can be referred that there might be some outliers in the variable. However, it is best to make the final judgement with boxplot analysis.
# Mean of the perimeter variable is higher then than the median. This means that perimeter variable is right-skewed. When we compare both the values of the third quantile and maximum value, and min and first quantile value, it can be referred that there might be some outliers in the variable. However, it is best to make the final judgement with boxplot analysis.
# From the descriptive statistics, it can easily be stated that values differ a lot. THe dataset must be scaled. However, if there is high correlation between variable pairs, PCA needs to be applied to the dataset.


################################################################################

############################
### Correlation Analysis ###
############################

corr <- cor(data, method = "pearson")
corrplot(corr, method="color" )

# Correlation matrix of the dataset reveals that there are high correlation between variable pairs.
# Correlation between Radius, perimeter, and area variables are more than 0.98. These are too much.
# Correlation between Compactness, concavity, and  concave points variables are more that 0.83.
# Fractal Dimension değişkeni negatif korrelasyonu olan tek değişken. Radius, Perimeter ve Area değişkenleri ile -0.31'den daha fazla değerlerle korrelasyona sahip.

################################################################################

##########################
### Graphical Analysis ###
##########################

par(mfrow=c(2,5))
boxplot(data$radius, main = "radius", col = "dodgerblue2")
boxplot(data$texture, main = "texture", col = "dodgerblue2")
boxplot(data$perimeter, main = "perimeter", col = "dodgerblue2")
boxplot(data$area, main = "area", col = "dodgerblue2")
boxplot(data$smoothness, main = "smoothness", col = "dodgerblue2")
boxplot(data$compactness, main = "compactness", col = "dodgerblue2")
boxplot(data$concavity, main = "concavity", col = "dodgerblue2")
boxplot(data$concave.points, main = "concave points", col = "dodgerblue2")
boxplot(data$symmetry , main = "symmetriy", col = "dodgerblue2")
boxplot(data$fractal.dimension , main = "fractal dimension", col = "dodgerblue2")
dev.off()

# It can be observed from the boxplots that there are lots of outliers in each variables. 
# Variance of the some variables(radius, texture, perimeter, concavity, concave points) is very high.

indexes = sapply(df2, is.numeric)
indexes["Diagnosis"] = TRUE
df2[,indexes]%>%
  gather(-Diagnosis, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = Diagnosis, color = Diagnosis)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free")+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.85),legend.position="none",
        panel.background = element_rect(fill = "white"))+
  theme(strip.background =element_rect(fill="goldenrod1"))+
  theme(strip.text = element_text(colour = "firebrick3"))

# When the boxplots of the variables according to the levels of Diagnosis are examined, it is noticed that the M level takes higher values for almost every variable. This is not only valid for the fractal.dimension variable. Again, it was noticed that the variance of the M level was higher for all variables except the fractal.dimension variable.
# This makes the dataset clusterable.

################################################################################

####################################
### Principle Component Analysis ###
####################################


# Principal component analysis (PCA) is a technique used to identify patterns in a dataset. It does this by identifying the directions (or "components") in the data that account for the most variation. The first component is the direction in the data that accounts for the most variation, the second component is the direction in the data that accounts for the second most variation, and so on.

#Here is a step-by-step explanation of how PCA is calculated:
  
#Standardize the data: The data is transformed so that each variable has a mean of zero and a standard deviation of one. This is done to ensure that all variables are on the same scale.

#Compute the covariance matrix: This matrix is calculated to determine the relationship between the variables in the dataset.

#Compute the eigenvectors and eigenvalues of the covariance matrix: Eigenvectors represent the directions in the data that account for the most variation, and eigenvalues represent the amount of variation that is accounted for by each eigenvector.

#Select the principal components: The eigenvectors with the highest eigenvalues are chosen as the principal components of the dataset.

#Transform the data: The original dataset is transformed by projecting it onto the principal components, resulting in a new dataset with reduced dimensionality.

#Interpret the components: The principal components are interpreted in terms of the original variables to understand the underlying patterns in the data.
#####################
### Stats Package ###
#####################

# Since there is difference between values, data needs to be scaled while pca is applied.
data.pca <- prcomp(data, center = TRUE, scale. = TRUE)
summary(data.pca)

# Two components seems to be the best to explain the dataset.
# 0.7997 cumulative proportion of the PC2 increases to 0.887 cumulative proportion in the PC3. This increase can be dismissed.
# At PC3 Proportion of variance is very low. This means that PC2 is the best number for the principle component.

data.eigen <- eigen(corr)
eigenvalues <- data.eigen$value
eigenvalues

# Eigen values show that as the closest value to 1, 3 components is the best.

fviz_eig(data.pca)

# According to scree plot, 3 components is the best.


data.pca$rotation[,1:3]

# When the output containing the variables expressed by the components was analyzed, it was realized that the third component expressed only the Texture variable.
# It was also possible to see the anomaly of the Texture variable in the correlation analysis. It is understandable that this variable, which has no significant correlation with any variable, is expressed by another component.
# If the number of components is chosen to be two, it is also observed that 2 components do not express the Texture variable well.
# Adding a component for a single variable did not seem to make much sense with an explanatory cost of 0.9. 
# The low correlation of the Texture variable with other variables was also taken into account to reach this decision.
# Bu kararların kontrolü için bileşen sayısına otomatik olarak karar veren Psych paketi de kullanılmak istenilmiştir.
# Buna ek olarak bileşenlerin hangi değişkenleri ifade ettiğine dair güzel bir görsel sunan bu paketten yararlanılmak istenilmiştir.


#####################
### psych package ###
#####################
dev.off()
x <- fa.parallel(data, fm="pa", fa="both", n.iter=1)

# fa.parallel function in the psych package decides the best component number by itself.
x$ncomp
# it decided the component number to be two.

# following diagram show the contributions of the variables to the components.
fit <- principal(data, x$ncomp, rotate="varimax") 
fa.diagram(fit)

# As it can be seen from the diagram, following variables are explained by the following components.
# PC1 : Radius, Perimeter, Area, Concave Points, Concavity, Texture
# PC2 : Fractal Dimension, Smoothness, Compactness, Symmetry


fviz_pca_ind(data.pca,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE   )

# When the contributions of the observations in the PC1 and PC2 graphs are analyzed, a clustering is observed in the upper right and lower right.
# It can be said that these observations express similar characteristics. 
# For example, when the values of the 79th observation at the bottom left are examined, it can be seen that it has values close to the maximum for all variables except Radius and Texture.
# When the 569th observation values on the opposite axis are examined, it can be seen that Smoothness and Concavity have minimum values, while Texture has a value above the 3rd quartile.


fviz_pca_var(data.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     )

# When the PCA graph of the variables is analyzed, it can be said that the variables with positive correlation point to the same regions. 
# While Area has a positive correlation with Perimeter, which is in the same component, it has a negative correlation with fractal dimension, which is in a different component. 
# The contributions of the variables can be better seen through this graph.


exp(cor(data.pca$x[,1],data.pca$x[,2]))

# After pca, there is no signitficant correlation between components

###########################
### Clustering Analysis ###
###########################

# For cluster analysis, PCA applied dataset will be used.
pcadata <- predict(data.pca)[,1:2]

######################################
###  Measuring Clustering Tendency ###
######################################

# The Hopkins statistic is a measure used to determine the likelihood that a dataset is generated from a uniform distribution, which is useful for determining whether a dataset is suitable for clustering.

#The Hopkins statistic is calculated as follows:
  
#  Generate a random sample of n points from the dataset, where n is a small number (typically n=50).
# Generate a random sample of n points from a uniform distribution, with the same number of dimensions as the dataset.
# Calculate the average distance between each point in the dataset sample and its nearest neighbor in the dataset sample (d(data)).
# Calculate the average distance between each point in the uniform sample and its nearest neighbor in the uniform sample (d(unif)).
# Calculate the Hopkins statistic as: Hopkins = (d(data) / (d(data) + d(unif)))
# A value of Hopkins statistic close to 1 indicates that the dataset is suitable for clustering, while a value close to 0 indicates that the dataset is not suitable for clustering and might have been generated from a uniform distribution.

# It should be noted that Hopkins statistic is not a definitive test and it may not be a reliable indicator for high-dimensional datasets or datasets with non-convex structure. Therefore, other methods such as visualizing the data, or using other clustering validation metrics, such as Silhouette score, should be used in conjunction with Hopkins statistic to evaluate the clusterability of a dataset.



hopkins.data <- hopkins(pcadata, n = nrow(pcadata)-1)
hopkins.data

# The Hopkins statistic is used to assess the clustering tendency of a data set by measuring the probability that a given data set is generated from a uniform distribution. The Hopkins value for this data set is 0.1908455. This indicates that the data set is clusterable.

#######################
### K - Ortalamalar ###
#######################

#############################################
### Determination of the Cluster Number k ###
#############################################

####################
### Elbow Method ###
####################

# The elbow method is a technique used to determine the optimal number of clusters for a k-means clustering analysis. The idea behind the elbow method is to run k-means clustering on the dataset for a range of values of k (number of clusters), and for each value of k calculate the sum of squared distances of each point from its closest centroid (SSE).

# The elbow point is the point on the plot of SSE against the number of clusters (k) where the change in SSE begins to level off, indicating that adding more clusters doesn't improve the model much.

# The steps to perform the elbow method are:

# Select a range of k values, usually from 1 to 10 or the square root of the number of observations in the dataset.
# Run k-means clustering for each k value and calculate the SSE (sum of squared distances of each point from its closest centroid).
# Plot the SSE for each k value.
# The point on the plot where the SSE starts to decrease at a slower rate is the elbow point, and the corresponding number of clusters is the optimal value for k.
# It is important to note that the elbow method is not a definitive method and results may vary depending on the data and the initial conditions, so it's important to compare the results with other techniques such as silhouette scores and visualizing the data.


fviz_nbclust(pcadata , kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Dirsek Yöntemi") 

#################################
### Average Silhouette Method ###
#################################


# The average silhouette method is a technique used to determine the optimal number of clusters for a clustering analysis. It measures the similarity of each point to its own cluster compared to other clusters. The silhouette value of a point is a measure of how similar that point is to other points in its own cluster compared to other clusters.

# The steps to perform the average silhouette method are:
  
# Select a range of k values, usually from 1 to 10 or the square root of the number of observations in the dataset.
# Run clustering algorithm (such as k-means or hierarchical clustering) for each k value
# For each point in the dataset, calculate its silhouette value using the formula: (b-a)/max(a,b) where a is the mean distance to the points in the same cluster, and b is the mean distance to the points in the closest other cluster.
# Calculate the average silhouette value for all points in the cluster.
# Plot the average silhouette value for each k value.
# The k value that corresponds to the highest average silhouette value is the optimal number of clusters.
# It's important to note that the silhouette method is sensitive to the scale and density of the data and the initial conditions, so it's important to compare the results with other techniques such as elbow method and visualizing the data. Also, the silhouette method is only applicable when the number of clusters is not known a priori, and it's not useful when the number of clusters is known.

fviz_nbclust(pcadata, kmeans ,method = "silhouette")+
  labs(subtitle = "Silhouette Grafiği") 

######################
### Gap Statistics ###
######################

# The gap statistic is a technique used to determine the optimal number of clusters for a clustering analysis. It compares the observed within-cluster variation for different values of k with the variation expected under a null reference distribution of the data.

# The steps to perform the gap statistic method are:
  
# Select a range of k values, usually from 1 to 10 or the square root of the number of observations in the dataset.
# Run the clustering algorithm (such as k-means or hierarchical clustering) for each k value and calculate the within-cluster variation Wk.
# Generate B reference datasets by randomly sampling the original data and calculate the within-cluster variation W*k for each dataset.
# Calculate the gap statistic as Gk = E{log(W*k)} - log(Wk)
# Plot the gap statistic for each k value.
# The k value that corresponds to the maximum gap statistic is the optimal number of clusters.

fviz_nbclust(pcadata, kmeans ,nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap Grafiği") 

#  When the Elbow Method graph is analyzed, it can be said that it is not possible to make a definite decision for the number of clusters, but two clusters can be selected. When the Silhouette graph is analyzed, it can be observed that the highest silhouette value is in two clusters. However, 3 clusters can also be tried since there is not much difference between them. The Gap Statistics value also indicates that the most appropriate number of clusters is two.

nb <- NbClust(pcadata , distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "kmeans")
fviz_nbclust(nb) + labs(subtitle ="NbClust Grafiği")

# When the output of the NbClust package was analyzed, it was found that 9 methods suggested 2 clusters and 6 methods suggested 3 clusters. 
# According to majority rule, the best number of cluster is 2. 
# However, both 2 and 3 clusters will be examined for k-means, k-medoids, and hierarchical clustering alhorithms.

###############
### k-means ###
###############

# K-means is a popular clustering algorithm that groups similar observations together (clusters) based on a set of features. The main idea behind k-means is to define spherical clusters where the observations in the same cluster are as similar as possible and observations in different clusters are as dissimilar as possible.

# The steps to perform k-means clustering are:
  
# Select k, the number of clusters, that you want to form in the data.
# Select k random points from the dataset as the initial centroids (cluster center)
# Assign each observation to the cluster whose centroid is closest to it.
# Recalculate the centroids as the mean of all the observations in each cluster.
# Repeat steps 3 and 4 until the cluster assignments no longer change or reach a maximum number of iterations.
# It's important to note that the final clusters may depend on the initial conditions, so it's recommended to run k-means multiple times with different initial centroids, then choose the best solution.
# Also k-means is sensitive to the scale of the data, so it's recommended to scale the data before applying the k-means algorithm.
# K-means is efficient for large datasets, but it's not well suited for non-globular clusters or clusters of different densities.

# After applying the k-means algorithm, the resulting output will be k clusters where each cluster has its own centroid, and each observation will be assigned to the cluster to which it is closest. These clusters can be used for further analysis or interpretation of the data.



####################################
### k-means for 2 cluster number ###
####################################

set.seed(1993)
km_data <- kmeans(pcadata, 2, nstart=25) 
print(km_data) 

# When the result of the k-means clustering with 2 clusters is examined, the followings are founded:

# There are 398 observations in cluster 1, 171 observations in cluster 2.
# Total within cluster sum of squares for clusters are 1121.768 and 1216.540.
# It is best for within cluster sum of squares for each cluster to be closer to each other. In this case, they are very close.
# This clustering result explain %48.5 of the separation.

fviz_cluster(km_data, data = pcadata,
             ellipse.type = "convex", 
             star.plot = TRUE, 
             repel = TRUE, 
             ggtheme = theme_minimal()
) + labs(subtitle = "Two Dimensional Clustering Graphic")


# Separation can be observed only in PC1 dimension.
# Within sum of square of the cluster 2 is much than the cluster 1. The reason of this needs to be the difference between observation numbers of the clusters.
# There is no visible overlap between clusters. 

##################################################
### Cluster Validation of 2 clustered k-means ###
##################################################

###################################
### Internal Cluster Validation ###
###################################

##################
### Silhouette ###
##################

k2m_data <- eclust(pcadata, "kmeans", k = 2, nstart = 25, graph = F)
fviz_silhouette(k2m_data, palette = "jco",
                ggtheme = theme_classic())

# When the graph containing the silhouette values of each observation is examined, it can be seen that some observations in the first cluster which is shown in blue colored, have negative values. This indicates that those observations may have been assigned to the wrong cluster.
# The average silhouette value is 0.49 which can be stated as average.

sil <- k2m_data$silinfo$widths[, 1:3]
neg_sil_index <- which(sil[, 'sil_width']<0)
sil[neg_sil_index, , drop = FALSE]

# The number of observations with negative silhouette values was determined as 10. 
# Observations having negative silhouette values is an important criterion for questioning the validity of clustering.


##################
### Dunn Index ###
##################

km_stats <- cluster.stats(dist(pcadata), k2m_data$cluster)
km_stats$dunn

# The Dunn index takes values ranging from zero to maximum. The best dunn value is the maximum value. 
# The clustering result is very close to zero (0.005768501). This calls into question the success of the clustering.
# These values will be compared later.

####################
### Connectivity ###
####################

connectivity(distance = NULL, k2m_data$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity takes values from 0 to infinity. It should be as small as possible. 
# For this clustering, the value is 64.96498. 
# It will be compared with other clustering results.

#####################
### Accuracy Rate ###
#####################

table(wdbc$Diagnosis,k2m_data$cluster)

# there are 55 observations that clustered false clusters.
# 514 observations are clustered correctly. 

###################################
### External Cluster Validation ###
###################################

############################
### Corrected Rand Index ###
############################

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, k2m_data$cluster)$corrected.rand

# Corrected Rand Index takes values between 0 and 1. It needs to be closer to the 1. In this clustering result, it is 0.6465881. 
# This value will be compared with other clustering results.

########################################
### Meila's Variation of Information ###
########################################

cluster.stats(d = dist(data),diagnosis, k2m_data$cluster)$vi

# Meila's Variation of Informationtakes values between 0 and 1. It needs to be closer to the 0. In this clustering result, it is 0.5687046. 
# This value will be compared with other clustering results.

####################################
### k-means for 3 cluster number ###
####################################

set.seed(1993)
k3m_data <- kmeans(pcadata, 3, nstart=25) 
print(k3m_data) 

# When the result of the k-means clustering with 3 clusters is examined, the followings are founded:

# There are 117 observations in cluster 1, 117 observations in cluster 2, and 335 observations in cluster 3.
# Total within cluster sum of squares for clusters are 620.5682, 455.6488, and 634.0446.
# It is best for within cluster sum of squares for each cluster to be closer to each other. In this case, WSS of cluster 2 is less than the other clusters.
# This clustering result explain %62.3 of the separation.


fviz_cluster(k3m_data, data = pcadata,
             ellipse.type = "convex", 
             star.plot = TRUE, 
             repel = TRUE, 
             ggtheme = theme_minimal()
)

# There is no overlap between clusters.
# Separation can be observed both in PC1 and in PC2 dimensions.
# Within sum of square of the cluster 1 is more than other clusters. 

#################################################
### Cluster Validation of 3 clustered k-means ###
#################################################

###################################
### Internal Cluster Validation ###
###################################

# Internal cluster validation refers to the process of evaluating the quality of a clustering solution using a metric that is based on the characteristics of the data and the clustering solution itself. Unlike external validation, which uses external information such as class labels to evaluate the clustering solution, internal validation relies on the properties of the clusters themselves and the observations within them.

# There are several internal cluster validation metrics that can be used to evaluate the quality of a clustering solution, such as:
  
# Silhouette score
# Dunn index
# Connectivity

# It's important to note that no single metric can provide a definitive answer about the quality of a clustering solution, and it's recommended to use multiple internal cluster validation metrics and compare the results with other techniques such as visualizing the data.
# Also, these metrics are sensitive to the scale and density of the data, so it's important to scale the data before applying these metrics.

##################
### Silhouette ###
##################

km3_data <- eclust(pcadata, "kmeans", k = 3, nstart = 25, graph = F)
fviz_silhouette(km3_data, palette = "jco",
                ggtheme = theme_classic())



# When the graph containing the silhouette values of each observation is examined, it can be seen that some observations in the first cluster which is shown as blue, and in the second cluster which is shown as yellow have negative values.
# The average silhouette value was observed as 0.44. This will be compared with other clustering results.

sil <- km3_data$silinfo$widths[, 1:3]
neg_sil_index <- which(sil[, 'sil_width']<0)
sil[neg_sil_index, , drop = FALSE]

# The number of observations with negative silhouette values is 9 (1 in the first cluster and 8 in the second cluster). 
# Observations having negative silhouette values is an important criterion for questioning the validity of clustering.

###################
### Dunn Index ###
###################


# The Dunn index is a measure used to validate the quality of clustering solutions by comparing the similarity within clusters to the dissimilarity between clusters. It is based on the ratio of the minimum inter-cluster distance to the maximum intra-cluster distance.

# The Dunn index is calculated as follows:
  
# Compute the pairwise distances between all observations in the dataset.
# For each cluster, compute the maximum distance between any two points within the cluster (intra-cluster distance)
# For each pair of clusters, compute the minimum distance between any two points in different clusters (inter-cluster distance)
# Calculate the Dunn index as: D = min(inter-cluster distances) / max(intra-cluster distances)
# A higher Dunn index value indicates that the clusters are more compact and well separated, and thus the clustering solution is considered to be of higher quality. The Dunn index is sensitive to the scale of the data, so it's important to scale the data before applying the method.

# It's important to note that the Dunn index is not a definitive measure and it may not be a reliable indicator for high-dimensional datasets or datasets with non-convex structure, so it's important to compare the results with other clustering validation metrics such as Silhouette score, Calinski-Harabasz index, and Davies-Bouldin index and visualizing the data.


k3m_stats <- cluster.stats(dist(pcadata), km3_data$cluster)
k3m_stats$dunn

# The Dunn index takes values ranging from zero to maximum. The best dunn value is the maximum value. 
# The clustering result is very close to zero (0.01145259). 
# This value will be compared with other clustering results.

####################
### Connectivity ###
####################

# Connectivity is a measure used to assess the quality of clustering solutions by evaluating the degree of connectivity among the observations in a cluster. It is based on the idea that observations in the same cluster should be more similar to each other than to observations in other clusters.

# Connectivity is typically measured by calculating the ratio of the number of observations within a cluster that are closer to each other than to any other observation outside the cluster, to the total number of observations in the cluster.

# There are different ways to calculate connectivity, but a common approach is:

# Compute the pairwise distances between all observations in the dataset.
# For each observation in a cluster, find the closest observation outside the cluster, and calculate the distance between them.
# For each observation in a cluster, find the closest observation within the cluster, and calculate the distance between them.
# Compare the two distances and assign the observation to the cluster it is closer to.
# Repeat the process for all observations in the dataset.
# Calculate the connectivity for each cluster as the ratio of the number of observations that are closer to other observations within the cluster than to any other observation outside the cluster, to the total number of observations in the cluster.
# A higher connectivity value indicates that the observations within a cluster are more similar to each other than to observations outside the cluster, and thus the clustering solution is considered to be of higher quality. Connectivity is sensitive to the distance metric used and the number of clusters.

# It's important to note that Connectivity is not a definitive measure and it may not be a reliable indicator for high-dimensional datasets or datasets with non-convex structure, so it's important to compare the results with other clustering validation metrics such as Silhouette score, Calinski-Harabasz index, and Davies-Bouldin index and visualizing the data.



connectivity(distance = NULL, km3_data$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity takes values from 0 to infinity. It should be as small as possible. 
# For this clustering, the value is 87.85166. 
# It will be compared with other clustering results.

#####################
### Accuracy Rate ###
#####################

table(wdbc$Diagnosis, km3_data$cluster)

# there are 146 observations that clustered false clusters.
# 423 observations are clustered correctly. 

###################################
### External Cluster Validation ###
###################################

# External cluster validation refers to the process of evaluating the quality of a clustering solution using external information such as class labels or ground truth. The idea behind external validation is to compare the clustering solution to the known class labels and measure the degree of correspondence between them.

# There are several external cluster validation metrics that can be used to evaluate the quality of a clustering solution, such as:
  
# Rand index 
# Adjusted Rand index
# Meila's Variation of Information

# It's important to note that external validation assumes that the class labels are accurate and that the clustering solution should match them closely. However, In real-world applications, class labels are often not available or not reliable, and it's recommended to use multiple external cluster validation metrics and compare the results with other techniques such as visualizing the data.

############################
### Corrected Rand Index ###
############################

# The corrected Rand index (CRI) is a measure used to assess the similarity between two different clustering solutions for a given dataset. It is a modified version of the Rand index, which measures the proportion of correctly classified observations in both clustering solutions. The corrected Rand index takes into account the chance agreement between the two clustering solutions, and ranges from -1 to 1.

# The corrected Rand index is calculated as follows:
  
# Compute the pairwise similarity between all observations in the dataset.
# Compare the two clustering solutions and count the number of pairs of observations that are in the same cluster in both solutions (a), in different clusters in both solutions (b), in the same cluster in one solution and different clusters in the other solution (c), and in different clusters in one solution and the same cluster in the other solution (d).
# Calculate the corrected Rand index as: CRI = (a + b) / (a + b + c + d) - ((a + c) * (a + d) + (b + c) * (b + d)) / ( (a + b + c + d) * (a + b + c + d - 1) )
# A higher corrected Rand index value indicates that the two clustering solutions are more similar, and thus the clustering solution is considered to be of higher quality. The corrected Rand index is sensitive to the number of clusters and the number of observations in the dataset.

# It's important to note that the corrected Rand index is not a definitive measure and it may not be a reliable indicator for high-dimensional datasets or datasets with non-convex structure, so it's important to compare the results with other clustering validation metrics such as Silhouette score, Calinski-Harabasz index, and Davies-Bouldin index, and visualizing the data.

diagnosis <- ifelse(wdbc$Diagnosis == "M",1,2 )
cluster.stats(d = dist(data),diagnosis, km3_data$cluster)$corrected.rand

# Corrected Rand Index takes values between 0 and 1. It needs to be closer to the 1. In this clustering result, it is 0.4985402. 
# This value will be compared with other clustering results.

########################################
### Meila's Variation of Information ###
########################################


# Meila's variation of information (VI) is a measure used to assess the similarity between two different clustering solutions for a given dataset. It is based on the idea that the similarity between two clustering solutions can be measured by the amount of information that is gained or lost when going from one clustering solution to the other.

# VI compares the entropy of each clustering solution, and ranges from 0 to log(n) where n is the number of observations. A lower VI value indicates that the two clustering solutions are more similar.

# The steps to calculate Meila's VI are:

# Compute the contingency matrix that counts the number of observations in the same cluster for both clustering solutions.
# Compute the entropy for each clustering solution using the formula: -sum(p_i * log(p_i)) where p_i is the proportion of observations in the i-th cluster.
# Compute Meila's VI as: VI = H1 + H2 - 2*I where H1, H2 are the entropies of the two clustering solutions, and I is the mutual information between the two clustering solutions.
# It's important to note that Meila's VI is sensitive to the number of clusters and the number of observations in the dataset. Also, VI is not robust to small sample sizes and can be affected by the randomness in the clustering solutions. Therefore, it's important to compare the results with other clustering validation metrics such as Silhouette score, Calinski-Harabasz index, and Davies-Bouldin index, and visualizing the data.



cluster.stats(d = dist(data),diagnosis, km3_data$cluster)$vi

# Meila's Variation of Informationtakes values between 0 and 1. It needs to be closer to the 0. In this clustering result, it is 0.5687046. 
# This value will be compared with other clustering results.

###################
### k - Medoids ###
###################

# K-medoids is a clustering algorithm that is similar to k-means, but instead of using the mean of the observations in each cluster as the centroid, it uses one of the observations in the cluster as the "medoid." The main idea behind k-medoids is to define clusters where the total dissimilarity between observations and the medoid is minimized. The k-medoids algorithm is also known as Partitioning Around Medoids (PAM) algorithm.

# The steps to perform k-medoids clustering are:
  
# Select k, the number of clusters, that you want to form in the data.
# Select k random observations from the dataset as the initial medoids.
# Assign each observation to the cluster whose medoid is closest to it based on a distance metric.
# Recalculate the medoids as the observation in each cluster that minimizes the total dissimilarity to the other observations in the same cluster.
# Repeat steps 3 and 4 until the cluster assignments no longer change or reach a maximum number of iterations.
# It's important to note that k-medoids is more robust to noise and outliers than k-means, it's also more efficient for handling categorical variables. However, k-medoids is more computationally expensive than k-means because it requires the calculation of all pairwise distances between observations at each iteration. Like k-means, k-medoids is sensitive to the initial conditions and it's recommended to run the algorithm multiple times and choose the best solution.

# After applying the k-medoids algorithm, the resulting output will be k clusters where each cluster has its own medoid, and each observation will be assigned to the cluster to which it is closest. These clusters can be used for further analysis or interpretation of the data.




##################################################
### Determining the Optimal Number of Clusters ###
##################################################

fviz_nbclust(pcadata , pam, method = "wss") +
  labs(subtitle = "Elbow Method") 
fviz_nbclust(pcadata, pam ,method = "silhouette")+
  labs(subtitle = "Average Silhouette Method") 
fviz_nbclust(pcadata, pam, method = "gap")+
  labs(subtitle = "Gap Statistics") 

# When the Elbow Method graph is analyzed, it can be said that it is not possible to make a definite decision for the number of clusters, but two clusters can be selected. 
# When the Silhouette graph is analyzed, it can be observed that the highest silhouette value is in two clusters. 
# The Gap Statistics value also indicates that the most appropriate number of clusters is two.


######################################
### k-medoids for 2 cluster number ###
######################################

set.seed(1993)
pam_data <- pam(pcadata,2)
print(pam_data)


# There are 499 observations in cluster 1, 269 observations in cluster 2.

fviz_cluster(pam_data,
             ellipse.type = "convex", 
             repel = TRUE, 
             ggtheme = theme_classic()
)

# No overlap is observed when two and three dimensional graphs are analyzed.
# Just like in the k-means, it is observed that the separation occurs only in the PC1 dimension.
# The variance in the first cluster shown in red color is higher. 

###################################################
### Cluster Validation of 2 clustered k-medoids ###
###################################################

###################################
### Internal Cluster Validation ###
###################################

##################
### Silhouette ###
##################

p2m_data <- eclust(pcadata, "pam", k = 2, nstart = 25, graph = F)
fviz_silhouette(p2m_data, palette = "jco",
                ggtheme = theme_classic())


# When the graph containing the silhouette values of each observation is examined, it can be seen that some observations in the first cluster which is shown in blue colored, have negative values. This indicates that those observations may have been assigned to the wrong cluster.
# The average silhouette value is 0.48 which can be stated as average.

silp <- p2m_data$silinfo$widths[, 1:3]
neg_silp_index <- which(silp[, 'sil_width']<0)
silp[neg_silp_index, , drop = FALSE]

# The number of observations with negative silhouette values was determined as 26. 
# Observations having negative silhouette values is an important criterion for questioning the validity of clustering.

##################
### Dunn Index ###
##################

pam_stats <- cluster.stats(dist(pcadata), p2m_data$cluster)
pam_stats$dunn

# The Dunn index takes values ranging from zero to maximum. The best dunn value is the maximum value. 
# The clustering result is very close to zero (0.01328364). 
# This value will be compared with other clustering results.

####################
### Connectivity ###
####################

connectivity(distance = NULL, p2m_data$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity takes values from 0 to infinity. It should be as small as possible. 
# For this clustering, the value is 87.85166. 
# It will be compared with other clustering results.

################
### Accuracy ###
################

table(wdbc$Diagnosis, p2m_data$cluster)

# there are 42 observations that clustered false clusters.
# 527 observations are clustered correctly. 

###################################
### External Cluster Validation ###
###################################

############################
### Corrected Rand Index ###
############################

cluster.stats(d = dist(data),diagnosis, p2m_data$cluster)$corrected.rand

# Corrected Rand Index takes values between 0 and 1. It needs to be closer to the 1. In this clustering result, it is 0.7241586. 
# This value will be compared with other clustering results.

########################################
### Meila's Variation of Information ###
########################################

cluster.stats(d = dist(data),diagnosis, p2m_data$cluster)$vi

# Meila's Variation of Informationtakes values between 0 and 1. It needs to be closer to the 0. In this clustering result, it is 0.4999193. 
# This value will be compared with other clustering results.


######################################
### k-medoids for 3 cluster number ###
######################################


set.seed(1993)
pam3_data <- pam(pcadata,3)
print(pam3_data)

# There are 159 observations in cluster 1, 19 observations in cluster 2, and 241 observations in cluster 3.


fviz_cluster(pam3_data,
             ellipse.type = "convex", 
             repel = TRUE, 
             ggtheme = theme_classic()
)

# When the cluster graph is analyzed, it can be seen that there is no overlap.
# It can be seen that the separation occurs in both PC1 and PC2 dimensions.
# While the variance in the first cluster shown in red is high, the variance in the third cluster shown in blue is low. 

#################################################
### Cluster Validation of 3 clustered k-means ###
#################################################

###################################
### Internal Cluster Validation ###
###################################

##################
### Silhouette ###
##################

p3m_data <- eclust(pcadata, "pam", k = 3, nstart = 25, graph = F)
fviz_silhouette(p3m_data, palette = "jco",
                ggtheme = theme_classic())

# When the graph containing the silhouette values of each observation is examined, it can be seen that some observations in the first cluster which is shown as blue, and in the second cluster which is shown as yellow have negative values.
# The average silhouette value was observed as 0.39 which will be compared with other clustering results.


silp3 <- p3m_data$silinfo$widths[, 1:3]
neg_silp3_index <- which(silp3[, 'sil_width']<0)
silp3[neg_silp3_index, , drop = FALSE]

# The number of observations with negative silhouette values is 47 (26 in the first cluster, and 21 in the second cluster). 
# Observations having negative silhouette values is an important criterion for questioning the validity of clustering.

###################
### Dunn Indexi ###
###################

pam3_stats <- cluster.stats(dist(pcadata), p3m_data$cluster)
pam3_stats$dunn

# The Dunn index takes values ranging from zero to maximum. The best dunn value is the maximum value. 
# The clustering result is very close to zero (0.004579698). 
# This value will be compared with other clustering results.

####################
### Connectivity ###
####################

connectivity(distance = NULL, p3m_data$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity takes values from 0 to infinity. It should be as small as possible. 
# For this clustering, the value is 109.0265. 
# It will be compared with other clustering results.

################
### Accuracy ###
################

table(wdbc$Diagnosis, p3m_data$cluster)

# there are 57 observations that clustered false clusters.
# 512 observations are clustered correctly. 

###################################
### External Cluster Validation ###
###################################

############################
### Corrected Rand Index ###
############################

cluster.stats(d = dist(data),diagnosis, p3m_data$cluster)$corrected.rand

# Corrected Rand Index takes values between 0 and 1. It needs to be closer to the 1. In this clustering result, it is 0.3972545. 
# This value will be compared with other clustering results.

########################################
### Meila's Variation of Information ###
########################################

cluster.stats(d = dist(data),diagnosis, p3m_data$cluster)$vi

# Meila's Variation of Informationtakes values between 0 to infinity. It needs to be closer to the 0. In this clustering result, it is 1.013189. 
# This value will be compared with other clustering results.

###############################
### Hierarchical Clustering ###
###############################


# Hierarchical Clustering is a method of clustering in which the objects are organized into a tree-like structure called a dendrogram. The main idea behind hierarchical clustering is to start with each object as a separate cluster and then combine them into larger clusters iteratively based on their similarity. There are two main types of hierarchical clustering: Agglomerative and Divisive.

# Agglomerative hierarchical clustering:
  
# Start with each object as a separate cluster
# Find the two most similar clusters and combine them into a new cluster
# Repeat step 2 until all objects are in the same cluster
# Divisive hierarchical clustering:
  
# Start with all objects in the same cluster
# Divide the largest cluster into two smaller clusters based on their similarity
# Repeat step 2 until each object forms its own cluster
# Hierarchical clustering can be represented by a dendrogram, which is a tree-like structure that shows the hierarchy of clusters and the relations between them. The dendrogram can be cut at a certain height to obtain a flat clustering solution with a specific number of clusters.

# It's important to note that hierarchical clustering is sensitive to the scale and density of the data, so it's important to scale the data before applying the method. Also, the choice of linkage method (single, complete, average, etc) is important and it affects the final clustering. Additionally, hierarchical clustering is computationally expensive for large datasets and it's not suitable for handling high-dimensional data.


###############
### Ward D2 ###
###############

# Ward's method is an agglomerative linkage method used in hierarchical clustering. It is based on the idea of minimizing the variance of the distances between the observations in the new cluster and the cluster centroid. This linkage method aims to minimize the total within-cluster variance of the new cluster formed by merging two smaller clusters.

# The steps to perform hierarchical clustering using Ward's method are:

# Start with each observation as a separate cluster
# Compute the distance matrix between all pairs of clusters.
# Merge the two clusters that have the minimum distance between their centroids and form a new cluster.
# Recalculate the cluster centroid
# Repeat steps 2 to 4 until all observations are in the same cluster.
# Ward's linkage method is sensitive to the scale of the variables, so it's recommended to standardize the variables before applying the method. Ward's linkage method tends to create compact and spherical clusters, and it's more efficient for handling datasets with small number of observations and variables.

# It's important to note that the choice of linkage method affects the final clustering and it's recommended to compare the results with other linkage methods and visualizing the data. Also, the hierarchical clustering is sensitive to the scale and density of the data, so it's important to scale the data before applying the method, especially when using Ward's linkage method.



dist_euc <- dist(pcadata, method="euclidean")
dist_man <- dist(pcadata, method="manhattan")


hc_e <- hclust(d=dist_euc, method="ward.D2")
fviz_dend(hc_e,cex=.5) 

hc_m <- hclust(d=dist_man, method="ward.D2")
fviz_dend(hc_m,cex=.5) 

###########################
### cophenetic distance ###
###########################

# The cophenetic distance is a measure used in hierarchical clustering to evaluate the similarity between two observations in the dendrogram produced by the clustering algorithm. It is defined as the distance between two observations in the original data space at the level in the dendrogram where they first merge into the same cluster.

# The cophenetic distance is calculated as follows:
  
# Perform hierarchical clustering on the data to produce a dendrogram
# For each pair of observations, find the level in the dendrogram where they first merge into the same cluster.
# Compute the distance between the two observations in the original data space.
# Repeat steps 2 and 3 for all pairs of observations.
# The cophenetic distance is used to evaluate the quality of the clustering solution by comparing it to the original data space. A high correlation between the cophenetic distance and the original distance between observations in the data space indicates that the clustering solution is preserving the structure of the data well.

# It's important to note that the cophenetic distance is computationally expensive for large datasets, also the linkage method used in the hierarchical clustering affects the final clustering and it's recommended to compare the results with other linkage methods and visualizing the data.
coph_e <- cophenetic(hc_e)
cor(dist_euc,coph_e)

coph_m <- cophenetic(hc_m)
cor(dist_man,coph_m)

# When the correlation between Cophenetic and distance matrix is examined, it is observed that hierarchical clustering with euclidean distance gives better results.


nbward <- NbClust(pcadata , distance = "euclidean", min.nc = 2,
                  max.nc = 9, method = "ward.D2")
fviz_nbclust(nbward) + labs(subtitle ="NbClust Graph")

# When the output of the NbClust package was analyzed, it was found that 7 methods suggested 2 clusters and 5 methods suggested 3 clusters. 
# According to majority rule, the best number of cluster is 2. 
# However, both 2 and 3 clusters will be examined for k-means, k-medoids, and hierarchical clustering alhorithms.

##############
###  k = 2 ###
##############

grupward2 <- cutree(hc_e, k = 2)
grupward2
table(grupward2)

# There are 180 observations in cluster 1, 389 observations in cluster 2.

fviz_dend(hc_e, k = 2, 
          cex = 0.5, 
          color_labels_by_k = TRUE, 
          rect = TRUE )


fviz_cluster(list(data = pcadata, cluster = grupward2),
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# When the cluster graph is analyzed, overlap can be observed.
# It can be seen that the separation occurs only in PC1.
# While the variance in the first cluster shown in red is high, the variance in the second cluster shown in blue is low. 


hc_dataward2 <- eclust(pcadata, "hclust", k = 2, hc_metric = "euclidean",hc_method = "ward.D2", graph = F)


#################################################
### Cluster Validation of 2 clustered Ward.D2 ###
#################################################

###################################
### Internal Cluster Validation ###
###################################

##################
### Silhouette ###
##################

fviz_silhouette(hc_dataward2, palette = "jco",
                ggtheme = theme_classic())

# When the graph containing the silhouette values of each observation is examined, it can be seen that some observations in the first cluster which is shown as blue, and in the second cluster which is shown as yellow have negative values.
# The average silhouette value was observed as 0.48 which will be compared with other clustering results.


silward2 <- hc_dataward2$silinfo$widths[, 1:3]
neg_silward2_index <- which(silward2[, 'sil_width']<0)
silward2[neg_silward2_index, , drop = FALSE]

# The number of observations with negative silhouette values is 24 (22 in the first cluster, and 2 in the second cluster). 
# Observations having negative silhouette values is an important criterion for questioning the validity of clustering.

###################
### Dunn Indexi ###
###################

ward2_stats <- cluster.stats(dist(pcadata), hc_dataward2$cluster)
ward2_stats$dunn

# The Dunn index takes values ranging from zero to maximum. The best dunn value is the maximum value. 
# The clustering result is close to zero (0.02706536). 
# This value will be compared with other clustering results.

####################
### Connectivity ###
####################

connectivity(distance = NULL, hc_dataward2$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity takes values from 0 to infinity. It should be as small as possible. 
# For this clustering, the value is 40.70481. 
# It will be compared with other clustering results.

################
### Accuracy ###
################

table(wdbc$Diagnosis, hc_dataward2$cluster)

# there are 70 observations that clustered false clusters.
# 499 observations are clustered correctly. 

###################################
### External Cluster Validation ###
###################################

############################
### Corrected Rand Index ###
############################

cluster.stats(d = dist(data),diagnosis, grupward2)$corrected.rand

# Corrected Rand Index takes values between 0 and 1. It needs to be closer to the 1. In this clustering result, it is 0.5640062. 
# This value will be compared with other clustering results.

########################################
### Meila's Variation of Information ###
########################################

cluster.stats(d = dist(data),diagnosis, grupward2)$vi

# Meila's Variation of Informationtakes values between 0 to infinity. It needs to be closer to the 0. In this clustering result, it is 0.7082363. 
# This value will be compared with other clustering results.



#############
### k = 3 ###
#############

grupward3 <- cutree(hc_e, k = 3)
table(grupward3)

# There are 104 observations in cluster 1, 76 observations in cluster 2, and 389 observations in cluster 3.


fviz_dend(hc_e, k = 3, 
          cex = 0.5, 
          color_labels_by_k = TRUE, 
          rect = TRUE )


fviz_cluster(list(data = pcadata, cluster = grupward3),
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# When the cluster graph is analyzed, overlap can be observed.
# It can be seen that the separation occurs only in both PC1 and PC2.
# While the variance in the first cluster shown in red is high, the variance in the second cluster shown in green is low. 


hc_dataward3 <- eclust(pcadata, "hclust", k = 3, hc_metric = "euclidean",hc_method = "ward.D2", graph = F)


#################################################
### Cluster Validation of 3 clustered Ward.D2 ###
#################################################

###################################
### Internal Cluster Validation ###
###################################

##################
### Silhouette ###
##################

fviz_silhouette(hc_dataward3, palette = "jco",
                ggtheme = theme_classic())

# When the graph containing the silhouette values of each observation is examined, all clusters have some observations with negative silhouette value.
# The average silhouette value was observed as 0.43 which will be compared with other clustering results.


silward3 <- hc_dataward3$silinfo$widths[, 1:3]
neg_silward3_index <- which(silward3[, 'sil_width']<0)
silward3[neg_silward3_index, , drop = FALSE]

# The number of observations with negative silhouette values is 34 (23 in the first cluster, 2 in the second cluster, and in the third cluster). 
# Observations having negative silhouette values is an important criterion for questioning the validity of clustering.

###################
### Dunn Indexi ###
###################

ward3_stats <- cluster.stats(dist(pcadata), hc_dataward3$cluster)
ward3_stats$dunn

# The Dunn index takes values ranging from zero to maximum. The best dunn value is the maximum value. 
# The clustering result is close to zero (0.03537124). 
# This value will be compared with other clustering results.

####################
### Connectivity ###
####################

connectivity(distance = NULL, hc_dataward3$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity takes values from 0 to infinity. It should be as small as possible. 
# For this clustering, the value is 60.24877. 
# It will be compared with other clustering results.

################
### Accuracy ###
################

table(wdbc$Diagnosis, hc_dataward3$cluster)

# there are 127 observations that clustered false clusters.
# 442 observations are clustered correctly. 

###################################
### External Cluster Validation ###
###################################

############################
### Corrected Rand Index ###
############################

cluster.stats(d = dist(data),diagnosis, grupward3)$corrected.rand

# Corrected Rand Index takes values between 0 and 1. It needs to be closer to the 1. In this clustering result, it is 0.5176894. 
# This value will be compared with other clustering results.

########################################
### Meila's Variation of Information ###
########################################

cluster.stats(d = dist(data),diagnosis, grupward3)$vi

# Meila's Variation of Informationtakes values between 0 to infinity. It needs to be closer to the 0. In this clustering result, it is 0.8605934. 
# This value will be compared with other clustering results.


#######################
### Average linkage ###
#######################

# The average linkage method (also known as UPGMA) is an agglomerative linkage method used in hierarchical clustering. It is based on the idea of minimizing the average distance between observations in the two clusters being merged. The average linkage method is a measure of the dissimilarity between two clusters, defined as the average distance between the points in one cluster and the points in the other.

# The steps to perform hierarchical clustering using average linkage method are:
  
# Start with each observation as a separate cluster
# Compute the distance matrix between all pairs of clusters.
# Merge the two clusters that have the minimum average distance between their observations and form a new cluster.
# Repeat steps 2 and 3 until all observations are in the same cluster.
# The average linkage method is sensitive to the scale of the variables, so it's recommended to standardize the variables before applying the method. Average linkage method tends to create elongated and non-compact clusters, and it's more efficient for handling datasets with small number of observations and variables.

# It's important to note that the choice of linkage method affects the final clustering and it's recommended to compare the results with other linkage methods and visualizing the data. Also, the hierarchical clustering is sensitive to the scale and density of the data, so it's important to scale the data before applying the method, especially when using average linkage method.

hc_e2 <- hclust(d=dist_euc, method="average")
fviz_dend(hc_e2,cex=.5) 

hc_m2 <- hclust(d=dist_man, method="average")
fviz_dend(hc_m2,cex=.5) 

###########################
### Cophenetic Distance ###
###########################

coph_e2 <- cophenetic(hc_e2)
cor(dist_euc,coph_e2)

coph_m2 <- cophenetic(hc_m2)
cor(dist_man,coph_m2)

# When the correlation between Cophenetic and distance matrix is examined, it is observed that hierarchical clustering with euclidean distance gives better results.


nbaverage <- NbClust(pcadata , distance = "euclidean", min.nc = 2,
                     max.nc = 9, method = "average")
fviz_nbclust(nb) + labs(subtitle ="NbClust Graph")

# When the output of the NbClust package was analyzed, it was found that 9 methods suggested 2 clusters and 6 methods suggested 3 clusters. 
# According to majority rule, the best number of cluster is 2. 
# However, both 2 and 3 clusters will be examined for k-means, k-medoids, and hierarchical clustering alhorithms.


#############
### k = 3 ###
#############

grupav3 <- cutree(hc_e2, k = 3)

table(grupav3)

# There are 23 observations in cluster 1, 541 observations in cluster 2, and 5 observations in cluster 3. It can easily be seen that clusters are unbalanced.


fviz_dend(hc_e2, k =3, 
          cex = 0.5, 
          color_labels_by_k = TRUE, 
          rect = TRUE )


fviz_cluster(list(data = pcadata, cluster = grupav3),
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# When the cluster graph is analyzed, overlap can be observed.
# It can be seen that the separation occurs only in both PC1 nad PC2.
# While the variance in the second cluster shown in green is high, the variance in the third cluster shown in blue is low. 


hc_datav3 <- eclust(pcadata, "hclust", k = 3, hc_metric = "euclidean",hc_method = "average", graph = F)


#################################################
### Cluster Validation of 3 clustered Average ###
#################################################

###################################
### Internal Cluster Validation ###
###################################

##################
### Silhouette ###
##################

fviz_silhouette(hc_datav3, palette = "jco",
                ggtheme = theme_classic())

# When the graph containing the silhouette values of each observation is examined, cluster 1 and cluster 2 have observations with negative silhouette value.
# The average silhouette value was observed as 0.43 which will be compared with other clustering results.


silav3 <- hc_datav3$silinfo$widths[, 1:3]
neg_silav3_index <- which(silav3[, 'sil_width']<0)
silav3[neg_silav3_index, , drop = FALSE]

# The number of observations with negative silhouette values is 42 (1 in the first cluster,  and 41 in the third cluster). 
# Observations having negative silhouette values is an important criterion for questioning the validity of clustering.

###################
### Dunn Index ###
################### 


av3_stats <- cluster.stats(dist(pcadata), hc_datav3$cluster)
av3_stats$dunn

# The Dunn index takes values ranging from zero to maximum. The best dunn value is the maximum value. 
# The clustering result is close to zero (0.07186554). 
# This value will be compared with other clustering results.

####################
### Connectivity ###
####################

connectivity(distance = NULL, hc_datav3$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity takes values from 0 to infinity. It should be as small as possible. 
# For this clustering, the value is 30.9426. 
# It will be compared with other clustering results.

################
### Accuracy ###
################

table(wdbc$Diagnosis, hc_datav3$cluster)

# there are 193 observations that clustered false clusters.
# 376 observations are clustered correctly. 

###################################
### External Cluster Validation ###
###################################

############################
### Corrected Rand Index ###
############################

cluster.stats(d = dist(data),diagnosis, grupav3)$corrected.rand

# Corrected Rand Index takes values between 0 and 1. It needs to be closer to the 1. In this clustering result, it is 0.0565564. 
# This value will be compared with other clustering results.

########################################
### Meila's Variation of Information ###
########################################

cluster.stats(d = dist(data),diagnosis, grupav3)$vi

# Meila's Variation of Informationtakes values between 0 to infinity. It needs to be closer to the 0. In this clustering result, it is 0.7959572. 
# This value will be compared with other clustering results.

#############
### k = 2 ###
#############

grupav2 <- cutree(hc_e2, k = 2)
table(grupav2)

# There are 23 observations in cluster 1, 541 observations in cluster 2, and 5 observations in cluster 3. It can easily be seen that clusters are unbalanced.


fviz_dend(hc_e2, k = 2, 
          cex = 0.5, 
          color_labels_by_k = TRUE, 
          rect = TRUE )


fviz_cluster(list(data = pcadata, cluster = grupav2),
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# When the cluster graph is analyzed, overlap can be observed.
# It can be seen that the separation occurs only in PC1.
# While the variance in the second cluster shown in green is high, the variance in the first cluster shown in blue is low. 


hc_datav2 <- eclust(pcadata, "hclust", k = 2, hc_metric = "euclidean",hc_method = "average", graph = F)

#################################################
### Cluster Validation of 3 clustered Average ###
#################################################

###################################
### Internal Cluster Validation ###
###################################

##################
### Silhouette ###
##################

fviz_silhouette(hc_datav2, palette = "jco",
                ggtheme = theme_classic())

# When the graph containing the silhouette values of each observation is examined, cluster 2 has observations with negative silhouette value.
# The average silhouette value was observed as 0.54 which will be compared with other clustering results.


silav2 <- hc_datav2$silinfo$widths[, 1:3]
neg_silav2_index <- which(silav2[, 'sil_width']<0)
silav2[neg_silav2_index, , drop = FALSE]

# The number of observations with negative silhouette values in cluster 2 is 29. 
# Observations having negative silhouette values is an important criterion for questioning the validity of clustering.

##################
### Dunn Index ###
##################

av2_stats <- cluster.stats(dist(pcadata), hc_datav2$cluster)
av2_stats$dunn

# The Dunn index takes values ranging from zero to maximum. The best dunn value is the maximum value. 
# The clustering result is close to zero (0.063734). 
# This value will be compared with other clustering results.

####################
### Connectivity ###
####################

connectivity(distance = NULL, hc_datav2$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity takes values from 0 to infinity. It should be as small as possible. 
# For this clustering, the value is 20.61224. 
# It will be compared with other clustering results.

################
### Accuracy ###
################

table(wdbc$Diagnosis, hc_datav2$cluster)

# there are 189 observations that clustered false clusters.
# 380 observations are clustered correctly. 


###################################
### External Cluster Validation ###
###################################

############################
### Corrected Rand Index ###
############################

cluster.stats(d = dist(data),diagnosis, grupav2)$corrected.rand

# Corrected Rand Index takes values between 0 and 1. It needs to be closer to the 1. In this clustering result, it is 0.06091674. 
# This value will be compared with other clustering results.

########################################
### Meila's Variation of Information ###
########################################

cluster.stats(d = dist(data),diagnosis, grupav2)$vi

# Meila's Variation of Informationtakes values between 0 to infinity. It needs to be closer to the 0. In this clustering result, it is 0.7468855. 
# This value will be compared with other clustering results.

##########################################################################################################################################



##############################
### Model Based Clustering ###
##############################


# Model-based clustering is a method of clustering in which a probabilistic model is fit to the data, and the clusters are defined as the parameters of the model. The main idea behind model-based clustering is to assume that the data is generated by a certain probability distribution, and the clusters correspond to different modes of that distribution.

# There are several types of model-based clustering methods such as:
  
# Gaussian Mixture Model (GMM): assumes that the data is generated by a mixture of Gaussian distributions, and estimates the parameters of the distributions, such as means and covariances, to define the clusters. I will use this version in this analysis.
# Latent Dirichlet Allocation (LDA): a generative probabilistic model used to classify text in natural language processing and information retrieval. It assumes that each document is a mixture of topics and each topic is a mixture of words.
# Hidden Markov Model (HMM): a statistical model used to predict a sequence of hidden states from a sequence of observations. It can be used for clustering sequences of data.
# Model-based clustering methods have some advantages over traditional clustering methods, such as the ability to model complex data distributions and handle missing data. However, it's also sensitive to the initial conditions and the number of clusters and it's computationally expensive for large datasets.

# It's important to note that model-based clustering methods require assumptions about the underlying data distribution, and the choice of model affects the final clustering, so it's recommended to compare the results with other clustering methods and visualizing the data.




mc <- Mclust(pcadata, G = 2 )
summary(mc)
mc$classification

# OBservation numbers for each clusters is as follows:
# C1 : 253
# C2 : 316

fviz_mclust(mc, "BIC", palette = "jco")

# En iyi model olarak VVI parametresi çıkmaktadır. VVV; hacmi ve şekli değişik, yönelimi eşit anlamına gelmektedir.

fviz_mclust(mc, "classification", geom = "point",
            pointsize = 1.5, palette = "jco")

# When the clustering graph is examined, it is observed that there are no overlaps.
# Blue dots are easily visible on the far right of the PC1 length. This can be interpreted as an interesting result.
# Separation occurred only in the PC1 dimension.

fviz_mclust(mc, "uncertainty", palette = "jco",pos = FALSE)

# Observations with larger points in the uncertainty graph indicate that the clustering results are more uncertain.
# It can be observed that uncertainty increases between two clusters which is not suprising.

#####################################################
### Cluster Validation for Model Based Clustering ###
#####################################################

##################
### Silhouette ###
##################


mc_stats <- cluster.stats(dist(pcadata), mc$classification)
mc_stats[c("avg.silwidth")]

# Average silhouette score is 0.4125059.

##################
### Dunn Index ###
##################

mc_stats$dunn

# The Dunn index takes values ranging from zero to maximum. The best dunn value is the maximum value. 
# The clustering result is close to zero (0.002348756). 
# This value will be compared with other clustering results.

####################
### Connectivity ###
####################

connectivity(distance = NULL, mc$classification, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity takes values from 0 to infinity. It should be as small as possible. 
# For this clustering, the value is 83.61869. 
# It will be compared with other clustering results.

################
### Accuracy ###
################

table(wdbc$Diagnosis, mc$classification)

# there are 77 observations that clustered false clusters.
# 492 observations are clustered correctly. 


###################################
### External Cluster Validation ###
###################################

############################
### Corrected Rand Index ###
############################

cluster.stats(d = dist(data),diagnosis, mc$classification)$corrected.rand

# Corrected Rand Index takes values between 0 and 1. It needs to be closer to the 1. In this clustering result, it is 0.5308146. 
# This value will be compared with other clustering results.

########################################
### Meila's Variation of Information ###
########################################

cluster.stats(d = dist(data),diagnosis, mc$classification)$vi

# Meila's Variation of Informationtakes values between 0 to infinity. It needs to be closer to the 0. In this clustering result, it is 0.7523911. 
# This value will be compared with other clustering results.



################################
### Density-Based Clustering ###
################################

# Density-based clustering is a type of clustering algorithm that groups together data points that are closely packed together, while separating those that are more sparsely distributed. The main idea behind density-based clustering is to identify regions in the feature space where the data points are dense, and then to extract clusters based on these regions.

# One commonly used density-based clustering algorithm is DBSCAN (Density-Based Spatial Clustering of Applications with Noise). DBSCAN groups together data points that are close to each other based on a distance measure and a density threshold. It defines clusters as dense regions of points that are separated from other dense regions by regions of lower point density.

# Another example of density-based clustering is HDBSCAN (Hierarchical Density-Based Spatial Clustering of Applications with Noise) which is an extension of DBSCAN algorithm, it can discover clusters of varying densities and shapes, and it can also discover clusters with different numbers of points, and it is less sensitive to parameter tuning.

# Density-based clustering is useful for data sets that contain clusters of different shapes and sizes, and for data sets with noise and outliers.




# In density-based clustering, the number of clusters does not need to be predetermined, but the values of MinPts and eps do.
# The eps parameter defines the radius of the neighbors around a point x. This is called the epsilon neighborhood of x.
# The MinPts parameter is the minimum number of neighbors within the "eps" radius.
# KNN distplot can be used to determine these values.

kNNdistplot(pcadata, k = 10)
abline(h = 0.6, lty = 2)

# k stands for MinPts. After several trials, 5 was decided upon.
# When analyzing the kNNdisplot, just like the Elbow Method, the point where the line makes an "elbow" should be determined. This point should be chosen as the eps value.
# After various trials, the most appropriate value was decided to be 0.6. 

db <- fpc::dbscan(pcadata, eps = 0.6, MinPts = 10)
print(db)

# Density-based clustering divided the dataset into two clusters. The output shows a total of 96 noise values. 
# There are 58 border points in the first cluster and 27 in the second cluster.
# There are 49 seed points in the first cluster and 339 seed points in the second cluster.


fviz_cluster(db, data = pcadata, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

# When the graph is examined, it can be seen that the element difference between the clusters is small. The excess of noise values is also noteworthy.

#######################################################
### Cluster Validation for Density Based Clustering ###
#######################################################

##################
### Silhouette ###
##################

db_stats <- cluster.stats(dist(pcadata), db$cluster)
db_stats[c("avg.silwidth")]

# Average silhouette score for density based clustering is 0.3943054.

##################
### Dunn Index ###
##################

db_stats$dunn

# The Dunn index takes values ranging from zero to maximum. The best dunn value is the maximum value. 
# The clustering result is close to zero (0.00515391). 
# This value will be compared with other clustering results.

####################
### Connectivity ###
####################

connectivity(distance = NULL, db$cluster, Data = pcadata, neighbSize = 20,
             method = "euclidean")

# Connectivity takes values from 0 to infinity. It should be as small as possible. 
# For this clustering, the value is 160.8934. 
# It will be compared with other clustering results.

################
### Accuracy ###
################

table(wdbc$Diagnosis, db$cluster)

# there are 142 observations are not clustered correctly.
# 427 observations are clustered correctly. 


###################################
### External Cluster Validation ###
###################################

############################
### Corrected Rand Index ###
############################

cluster.stats(d = dist(data),diagnosis, db$cluster)$corrected.rand

# Corrected Rand Index takes values between 0 and 1. It needs to be closer to the 1. In this clustering result, it is 0.48. 
# This value will be compared with other clustering results.

########################################
### Meila's Variation of Information ###
########################################

cluster.stats(d = dist(data),diagnosis, db$cluster)$vi

# Meila's Variation of Informationtakes values between 0 to infinity. It needs to be closer to the 0. In this clustering result, it is NA.
# This value will be compared with other clustering results.


#########################################################################################################################################################


##########################
### Cluster Validation ###
##########################


clustervalid <- data.frame( Clustering.Algorithm = c("2k-means", "3k-means", "2k-medoids", "3k-medoids", "Ward.D2-2", "Ward.D2-3", "Average-5", "Average-2", "Model.Based", "Density.Based"),
                               Cluster.Number = c(2,3,2,3,2,3,5,2,2,3),
                               Overlap = c("Az", "Çok", "Az", "Çok", "Az", "Çok", "Çok", "Az", "Yok", NA),
                               Negative.Silhouette.Number = c(10,9,26,47,24,30,52,29, NA, NA),
                               Average.Silhouette.Number = c(0.49, 0.44, 0.48, 0.36,0.48,0.48,0.42,0.54, 0.41,0.14),
                               Dunn.Index = c(0.005,0.011, 0.013, 0.004, 0.02, 0.035, 0.029, 0.063, 0.002, 0.019),
                               Connectivity = c(64.96, 87.85, 50.08, 109.02, 40.70, 60.24, 68.88, 20.61, 83.61, 117.14),
                               Rand = c(0.64, 0.49, 0.72, 0.39, 0.56, 0.51, 0.44, 0.60, 0.53, 0.09),
                               VI = c(0.56, 0.93, 0.49, 1.013189, 0.70, 0.86, 0.80, 0.74, 0.75, NA)
)


##############################################
### Determining the best Clustering Result ###
##############################################

##################
### Silhouette ###
##################

# Average Silhouette value should be maximum. It was aimed to see in which clustering this value is maximum.


max(clustervalid["Average.Silhouette.Number"])
clustervalid[clustervalid["Average.Silhouette.Number"] == 0.54] [1]

ggplot(clustervalid, aes(x = Average.Silhouette.Number, y = Clustering.Algorithm )) +
  geom_bar(stat = "identity", width = 0.1, color="burlywood4", fill = "burlywood") +
  theme_minimal()+
  labs(title =  "Average Silhouette Scores")+
  xlab("Average Silhouette Score") +
  ylab("Clustering Algorithm") +
  theme(axis.text.y  = element_text(angle=360, vjust=.5, hjust=1))

# The clustering with the highest average silhouette value is the Average linkage method in Hierarchical Clustering. The number of cluster is two.

############
### Dunn ###
############


ggplot(clustervalid, aes(x = Dunn.Index, y = Clustering.Algorithm ))  +
  geom_bar(stat = "identity", width = 0.1, color="burlywood4", fill = "burlywood") +
  theme_minimal()+
  labs(title =  "Average Silhouette Scores")+
  xlab("Dunn Score") +
  ylab("Clustering Algorithm") +
  theme(axis.text.y  = element_text(angle=360, vjust=.5, hjust=1))

# The clustering with the highest Dunn value is the Average linkage method in hierarchical clustering. The number of clusters is two.

####################
### Connectivity ###
####################

# Connectivity Açıklaması GİR

ggplot(clustervalid, aes(x = Connectivity, y = Clustering.Algorithm ))  +
  geom_bar(stat = "identity", width = 0.1, color="burlywood4", fill = "burlywood") +
  theme_minimal()+
  labs(title =  "Connectivity Scores")+
  xlab("Connectivity Score") +
  ylab("Clustering Algorithm") +
  theme(axis.text.y  = element_text(angle=360, vjust=.5, hjust=1))

# State which one is the best

######################
### Corrected Rand ###
######################

# Bölümlenmiş küme ile harici referans değeri arasında uyum niceliği olarak düzeltilmiş Rand İndeksi değerleri kullanılabilir.
# -1 (uyum yok) ile 1 (mükemmel uyum) arasında değer alır.

ggplot(clustervalid, aes(x = Rand, y = Clustering.Algorithm )) +
  geom_bar(stat = "identity", width = 0.1, color="burlywood4", fill = "burlywood") +
  theme_minimal()+
  labs(title =  "Corrected Rand Scores")+
  xlab("Corrected Rand Score") +
  ylab("Clustering Algorithm") +
  theme(axis.text.y  = element_text(angle=360, vjust=.5, hjust=1))

# state which one is the best

###################
### Meila's VI ###
###################

# Meila's explanation 

ggplot(clustervalid, aes(x = VI , y = Clustering.Algorithm )) +
  geom_bar(stat = "identity", width = 0.1, color="burlywood4", fill = "burlywood") +
  theme_minimal()+
  labs(title =  "Meila's VI Scores")+
  xlab("Meila's VI Score") +
  ylab("Clustering Algorithm") +
  theme(axis.text.y  = element_text(angle=360, vjust=.5, hjust=1))

# State which one is best

################################################################################

##########################
### Clustering Results ###
##########################

# Küme bilgilerini içeren değişken veri setine eklenmiştir.
son_data <- cbind(data, cluster= pam_data$clustering )

# Her değişkenin ortalamaları, küme ortalamaları ile karşılaştırılmak istenilmiştir.
summary(data)
aggregate(data, by=list(cluster=pam_data$clustering), mean) 

################################################
# 1. Küme İçin:               ## 2 Küme için: ##
################################################
# Radius Yüksek,              ## Ortalama     ##
# Texture Yüksek,             ## Ortalama     ##  
# Perimeter Yüksek,           ## Az           ##
# Area Yüksek,                ## Az           ##
# Smoothness Yüksek,          ## Az           ##
# Compactness Yüksek,         ## Az           ##
# Concavity Yüksek,           ## Az           ##
# Concave Points Yüksek,      ## Ortalama     ##
# Symmetry Yüksek,            ## Ortalama     ##
# Fractal Dimension Ortalama. ## Ortalama     ##
################################################

# Raporu daha fazla uzatmamak düşüncesiyle PC1 ve PC2’nin temsil ettiği ikişer değişken seçilerek her bir küme için bu değişkenlerin birbiri ile olan ilişkisinin incelendiği iki adet grafik çizdirilmiştir.

ggplot(son_data, aes(area, radius))+
  geom_point(color = "darkblue") +
  facet_grid(rows = vars(cluster))

# Birinci küme için Radius için 8, Area için 250 bandından başlayıp her iki değerin maksimum değerlerine ulaşan pozitif bir ilişki gözlemlenmiştir. İkinci küme için her iki değişkende de 0'dan başlayıp Area için 1000'de, Radius için 17'de biten pozitif bir ilişki gözlemlenmiştir.

ggplot(son_data, aes(fractal.dimension, smoothness))+
  geom_point(color = "darkolivegreen4") +
  facet_grid(rows = vars(cluster))

# Birinci küme için hem Fractal dimension değişkeninde, hem de Smoothness geniş bir aralıkta yayılım gözlemlenmekte.  İkinci küme için hem Fractal dimension değişkeninde, hem de Smoothness geniş bir aralıkta yayılım gözlemlenmekte. Bunun sebebinin ayrışmanın yalnızca PC1 değişkeninde gerçekleşmiş olması olabilir.

describeBy(wdbc[3:10], group = wdbc[2])
