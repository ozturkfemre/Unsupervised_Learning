## Unsupervised Learning Project

Breast cancer is a type of cancer that forms in the breast tissue. It usually begins in the cells of the milk-producing glands or ducts of the breast and can spread to other parts of the body if left untreated. It is the most common cancer in women worldwide, but it can also occur in men. The process by which cells turn into cancer cells is complex and can be influenced by a variety of factors, including genetic mutations, environmental exposures, and lifestyle factors. In general, cancer cells develop when there are changes, or mutations, in the genes that control cell growth and division. These mutations can cause cells to grow and divide uncontrollably, leading to the formation of a tumor. In this process, I will try to cluster cells as malignant and benign.

### Aim of the Project

In this project, I compared k-means, k-medoids(pam), hierarchical clustering, model based clustering, and density based clustering on the same data set to se which one gives better results. Both [Turkish](https://github.com/ozturkfemre/unsupervisedlearning/blob/main/TR/Denetimsiz_Ogrenme%5BRapor%5D/denetimsizistatistikselogrenme.Rmd) and [English](https://github.com/ozturkfemre/unsupervisedlearning/blob/main/ENG/Unsupervised_Learning%5BReport%5D/report-unsupervisedlearning.Rmd) .Rmd and .R files, and Python notebook are available.

### Dataset Information

The [dataset](https://github.com/ozturkfemre/unsupervisedlearning/blob/main/dataset/wdbc.data) is taken from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+(diagnostic)). The dataset contains information about tumor cells. The task in this project was to extract the variable containing the information labeled as benign or malignant from the dataset and cluster the tumors as benign or malignant using clustering algorithms. There are 569 observations and 32 variables in the dataset. However, some variables are mean of the other variables. Thus, these variables are removed from the dataset. Moreover, ID and variable about class information is also removed. Class variable is used to evaluate performance of the clustering algorithms.

### Study

In this study, dataset is clustered using k-mean, k-medoids(pam), hierarchical clustering, model based clustering, and density based clustering algorithms. Also PCA is applied to the dataset. Both algorithms are explained in detailed.

### Data Analysis

The descriptive statistics of the data set were analysed and the data analysis section was started. Then, some inferences were made with data visualisation. Finally, correlation analysis was performed.

### Principle Component Analysis

In addition to improving the clustering results and dimension reduction, PCA was applied to the dataset due to the high correlation between pairs of variables. Both stats package and psych package were used for PCA.

### Clustering Algorithms

#### k-means

At the beginning of this section, elbow, silhouette, gap statistical methods were applied to decide the number of clusters. Using the NbClust function in the NbClust package, in addition to these three cluster number determination methods, 30 other methods were proposed. The k-means clustering algorithm was performed with the two different cluster numbers suggested by these methods and the outputs of the models were analysed.

#### k-medoids(pam)

At the beginning of this section, elbow, silhouette, gap statistical methods were applied to decide the number of clusters. The pam clustering algorithm was performed with the two different cluster numbers suggested by these methods and the outputs of the models were analysed.

#### Hierarchial Clustering

In this section, hierarchical clustering was performed with average linkage and Ward.D2 methods. Cophenetic distance was used to decide the distance metric. Then, 30 methods were used to determine the number of clusters. Different clustering was performed for 2 different cluster numbers suggested by these methods.

#### Model Based Clustering

Model-based clustering GMM method was used. Since the number of clusters does not need to be pre-determined, it is proceeded in a different order from the previous sections. After examining the model parameters, this section is completed by examining the model output and graph.

#### Density Based Clustering

In density-based clustering, the number of clusters does not need to be predetermined. However, Minpts, i.e. the minimum number of neighbours within the "eps" radius, needs to be determined in advance. For this reason, kNNdistplot was drawn and this parameter was decided. Then the outputs and graphs of the model were analysed.

### Cluster Validation

At the begining of this section, clValid function in the clValid package is used. This function performs clustering with the given clustering methods and recommends the most appropriate clustering algorithm and number of clusters. This function is used for three validity criteria. These criteria are internal and external cluster validity and clustering stability. Then, silhouette, dunn, Rand and Meila's Variation of Information metrics of all models were analysed to decide on the best clustering algorithm. In addition to these metrics, the accuracy rate was calculated by creating a confusion matrix with the values of the class variable. All these metrics were collected in a table and graph and the best model was decided by various evaluations.

### The Best Algorithm

In this section, clustering was performed again with the selected algorithm. The values of the variables for each cluster are tabulated as follows.

| Variables         | First Cluster | Second Cluster |
|-------------------|---------------|----------------|
| Radius            | High          | Average        |
| Texture           | High          | Average        |
| Perimeter         | High          | Low            |
| Area              | High          | Low            |
| Smoothness        | High          | Low            |
| Compactness       | High          | Low            |
| Concavity         | High          | Low            |
| Concave Points    | High          | Average        |
| Symmetry          | High          | Average        |
| Fractal Dimension | Average       | Average        |

: Clustering Results

Then, malignant and benign clusters were identified as a result of clustering. These clusters were interpreted by drawing the graphs below by selecting two variables represented by PC1 and PC2, which are the two components formed after PCA.

![Variables from PC1](plots/000012.png)

In the graph for M (Malignant Tumor), a positive relationship is observed starting from Radius 8, Area 250 band and reaching the maximum values of both values. For B (Benign Tumor), a positive relationship is observed in both variables starting at 0 and ending at 1000 for Area and 17 for Radius. From this graph, it can be inferred that the areas of the nucleus of malignant tumors are in a wider range, while the areas of benign tumors do not grow much. The radius of benign tumors does not increase beyond 17.

![Variables from PC2](plots/000010.png)

At the end of the analysis, it is concluded that for cluster B (Benign Tumor), both the Fractal dimension variable and the Smoothness variable have a wide range. For cluster M (Malignant Tumor), both the Fractal dimension variable and Smoothness have a wide range. This may be due to the fact that separation only occurs in the PC1 variable. From this, it can be inferred that it may be misleading to make comments according to the variables in PC2 (Fractal Dimension, Smoothness, Compactness, Symmetry) in the analyses to be made to distinguish between Benign or Malignant Tumor.


### Theoretical Information

### Principle Component Analysis

Principal component analysis (PCA) is a technique used to identify patterns in a dataset. It does this by identifying the directions (or "components") in the data that account for the most variation. The first component is the direction in the data that accounts for the most variation, the second component is the direction in the data that accounts for the second most variation, and so on. [1], [2]

Here is a step-by-step explanation of how PCA is calculated:

1.  Standardize the data: The data is transformed so that each variable has a mean of zero and a standard deviation of one. This is done to ensure that all variables are on the same scale.

2.  Compute the covariance matrix: This matrix is calculated to determine the relationship between the variables in the dataset.

3.  Compute the eigenvectors and eigenvalues of the covariance matrix: Eigenvectors represent the directions in the data that account for the most variation, and eigenvalues represent the amount of variation that is accounted for by each eigenvector.

4.  Select the principal components: The eigenvectors with the highest eigenvalues are chosen as the principal components of the dataset.

5.  Transform the data: The original dataset is transformed by projecting it onto the principal components, resulting in a new dataset with reduced dimensionality.

6.  Interpret the components: The principal components are interpreted in terms of the original variables to understand the underlying patterns in the data.

Applying PCA to the dataset before clustering has several advanteges that can be listed as follows:

1.  Dimensionality Reduction: PCA can be used to reduce the number of features in a high-dimensional dataset, which can help improve the performance of a clustering algorithm. By reducing the dimensionality, PCA can also help to reduce noise and eliminate multicollinearity in the data, making it easier to interpret the results of a clustering analysis. [3]

2.  Visualization: PCA can be used to visualize high-dimensional datasets in two or three dimensions, making it easier to understand the structure of the data and identify clusters. This can be especially useful for large datasets with many features, as it is often difficult to visualize and interpret the results of a clustering analysis in high dimensions.

3.  Speed: Clustering algorithms can be computationally expensive, especially for large datasets. By reducing the dimensionality of the data with PCA, the computational burden of the clustering algorithm can be significantly reduced, making it faster and more computationally efficient. [3]

4.  Improved Clustering Results: PCA can help to enhance the performance of a clustering algorithm by transforming the data into a new coordinate system that better separates the underlying clusters. This can lead to more accurate and meaningful results, especially for datasets with complex structures. [4]

### Clustering Analysis

Clustering analysis is a type of unsupervised machine learning technique that groups similar objects or data points together into clusters based on their similarities in features or attributes. The goal of clustering is to identify underlying patterns or structures in the data without prior knowledge of the labels or classes. The clustering algorithm assigns data points to a specific cluster based on their similarity(distance) to each other, such that data points within the same cluster are more similar(closer) to each other than to those in other clusters. [5]

Before starting clustering analysis, dataset's cluster tendency is measured with Hopkins Statistics. The Hopkins statistic is a measure used to determine the likelihood that a dataset is generated from a uniform distribution, which is useful for determining whether a dataset is suitable for clustering. [6], [7]

The Hopkins statistic is calculated as follows:

1.  Generating a random sample of n points from the dataset, where n is a small number (typically n=50).

2.  Generating a random sample of n points from a uniform distribution, with the same number of dimensions as the dataset.

3.  Calculating the average distance between each point in the dataset sample and its nearest neighbor in the dataset sample (d(data)).

4.  Calculating the average distance between each point in the uniform sample and its nearest neighbor in the uniform sample (d(unif)).

5.  Calculating the Hopkins statistic

A value of Hopkins statistic close to 1 indicates that the dataset is suitable for clustering, while a value close to 0 indicates that the dataset is not suitable for clustering and might have been generated from a uniform distribution.

#### k-means

K-means is a popular clustering algorithm that groups similar observations together (clusters) based on a set of features. The main idea behind k-means is to define spherical clusters where the observations in the same cluster are as similar as possible and observations in different clusters are as dissimilar as possible. [8], [9], [10]

The steps to perform k-means clustering are:

-   Select k, the number of clusters, that you want to form in the data.

-   Select k random points from the dataset as the initial centroids (cluster center)

-   Assign each observation to the cluster whose centroid is closest to it.

-   Recalculate the centroids as the mean of all the observations in each cluster.

-   Repeat steps 3 and 4 until the cluster assignments no longer change or reach a maximum number of iterations.

It's important to note that the final clusters may depend on the initial conditions, so it's recommended to run k-means multiple times with different initial centroids, then choose the best solution. Also k-means is sensitive to the scale of the data, so it's recommended to scale the data before applying the k-means algorithm. K-means is efficient for large datasets, but it's not well suited for non-globular clusters or clusters of different densities. After applying the k-means algorithm, the resulting output is k clusters where each cluster has its own centroid, and each observation is assigned to the cluster to which it is closest. These clusters can be used for further analysis or interpretation of the data.

##### Determination of the Cluster Number k

###### Elbow Method

The elbow method is a technique used to determine the optimal number of clusters for a k-means clustering analysis. The idea behind the elbow method is to run k-means clustering on the dataset for a range of values of k (number of clusters), and for each value of k calculate the sum of squared distances of each point from its closest centroid (SSE). The elbow point is the point on the plot of SSE against the number of clusters (k) where the change in SSE begins to level off, indicating that adding more clusters doesn't improve the model much. [11], [12]

The steps to perform the elbow method are:

-   Select a range of k values, usually from 1 to 10 or the square root of the number of observations in the dataset.

-   Run k-means clustering for each k value and calculate the SSE (sum of squared distances of each point from its closest centroid).

-   Plot the SSE for each k value.

-   The point on the plot where the SSE starts to decrease at a slower rate is the elbow point, and the corresponding number of clusters is the optimal value for k.

###### Average Silhouette Method

The average silhouette method is a technique used to determine the optimal number of clusters for a clustering analysis. It measures the similarity of each point to its own cluster compared to other clusters. The silhouette value of a point is a measure of how similar that point is to other points in its own cluster compared to other clusters.[13], [14]

The steps to perform the average silhouette method are:

1.  Select a range of k values, usually from 1 to 10 or the square root of the number of observations in the dataset.

2.  Run clustering algorithm (such as k-means or hierarchical clustering) for each k value

3.  For each point in the dataset, calculate its silhouette value using the formula: (b-a)/max(a,b) where a is the mean distance to the points in the same cluster, and b is the mean distance to the points in the closest other cluster.

4.  Calculate the average silhouette value for all points in the cluster.

5.  Plot the average silhouette value for each k value.

6.  The k value that corresponds to the highest average silhouette value is the optimal number of clusters.

###### Gap Statistic Method

The gap statistic is a technique used to determine the optimal number of clusters for a clustering analysis. It compares the observed within-cluster variation for different values of k with the variation expected under a null reference distribution of the data. [15]

The steps to perform the gap statistic method are:

1.  Select a range of k values, usually from 1 to 10 or the square root of the number of observations in the dataset.

2.  Run the clustering algorithm (such as k-means or hierarchical clustering) for each k value and calculate the within-cluster variation Wk.

3.  Generate B reference datasets by randomly sampling the original data and calculate the within-cluster variation W\*k for each dataset.

4.  Calculate the gap statistic

5.  Plot the gap statistic for each k value.

6.  The k value that corresponds to the maximum gap statistic is the optimal number of clusters.

#### k-medoids

K-medoids is a clustering algorithm that is similar to k-means, but instead of using the mean of the observations in each cluster as the centroid, it uses one of the observations in the cluster as the "medoid." The main idea behind k-medoids is to define clusters where the total dissimilarity between observations and the medoid is minimized. The k-medoids algorithm is also known as Partitioning Around Medoids (PAM) algorithm. [16], [17]

The steps to perform k-medoids clustering are:

1.  Select k, the number of clusters, that you want to form in the data.

2.  Select k random observations from the dataset as the initial medoids.

3.  Assign each observation to the cluster whose medoid is closest to it based on a distance metric.

4.  Recalculate the medoids as the observation in each cluster that minimizes the total dissimilarity to the other observations in the same cluster.

5.  Repeat steps 3 and 4 until the cluster assignments no longer change or reach a maximum number of iterations.

It's important to note that k-medoids is more robust to noise and outliers than k-means, it's also more efficient for handling categorical variables. However, k-medoids is more computationally expensive than k-means because it requires the calculation of all pairwise distances between observations at each iteration. Like k-means, k-medoids is sensitive to the initial conditions and it's recommended to run the algorithm multiple times and choose the best solution.

After applying the k-medoids algorithm, the resulting output is k clusters where each cluster has its own medoid, and each observation is assigned to the cluster to which it is closest. These clusters can be used for further analysis or interpretation of the data.

#### Hierarchical Clustering

Hierarchical Clustering is a method of clustering in which the objects are organized into a tree-like structure called a dendrogram. The main idea behind hierarchical clustering is to start with each object as a separate cluster and then combine them into larger clusters iteratively based on their similarity. There are two main types of hierarchical clustering: Agglomerative and Divisive. [18], [19], [20]

Agglomerative hierarchical clustering:

1.  Start with each object as a separate cluster

2.  Find the two most similar clusters and combine them into a new cluster

3.  Repeat step 2 until all objects are in the same cluster

Divisive hierarchical clustering:

1.  Start with all objects in the same cluster

2.  Divide the largest cluster into two smaller clusters based on their similarity

3.  Repeat step 2 until each object forms its own cluster

Hierarchical clustering can be represented by a dendrogram, which is a tree-like structure that shows the hierarchy of clusters and the relations between them. The dendrogram can be cut at a certain height to obtain a flat clustering solution with a specific number of clusters.

It's important to note that hierarchical clustering is sensitive to the scale and density of the data, so it's important to scale the data before applying the method. Also, the choice of linkage method (single, complete, average, etc) is important and it affects the final clustering. Additionally, hierarchical clustering is computationally expensive for large datasets and it's not suitable for handling high-dimensional data.

##### Ward's Minimum Variance Method

The Ward's linkage method is started for hierarchical clustering. Hierarchical clustering is performed using both euclidean and manhattan distance metrics and dendograms is visualized. Then, the cophenetic distances of the clustering is measured. The correlation between the original distance and cophenetic distance is examined and a decision is made on which distance metric to proceed with.

###### Cophenetic Distance

The cophenetic distance is a measure used in hierarchical clustering to evaluate the similarity between two observations in the dendrogram produced by the clustering algorithm. It is defined as the distance between two observations in the original data space at the level in the dendrogram where they first merge into the same cluster[21].

The cophenetic distance is calculated as follows:

-   Perform hierarchical clustering on the data to produce a dendrogram

-   For each pair of observations, find the level in the dendrogram where they first merge into the same cluster.

-   Compute the distance between the two observations in the original data space. Repeat steps 2 and 3 for all pairs of observations.

The cophenetic distance is used to evaluate the quality of the clustering solution by comparing it to the original data space. A high correlation between the cophenetic distance and the original distance between observations in the data space indicates that the clustering solution is preserving the structure of the data well.

##### Average Linkage Method

The average linkage method (also known as UPGMA) is an agglomerative linkage method used in hierarchical clustering. It is based on the idea of minimizing the average distance between observations in the two clusters being merged. The average linkage method is a measure of the dissimilarity between two clusters, defined as the average distance between the points in one cluster and the points in the other. [22], [23]

The steps to perform hierarchical clustering using average linkage method are:

Start with each observation as a separate cluster Compute the distance matrix between all pairs of clusters. Merge the two clusters that have the minimum average distance between their observations and form a new cluster. Repeat steps 2 and 3 until all observations are in the same cluster. The average linkage method is sensitive to the scale of the variables, so it's recommended to standardize the variables before applying the method. Average linkage method tends to create elongated and non-compact clusters, and it's more efficient for handling datasets with small number of observations and variables.

#### Model Based Clustering

Model-based clustering is a method of clustering in which a probabilistic model is fit to the data, and the clusters are defined as the parameters of the model. The main idea behind model-based clustering is to assume that the data is generated by a certain probability distribution, and the clusters correspond to different modes of that distribution.

There are several types of model-based clustering methods such as:

1.  Gaussian Mixture Model (GMM): assumes that the data is generated by a mixture of Gaussian distributions, and estimates the parameters of the distributions, such as means and covariances, to define the clusters. I will use this version in this analysis.

2.  Latent Dirichlet Allocation (LDA): a generative probabilistic model used to classify text in natural language processing and information retrieval. It assumes that each document is a mixture of topics and each topic is a mixture of words.

3.  Hidden Markov Model (HMM): a statistical model used to predict a sequence of hidden states from a sequence of observations. It can be used for clustering sequences of data.

Model-based clustering methods have some advantages over traditional clustering methods, such as the ability to model complex data distributions and handle missing data. However, it's also sensitive to the initial conditions and the number of clusters and it's computationally expensive for large datasets.

#### Density-Based Clustering

Density-based clustering is a type of clustering algorithm that groups together data points that are closely packed together, while separating those that are more sparsely distributed. The main idea behind density-based clustering is to identify regions in the feature space where the data points are dense, and then to extract clusters based on these regions. [26]

One commonly used density-based clustering algorithm is DBSCAN (Density-Based Spatial Clustering of Applications with Noise). DBSCAN groups together data points that are close to each other based on a distance measure and a density threshold. It defines clusters as dense regions of points that are separated from other dense regions by regions of lower point density. [27]

Another example of density-based clustering is HDBSCAN (Hierarchical Density-Based Spatial Clustering of Applications with Noise) which is an extension of DBSCAN algorithm, it can discover clusters of varying densities and shapes, and it can also discover clusters with different numbers of points, and it is less sensitive to parameter tuning.

Density-based clustering is useful for data sets that contain clusters of different shapes and sizes, and for data sets with noise and outliers.

In density-based clustering, the number of clusters does not need to be predetermined, but the values of MinPts and eps do. The eps parameter defines the radius of the neighbors around a point x. This is called the epsilon neighborhood of x. The MinPts parameter is the minimum number of neighbors within the "eps" radius. KNN distplot can be used to determine these values.

**References**

[1] Bryant, F. B., & Yarnold, P. R. (1995). Principal-components analysis and exploratory and confirmatory factor analysis.

[2] James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning (Vol. 112, p. 18). New York: springer.

[3] Ben-Hur, Asa, and Isabelle Guyon. Detecting stable clusters using principal component analysis. Functional genomics. Humana press, 159-182, 2003.

[4] Ding, Chris, and Xiaofeng He. K-means clustering via principal component analysis. Proceedings of the twenty-first international conference on Machine learning. 2004.

[5] Hinton, G., & Sejnowski, T. J. (Eds.).Unsupervised learning: foundations of neural computation. MIT press. 1999

[6] Kassambara, Alboukadel. Practical guide to cluster analysis in R: Unsupervised machine learning. Vol. 1. Sthda, 2017.

[7] Hopkins, J. W., & Gridgeman, N. T. (1955). Comparative sensitivity of pair and triad flavor intensity difference tests. Biometrics, 11(1), 63-68.

[8] Hartigan, John A., Manchek A. Wong. Algorithm AS 136: A k-means clustering algorithm. Journal of the royal statistical society. series c (applied statistics) 28., 100-108, 1979

[9] Kassambara, Alboukadel. Practical guide to cluster analysis in R: Unsupervised machine learning. Vol. 1. Sthda, 2017.

[10] James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning (Vol. 112, p. 18). New York: springer.

[11] Steinley, D., & Brusco, M. J. (2011). Choosing the number of clusters in Κ-means clustering. Psychological methods, 16(3), 285.

[12] Halkidi, Maria, Yannis Batistakis, and Michalis Vazirgiannis. "On clustering validation techniques." Journal of intelligent information systems 17 (2001): 107-145.

[13] Rousseeuw, Peter J. Silhouettes: a graphical aid to the interpretation and validation of cluster analysis.Journal of computational and applied mathematics, 1987, 20: 53-65.

[14] Halkidi, M., Batistakis, Y., & Vazirgiannis, M. (2001). On clustering validation techniques. Journal of intelligent information systems, 17, 107-145.

[15] Tibshirani, R., Walther, G., & Hastie, T. (2001). Estimating the number of clusters in a data set via the gap statistic. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 63(2), 411-423.

[16] Kaufman, L., & Rousseeuw, P. (1987). Clustering by means of medoids. Statistical Data Analysis Based on the L1-Norm and Related Methods, Y. Dodge Ed.

[17] Kaufman, L., & Rousseeuw, P. J. (2009). Finding groups in data: an introduction to cluster analysis. John Wiley & Sons.

[18] Ward Jr, J. H. (1963). Hierarchical grouping to optimize an objective function. Journal of the American statistical association, 58(301), 236-244.

[19] Roux, M. (2015). A comparative study of divisive hierarchical clustering algorithms. arXiv preprint arXiv:1506.08977.

[20] Kassambara, Alboukadel. Practical guide to cluster analysis in R: Unsupervised machine learning. Vol. 1. Sthda, 2017.

[21] Triayudi, A., & Fitri, I. (2018). Comparison of parameter-free agglomerative hierarchical clustering methods. ICIC Express Letters, 12(10), 973-980.

[22] Murtagh, F., & Contreras, P. (2012). Algorithms for hierarchical clustering: an overview. Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 2(1), 86-97.

[23] Kassambara, Alboukadel. Practical guide to cluster analysis in R: Unsupervised machine learning. Vol. 1. Sthda, 2017.

[24] McNicholas, P. D. (2016). Model-based clustering. Journal of Classification, 33, 331-373.

[25] Kassambara, Alboukadel. Practical guide to cluster analysis in R: Unsupervised machine learning. Vol. 1. Sthda, 2017.

[26] Kriegel, H. P., Kröger, P., Sander, J., & Zimek, A. (2011). Density‐based clustering. Wiley interdisciplinary reviews: data mining and knowledge discovery, 1(3), 231-240.

[27] Bäcklund, H., Hedblom, A., & Neijman, N. (2011). A density-based spatial clustering of application with noise. Data Mining TNM033, 33, 11-30.
