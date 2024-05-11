library(dplyr)
library(magrittr)  # this explicitly loads the magrittr package for the pipe operator
library(factoextra)  # load factoextra for PCA and clustering visualization

# import the dataset
data <- read.csv("/Users/tianxiangchen/Desktop/math 250/online_shoppers_intention.csv")
data <- subset(data, select = -Revenue)
# Explore the dataset
print(head(data))
print(summary(data))
str(data)
# Check for missing values
print(sum(is.na(data)))
# Handling missing values
# Impute numeric missing values with the median, and categorical with the mode
# Adjust this based on your specific data structure
data <- data %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>%
  mutate_if(is.character, ~ifelse(is.na(.), as.character(Stats::mode(.)[1]), .))

#Separate the data into integer,continuous,categorical
# Using subset to exclude columns
data_integer <- subset(data, select = -c(Administrative_Duration,Informational_Duration,ProductRelated_Duration, BounceRates, ExitRates,PageValues,SpecialDay, Month, VisitorType, Weekend))
data_contiune<- subset(data, select = c(Administrative_Duration,Informational_Duration,ProductRelated_Duration, BounceRates, ExitRates,PageValues,SpecialDay))
data_categorical<-subset(data, select = c(Month, VisitorType, Weekend))
# Normalization: Scale numeric features
# Scale function standardizes data (mean = 0, sd = 1)
data_contiune <- data_contiune %>%
  scale()
# Encoding Categorical Variables: Convert categorical variables into numerical values
# Assuming 'data' is your data frame
for (col in names(data_categorical)) {
  data_categorical[[col]] <- as.integer(as.factor(data_categorical[[col]]))
}


# Combine numeric and factored data
preprocessed_data <- cbind(data_integer, data_contiune,data_categorical)
print(head(preprocessed_data))
#Varibale selection

#Factor Analysis
# Assuming you have decided to extract 3 factors
fa_result <- factanal(preprocessed_data, factors = 9, rotation = "varimax")
#Check the Factor loading
fa_result$loadings
#Check the Uniqueness
fa_result$uniquenesses
#Check the Variance
# Extracting loadings from the factor analysis result
loadings <- fa_result$loadings[, 1:9]  # assuming you extracted 3 factors

# Calculating communalities: sum of the squares of the loadings for each variable
communalities <- rowSums(loadings^2)

# Summing the communalities to get total variance explained
total_variance_explained <- sum(communalities)

# Total initial variance (assuming each variable was standardized)
total_initial_variance <- ncol(data)

# Proportion of total variance explained
proportion_variance_explained <- total_variance_explained / total_initial_variance

# Output the result
print(paste("Total variance explained by the model: ", proportion_variance_explained * 100, "%"))
#visualization factor

# Creating a heatmap of the loadings
heatmap(as.matrix(fa_result$loadings), Rowv = NA, Colv = NA,
        col = heat.colors(256), scale = "column",
        margin = c(5, 10))

#implement k-means clustering on the results from a factor analysis

# Assuming `data` has been scaled and `fa_result` is your factor analysis output
factor_scores <- scale(preprocessed_data) %*% fa_result$loadings
# Assuming you decide on 3 clusters
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(factor_scores, centers = 2, nstart = 25)

# Viewing the clustering result
print(kmeans_result$size)
print(kmeans_result$centers)
#Visualize the Results
library(ggplot2)
factor_scores_df <- as.data.frame(factor_scores)
factor_scores_df$cluster <- as.factor(kmeans_result$cluster)
# Print column names to ensure correct referencing
print(colnames(factor_scores_df))

ggplot(factor_scores_df, aes(x = Factor1, y = Factor2, color = cluster)) + 
  geom_point(alpha = 0.5) +
  labs(title = "K-Means Clustering on Factor Scores",
       x = "Factor 1 Score",
       y = "Factor 2 Score")







# PCA
pca_result <- prcomp(preprocessed_data, center = TRUE, scale. = TRUE)
fviz_eig(pca_result)  # Visualize the explained variance

# Choose the number of principal components (e.g., components that explain 80% of the variance)
pca_data <- pca_result$x[, 1:5]  # Assume first 5 components are chosen
print(pca_data)
#Extract the loading 
loadings <- pca_result$rotation[,1:5]
print(loadings)
# visualize the loading
library(ggplot2)
loadings_df <- as.data.frame(loadings)
loadings_df$variable <- rownames(loadings)

# Plotting the first two principal components
ggplot(loadings_df, aes(x = PC1, y = PC2, label = variable)) +
  geom_text(aes(color = abs(PC1) + abs(PC2))) +  # Color by contribution magnitude
  xlab("First Principal Component") +
  ylab("Second Principal Component") +
  ggtitle("PCA Loadings Plot") +
  theme_minimal()

#implement the k-means using pca reduction
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(pca_data, centers = 2)  # change the number of centers as needed
# size of each cluster
print(kmeans_result$size)
# coordinates of cluster centers
print(kmeans_result$centers)  
library(ggplot2)
#plot the cluster
clusters <- as.factor(kmeans_result$cluster)
ggplot(as.data.frame(pca_data), aes(x = PC1, y = PC2, color = clusters)) +
  geom_point(alpha = 0.5) +
  geom_point(data = as.data.frame(kmeans_result$centers), aes(x = PC1, y = PC2), color = "red", size = 10) +
  ggtitle("K-Means Clustering on PCA Results")

## t-SNE
data_total <- read.csv("/Users/tianxiangchen/Desktop/math 250/online_shoppers_intention.csv")
library(Rtsne)
set.seed(42)
# perplexity=30
tsne_results_30 <- Rtsne(preprocessed_data, dims = 2, perplexity=30, check_duplicates = FALSE)
tsne_data_30 <- data.frame(X = tsne_results_30$Y[,1], Y = tsne_results_30$Y[,2], Revenue = data_total$Revenue)
ggplot(tsne_data_30, aes(x = X, y = Y, color = Revenue)) +
  geom_point(alpha = 0.5) +
  labs(title = "t-SNE (p=30)")

# perplexity=10
tsne_results_10 <- Rtsne(preprocessed_data, dims = 2, perplexity=10, check_duplicates = FALSE)
tsne_data_10 <- data.frame(X = tsne_results_10$Y[,1], Y = tsne_results_10$Y[,2], Revenue = data_total$Revenue)
ggplot(tsne_data_10, aes(x = X, y = Y, color = Revenue)) +
  geom_point(alpha = 0.5) +
  labs(title = "t-SNE (p=10)")

# perplexity=50
tsne_results_50 <- Rtsne(preprocessed_data, dims = 2, perplexity=50, check_duplicates = FALSE)
tsne_data_50 <- data.frame(X = tsne_results_50$Y[,1], Y = tsne_results_50$Y[,2], Revenue = data_total$Revenue)
ggplot(tsne_data_50, aes(x = X, y = Y, color = Revenue)) +
  geom_point(alpha = 0.5) +
  labs(title = "t-SNE (p=50)")


## umap
library(umap)
set.seed(52)
umap_results <- umap(preprocessed_data, n_neighbors=50, min_dist=0.1)

# create UMAP data
umap_data <- data.frame(X = umap_results$layout[,1], Y = umap_results$layout[,2], Revenue = data_total$Revenue)

# visualize UMAP results
ggplot(umap_data, aes(x = X, y = Y, color = Revenue)) +
  geom_point(alpha = 0.5) +
  labs(title = "UMAP Visualization of Online Shoppers' Intentions")

library(cluster)
set.seed(42)
kmeans_result <- kmeans(umap_data[, c("X", "Y")], centers=4)

# Label the clustering results on the UMAP map
umap_data$cluster <- as.factor(kmeans_result$cluster)

ggplot(umap_data, aes(x = X, y = Y, color = Revenue)) +
  geom_point(aes(shape = cluster), alpha=0.2) +
  labs(title = "UMAP Visualization with Clusters of Online Shoppers' Intentions")

## LDA
library(MASS)
data_total$Revenue <- as.factor(data_total$Revenue)

# Revenue is the categorical variable, and the others are the feature variables
lda_model <- lda(Revenue ~ ., data = data_total)
print(lda_model)
plot(lda_model) 
