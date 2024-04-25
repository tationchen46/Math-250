library(dplyr)
library(magrittr)  # this explicitly loads the magrittr package for the pipe operator
library(factoextra)  # load factoextra for PCA and clustering visualization

# import the dataset
data <- read.csv("/Users/tianxiangchen/Desktop/math 250/online_shoppers_intention.csv")
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
data_integer <- subset(data, select = -c(ProductRelated_Duration, BounceRates, ExitRates, Month, VisitorType, Weekend, Revenue))
data_contiune<- subset(data, select = c(ProductRelated_Duration, BounceRates, ExitRates))
data_categorical<-subset(data, select = c(Month, VisitorType, Weekend, Revenue))
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

# PCA
pca_result <- prcomp(preprocessed_data, center = TRUE, scale. = TRUE)
fviz_eig(pca_result)  # Visualize the explained variance

# Choose the number of principal components (e.g., components that explain 80% of the variance)
pca_data <- pca_result$x[, 1:5]  # Assume first 5 components are chosen
print(pca_data)
