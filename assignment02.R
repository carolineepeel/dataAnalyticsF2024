## Load the csv file in to read
epi2024<- read.csv("~/data_analytics/assignment02/epi2024results_DA_F24_lab03.csv")

## View the dataset
View(epi2024)
attach(epi2024)

## Installing packages 
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("caret")
library(caret)
install.packages("class")
library(class)
install.packages("magrittr")
library(magrittr)


#################################################################

## Q1 - Variable Distributions 

### Filtering data for regions 
latin_america <- epi2024 %>% filter(region == "Latin America & Caribbean")
eastern_europe <- epi2024 %>% filter(region == "Eastern Europe")

### 1.1 Creating Histograms 

##### Histogram for aatin_america
min_LAair <- min(latin_america$AIR)
max_LAair <- max(latin_america$AIR)

hist(latin_america$AIR, seq(min_LAair-5, max_LAair+5, 2), prob = TRUE, main = "Air Qaulity in Latin America")

#### Adding density line
lines(density(latin_america$AIR, na.rm=TRUE, bw=5))

#### Histogram for eastern_europe
min_EEair <- min(eastern_europe$AIR)
max_EEair <- max(eastern_europe$AIR)

hist(eastern_europe$AIR, seq(min_EEair-5, max_EEair+5, 2), prob = TRUE, main = "Air Qaulity in Eastern Europe")

#### Adding density line
lines(density(eastern_europe$AIR, na.rm=TRUE, bw=5))

### 1.2 QQ Plots for both Latin America and Eastern Europe

#### QQ Plot for Latin America 
qqnorm(latin_america$AIR, main = "QQ Plot for Air Quality in Latin America (normal)")
qqline(latin_america$AIR)


#### QQ Plot for Eastern Europe
qqnorm(eastern_europe$AIR, main = "QQ PLot for Air Quality in Eastern Europe (normal)")
qqline(eastern_europe$AIR)

#################################################################

## Q2 - Linear Models
### 2.1 - Choose 5 variables for a linear model
selected_vars <- c("FLI","FSH","FCD","FSS","BTZ")

#### Fit a Linear Model
q2_model <- lm(EPI ~ ., data = epi2024[, c("EPI", selected_vars)])

#### ID the most significant variable -- check p values
summary(q2_model) 

#### Plotting most significant variable against EPI
most_sig <- names(q2_model$coefficients)[which.min(abs(q2_model$coefficients))]
ggplot(epi2024, aes_string(x= most_sig, y ="EPI")) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Linear Model: EPI vs Most Significant Overall")

### 2.2 Linear Model of a Region - Eastern Europe
EE_sv <- c("FLI", "FSH", "FCD", "FSS", "BTZ")

#### Fit a linear model
ee_q2model <- lm(EPI ~ ., data = eastern_europe[, c("EPI", selected_vars)])

#### ID the most significant variable
summary(ee_q2model)

#### plot the most significant variable
m_sig <- names(ee_q2model$coefficients)[which.min(abs(ee_q2model$coefficients))]
ggplot(eastern_europe, aes_string(x = m_sig, y = "EPI")) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Linear Model: Eastern Europe")

### Two Sentence Response
###  I think the first model I made provided a better fit because there were more
### data points. Eastern Europe only had a few data points.

#################################################################

## Q3 - Classification (kNN)

### 3.1
q3_vars <- c("WRS", "WWG", "WWC", "WWT", "WWR")

#### Filter for regions 
regions <- c("Latin America & Caribbean", "Former Soviet States", "Southern Asia")
filtered_data <- epi2024 %>% filter(region %in% regions)

#### Split into testing and training 
index <- createDataPartition(filtered_data$region, p = 0.7, list = FALSE)
train_data <- filtered_data[index, ]
test_data <- filtered_data[-index, ]

#### Train kNN Model 
k <- 5
knn_model <- knn(train_data[, q3_vars], test_data[, q3_vars], train_data$region, k=k)

#### create a confusion matrix
con_matrix <- table(test_data$region, knn_model)

#### Calculate Accuracy
accuracy <- sum(diag(con_matrix)) / sum(con_matrix)
cat("Accuracy for Q3.1:",accuracy,"\n")

### 3.2 
q32_vars <- c("WRS", "WWG", "WWC", "WWT", "WWR")

#### Filter for regions 
regions32 <- c("Eastern Europe", "Greater Middle East", "Sub-Saharan Africa")
filtered_data32 <- epi2024 %>% filter(region %in% regions32)

#### Split into testing and training 
index32 <- createDataPartition(filtered_data32$region, p = 0.7, list = FALSE)
train_data32 <- filtered_data[index, ]
test_data32 <- filtered_data[-index, ]

#### Train kNN Model 
k32 <- 5
knn_model32 <- knn(train_data32[, q32_vars], test_data32[, q32_vars], train_data32$region, k=k)

#### create a confusion matrix
con_matrix32 <- table(test_data32$region, knn_model32)

#### Calculate Accuracy
accuracy32 <- sum(diag(con_matrix32)) / sum(con_matrix32)
cat("Accuracy for Q3.2:",accuracy32,"\n")

#### Two Sentence Response
#### I think the second model is better because the regions are slightly 
#### closer to each other. 

#################################################################

## Q4 - Clustering
# 1.0 Fit a K Model for 5 variables of 2 sets of 3 regions
## Defining regions for the data
cluster1 <- c("Eastern Europe", "Former Soviet States", "Greater Middle East")
cluster2 <- c("Southern Asia", "Sub-Saharan Africa", "Latin American & Carribean")

## Defining the Variables to filter
variables <- c("ECO","AIR","OEB","FSH","FCD")

## Filter through epi2024 and standardize 
filtered_c1 <- epi2024 %>% filter(region %in% cluster1) %>%
  select(all_of(variables)) %>% scale() %>%
  as.data.frame() %>%
  mutate_all(as.numeric)

filtered_c2 <- epi2024 %>% filter(region %in% cluster2) %>%
  select(all_of(variables)) %>% scale() %>%
  as.data.frame %>%
  mutate_all(as.numeric)

##replacing NA values with 0 
filtered_c1[is.na(filtered_c1)] <- 0
filtered_c2[is.na(filtered_c2)] <- 0

##Fitting k-means models with k=3
kmeans_c1 <- kmeans(filtered_c1, centers = 3)
kmeans_c2 <- kmeans(filtered_c2, centers =3)

# 1.1 Compare using sum of squares
wcss_cluster1 <- kmeans_c1$tot.withinss
wcss_cluster2 <- kmeans_c2$tot.withinss

cat("Within-Cluster Sum of Squares (WCSS):\n")
cat("Cluster 1:", wcss_cluster1,"\n")
cat("Cluster 2:", wcss_cluster2,"\n")

# 1.2 Loop Kmeans models for multiple values of K 
n_row1  <- nrow(filtered_c1)
n_row2 <- nrow(filtered_c2)

k_values1 <- seq(from = 1, to  = min(n_row1 - 1, 10))
k_values2 <- seq(from = 1, to = min(n_row2 - 1, 10))

wcss_cluster1_all <- sapply(k_values1, function(k){
  kmeans_model1 <- kmeans(cluster1, centers = k)
  return(kmeans_model1$tot.withinss)
})

wcss_cluster2_all <- sapply(k_values2, function(k){
  kmeans_model2 <- kmeans(cluster2, centers = k)
  return(kmeans_model2$tot.withinss)
})

#Plot WCSS across k values
plot(k_values, wcss_cluster1_all, type="b", xlab = "Number of clusters", ylab = "WCSS", main = "Elbow Method")
lines(k_values, wcss_group2_all, type = "b", col = red)
legend("topright", legend = c("Cluster 1", "Cluster 2"), col =c("black", "red"), lty = 1)
                                                              
                                                              
                                                            
