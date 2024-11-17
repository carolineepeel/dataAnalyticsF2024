## Loading the Libraries
library(e1071)
library(caret)
library(class)
library(ggplot2)  # For visualization
library(caret)  # For data partitioning and metrics


## FILE PATHS 
### setting working directory
setwd('C:/Users/carol/OneDrive/Documents/data_analytics/lab05')

wine_data_path <- "wine.data"
wine_name_path <- "wine.names"

## Read the dataset
wine <- read.csv(wine_data_path, header = FALSE)
wine_names <- readLines('wine.names')

##Assigning Column names from Wine.names
column_names <- c("Type", "Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", 
                  "Phenols", "Flavonoids", "Nonflavanoids", "Proanthocyanins", 
                  "Color", "Hue", "OD280_OD315", "Proline")

colnames(wine) <- column_names

##TEST - COMMENT OUT WHEN DONE
##head(wine)

##PREPROCESS THE DATA 
wine$Type <- as.factor(wine$Type)
#selecting features
features <-c("Color", "Hue", "Alcohol", "Flavonoids")
wine_subset <- wine[,c("Type", features)]
#splitting into training and testing sets
set.seed(123)
train_index <- createDataPartition(wine_subset$Type, p = 0.7, list = FALSE )
train_data <- wine_subset[train_index, ]
test_data <- wine_subset[-train_index, ]

#Linear Kernel SVM with hyperparameter tuning
set.seed(123)
tune_linear <- tune.svm(Type ~ ., data = train_data, kernel = "linear",
                        cost = 10^(-1:2))

svm_linear <- tune_linear$best.model

#Radial Kernel SVM with hyperparameter tuning
set.seed(123)
tune_rbf <- tune.svm(Type ~., data = train_data, kernel = "radial",
                     cost = 10^(-1:2), gamma = 10^(-2:1))
svm_rbf <- tune_rbf$best.model

# Prepare data for kNN
train_knn <- train_data[, -1]
test_knn <- test_data[, -1]

# Train KNN Classifier
set.seed(123)
knn_pred <- knn(train_knn, test_knn, train_data$Type, k= 5)

#EVALUATE THE MODEL 
calc_metrics <- function(true, pred) {
  cm <- confusionMatrix(as.factor(pred), as.factor(true), mode = "everything")
  precision <- ifelse(!is.nan(cm$byClass["Pos Pred Value"]), cm$byClass["Pos Pred Value"], 0)
  recall <- ifelse(!is.nan(cm$byClass["Sensitivity"]), cm$byClass["Sensitivity"], 0)
  f1 <- ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), 0)
  return(list(Precision = precision, Recall = recall, F1 = f1))
}

#Evaluate SVM Linear
svm_linear_pre <- predict(svm_linear, test_data)
metrics_svm_linear <- calc_metrics(test_data$Type, svm_linear_pre)

#Evaluate SVM RBF
svm_rbf_pre <- predict(svm_rbf, test_data)
metrics_svm_rbf <- calc_metrics(test_data$Type, svm_rbf_pre)

#Evaluate KNN
metrics_knn <- calc_metrics(test_data$Type, knn_pred)

# Combine Results into a table
results <- data.frame(
  Model = c("SVM Linear", "SVM RBF", "kNN"),
  Precision = c(metrics_svm_linear$Precision, metrics_svm_rbf$Precision,
                metrics_knn$Precision),
  Recall = c(metrics_svm_linear$Recall, metrics_svm_rbf$Recall, 
             metrics_knn$Recall),
  F1 = c(metrics_svm_linear$F1, metrics_svm_rbf$F1, metrics_knn$F1)
)
table(svm_linear_pre)
table(svm_rbf_pre)
table(knn_pred)

# PRINT RESULTS
print(results)

## QUESTION 2
# Load the dataset
file_path <- "NY-House-Dataset (1).csv"
housing_data <- read.csv(file_path)

# Inspect column names
print(colnames(housing_data))

# Check for potential matches for PRICE and Square Footage
grep("price", colnames(housing_data), ignore.case = TRUE, value = TRUE)
grep("square", colnames(housing_data), ignore.case = TRUE, value = TRUE)


# Select necessary columns
housing_data <- housing_data[, c("PRICE", "PROPERTYSQFT")]
# Rename columns for consistency
colnames(housing_data) <- c("PRICE", "Square_Footage")

# Verify the changes
head(housing_data)

# Remove rows with missing values
housing_data <- na.omit(housing_data)

# Split the dataset into training and testing sets
set.seed(123)
train_index <- createDataPartition(housing_data$PRICE, p = 0.7, list = FALSE)
train_data <- housing_data[train_index, ]
test_data <- housing_data[-train_index, ]

# Train SVM Regression Model
svm_model <- svm(PRICE ~ Square_Footage, data = train_data, kernel = "radial", cost = 1, gamma = 0.1)

# Predict on test data
svm_pred <- predict(svm_model, test_data)

# Calculate performance metrics
svm_rmse <- sqrt(mean((svm_pred - test_data$PRICE)^2))
print(paste("SVM RMSE:", round(svm_rmse, 2)))
# Train Linear Regression Model
linear_model <- lm(PRICE ~ Square_Footage, data = train_data)

# Predict on test data
linear_pred <- predict(linear_model, test_data)

# Calculate performance metrics
linear_rmse <- sqrt(mean((linear_pred - test_data$PRICE)^2))
print(paste("Linear Regression RMSE:", round(linear_rmse, 2)))

# Combine results into one data frame
results <- data.frame(
  Real_Price = test_data$PRICE,
  SVM_Predicted = svm_pred,
  Linear_Predicted = linear_pred
)

# Plot for SVM
ggplot(results, aes(x = Real_Price, y = SVM_Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "SVM Predicted vs Real Price", x = "Real Price", y = "Predicted Price")

# Plot for Linear Regression
ggplot(results, aes(x = Real_Price, y = Linear_Predicted)) +
  geom_point(color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Linear Regression Predicted vs Real Price", x = "Real Price", y = "Predicted Price")