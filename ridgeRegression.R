library(caret)
library(Metrics)
library(glmnet)

# load dataset
data = read.csv('US_Tax_dataset_updated.csv')
set.seed(123)

# Data Splitting 80/20 ratio
trainIndex = createDataPartition(
  data$future_tax_2026,
  p = 0.8,
  list = FALSE
)

train_data = data[trainIndex, ]
test_data = data[-trainIndex, ]

# converting training features to matrix and extract target variable
X_train = as.matrix(train_data[, -which(names(train_data) == "future_tax_2026")])
y_train = train_data$future_tax_2026

# converting testing features to matrix and extract target variable
X_test = as.matrix(test_data[, -which(names(test_data) == "future_tax_2026")])
y_test = test_data$future_tax_2026

# Training Model (Algo : Ridge Regression)
ridge_model = cv.glmnet(X_train, y_train, alpha = 0)

# Predictions by model
predictions = as.vector(predict(ridge_model, newx = X_test, s = "lambda.min"))

# Evaluation
rmse_value = rmse(y_test, predictions)
mae_value = mae(y_test, predictions)

print(rmse_value)
print(mae_value)

saveRDS(ridge_model, "tax_model3.rds")

SSE = sum((y_test - predictions)^2)
SST = sum((y_test - mean(y_test))^2)
R2 = 1 - SSE / SST

print(R2)