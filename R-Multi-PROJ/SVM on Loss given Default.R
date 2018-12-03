data <- read.csv("G:/imarticus/R/Datasets/R_Module_Day_5.2_Data_Case_Study_Loss_Given_Default (1).csv")
View(data)
data[,1] <- NULL

# Scaling

normalize <- function(x) {
  return ( ( x - min(x) ) / ( max(x) - min(x) ) )
}
data[,c(1,2,3)] = lapply(data[,c(1,2,3)], FUN = normalize)
summary(data)

# Spliting 

split <- sample(2,nrow(data), prob = c(0.7,0.3), replace = TRUE)
train <- data[split == 1,]
test <- data[split == 2,]

# MODEL (SVM_linear)
library(e1071)
model_svm_linear = svm(Losses.in.Thousands ~ ., data = train, type = 'eps-regression', kernel = 'linear')

model_predict_linear <- predict(model_svm_linear,newdata = test)
head(model_predict_linear)
head(test$Losses.in.Thousands)

# RMSE
library("hydroGOF")
RMSE_linear <- rmse(model_predict_linear,test$Years.of.Experience)
RMSE_linear

# SVM_radial
model_radial <- svm(Losses.in.Thousands ~ ., data = train, type = 'eps-regression', kernel = 'radial')

model_predict_radial <- predict(model_radial, newdata = test)
head(model_predict_linear)
head(test$Losses.in.Thousands)

# RMSE
RMSE_radial <- rmse(model_predict_radial,test$Losses.in.Thousands)
RMSE_radial

# SVM_sigmoid
model_sigmoid <- svm(Losses.in.Thousands ~ ., data = train, type = 'nu-regression', kernel = 'sigmoid')

model_predict_sigmoid <- predict(model_sigmoid, newdata = test)
head(model_predict_sigmoid)
head(test$Losses.in.Thousands)

RMSE_sigmoid <- rmse(model_predict_sigmoid,test$Losses.in.Thousands)
RMSE_sigmoid

# SVM_polynomial
model_polynomial <- svm(Losses.in.Thousands ~.,data = train, type = 'eps-regression', kernel = 'polynomial')

model_predict_polymial <- predict(model_polynomial, newdata = test)
head(model_predict_polymial)
head(test$Losses.in.Thousands)

RMSE_poly <- rmse(model_predict_polymial,test$Losses.in.Thousands)
RMSE_poly
