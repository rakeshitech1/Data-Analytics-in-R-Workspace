data <- read.csv("G:/imarticus/R/Datasets/Social_Network_Ads.csv")
View(data)
data <- data[,-1]
data$Purchased = as.factor(data$Purchased)
summary(data)
colSums(is.na(data))

# scaling for age and est sal
normalize <- function(x){
  a <- (x - min(x))/( max(x) - min(x) )
  return(a)
}
data[,c(2,3)] <- lapply(data[,c(2,3)], FUN = normalize)
boxplot(data[,c(2,3)])

# Spliting
split = sample(2,nrow(data), prob = c(0.7,0.3), replace = TRUE)
train = data[split == 1,]
test = data[split == 2,]

# Model
library(e1071)
model <- naiveBayes(x = train[,-4],y = train[,4])
model

# Prediction
model_predict <- predict(model, newdata = test)
model_predict

# Validation
library(caret)
confusionMatrix(model_predict,test[,4])


# SVM

model_svm <- svm(Purchased ~ ., data = train, type = 'C-classification', kernel = 'linear')
model_svm

# Predict

model_svm_predict <- predict(model_svm, newdata = test)
head(model_svm_predict)
head(test$Purchased)

library("caret")
acc_linear <- confusionMatrix(model_svm_predict, test$Purchased)
acc_linear

# SVM_radial

model_svm_radial = svm(Purchased ~ ., data = train, type = 'C-classification', kernel = 'radial')

model_predict_radial <- predict(model_svm_radial, newdata = test)
head(model_predict_radial)
head(test$Purchased)

acc_radial <- confusionMatrix(model_predict_radial,test$Purchased)
acc_radial

# SVM_sigmoid

model_svm_sigmoid <- svm(Purchased ~ ., data = train, type = 'C-classification', kernel = 'sigmoid')

model_predict_sigmoid <- predict(model_svm_sigmoid, newdata = test)
head(model_predict_sigmoid)
head(test$Purchased)

acc_sigmoid <- confusionMatrix(model_predict_sigmoid,test$Purchased)
acc_sigmoid

# SVM_polynomial

model_svm_poly <- svm(Purchased ~ ., data = train, type = 'C-classification', kernel = 'polynomial')

model_predict_poly <- predict(model_svm_poly, newdata = test)
head(model_predict_poly)
head(test$Purchased)

acc_poly <- confusionMatrix(model_predict_poly, test[,4])
acc_poly
