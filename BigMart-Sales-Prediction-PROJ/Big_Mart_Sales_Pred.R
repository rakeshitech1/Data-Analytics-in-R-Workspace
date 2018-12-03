library(caTools)#used for spliiting
library(rpart)#used for decission trees
library(partykit)
library(randomForest)#random forest
library(hydroGOF)# rmse
library(Amelia)# miss map graph
library(class)# Knn
library(e1071)# Naive bayes
library(ElemStatLearn)

data <- read.csv("file:///C:/Users/RAKESH REDDY/Downloads/Train_UWu5bXk (1).csv", na.strings = c(" ","","NA"))
test_data <- read.csv("file:///C:/Users/RAKESH REDDY/Downloads/Test_u94Q5KV (1).csv", na.strings = c(" ","","NA"))

View(data)
any(is.na(data))
colSums(is.na(data))

# Converting the [Outlet_Establishment_Year] into a Factor from Numeric
data[,8] = as.factor(data[,8])
str(data)

# ALtering the Distribution of Dependent Variable
hist(data$Item_Outlet_Sales)
data$Item_Outlet_Sales = log(data$Item_Outlet_Sales)
hist(data$Item_Outlet_Sales)

# Missing Values Compensation
modes <- function(x){
  t <- table(x)
  return(names(t)[t == max(t)])
}

for(i in 1:length(data)){
  if(is.factor(data[,i]))
  {
    data[,i][is.na(data[,i])] = modes(data[,i])
  }
  else
  {
    data[,i][is.na(data[,i])] = median(data[,i],na.rm = TRUE)
  }
}
any(is.na(data))
colSums(is.na(data))

normalize = function(x){
  return (( x-min(x))/(max(x)-min(x)))
}

for(j in 1:11){
  if(!(is.factor(data[,j])))
  {
    data[,j] = normalize(data[,j])
  }
  else
  {
    data[,j] = data[,j]
  }
}
summary(data)

# Outlier Detection and Removal
boxplot(data$Item_Outlet_Sales)
boxplot(data$Item_Weight)
boxplot(data$Item_Visibility)
boxplot(data$Item_MRP)

floors <- function(x){
  q1 = summary(x)[2]
  q3 = summary(x)[5]
  f = q1 - 1.5*(q3 - q1)
  return(f)
}
ceilings = function(x){
  q1 = summary(x)[2]
  q3 = summary(x)[5]
  c = q3 + 1.5*(q3 - q1)
  return(c)
}

for(i in 1:(length(data)-1)){
  if(!(is.factor(data[,k])))
  {
    data[,i] = ifelse(data[,k]<floors(data[,i]),floors(data[,i]),data[,i])
    data[,i] = ifelse(data[,k]>ceilings(data[,i]),ceilings(data[,i]),data[,i])
  }
  else
  {
    data[,i] = data[,i]
  }
}

boxplot(data$Item_Weight)
boxplot(data$Item_Visibility)
boxplot(data$Item_MRP)

# (Test_data) dataset EDA

any(is.na(train_set))
colSums(is.na())

# Converting the [Outlet_Establishment_Year] into a Factor from Numeric
test_data[,8] = as.factor(test_data[,8])
str(test_data)

# Missing Values Compensation for test_data
for(l in 1:length(test_data)){
  if(is.factor(test_data[,l]))
  {
    test_data[,l][is.na(test_data[,l])] = modes(test_data[,l])
  }
  else
  {
    test_data[,l][is.na(test_data[,l])] = median(test_data[,l],na.rm = TRUE)
  }
}
any(is.na(test_data))
colSums(is.na(test_data))

boxplot(test_data$Item_Weight)
boxplot(test_data$Item_Visibility)
boxplot(test_data$Item_MRP)

for(m in 1:11){
  if(!(is.factor(test_data[,m])))
  {
    test_data[,m] = normalize(test_data[,m])
  }
  else
  {
    test_data[,m] = test_data[,m]
  }
}        

boxplot(test_data$Item_Weight)
boxplot(test_data$Item_Visibility)
boxplot(test_data$Item_MRP)

# Removing the outliers for test_data

for(n in 1:(length(data)-1)){
  if(!(is.factor(test_data[,n])))
  {
    test_data[,n] = ifelse(test_data[,n]<floors(test_data[,n]),floors(test_data[,n]),test_data[,n])
    test_data[,n] = ifelse(test_data[,n]>ceilings(test_data[,n]),ceilings(test_data[,n]),test_data[,n])
  }
  else
  {
    test_data[,n] = test_data[,n]
  }
}

boxplot(test_data$Item_Weight)
boxplot(test_data$Item_Visibility)
boxplot(test_data$Item_MRP)



#Splitting the data and performing Linear Regression
set.seed(132)
split = sample(2, nrow(data), prob = c(0.75,0.25), replace = TRUE)
train_set <- data[split == 1,]
test_set <- data[split == 2,]

train_set$Item_Identifier = as.numeric(train_set$Item_Identifier)
lin_model<- lm(Item_Outlet_Sales ~ Item_Identifier + Item_Visibility + Outlet_Identifier, data = train_set)
summary(lin_model)
comp_model = lm(Item_Outlet_Sales ~ ., data = train_set)
summary(comp_model)
step(comp_model, direction = 'both')

model_aic <- lm(Item_Outlet_Sales ~ Item_MRP + Outlet_Identifier,data = train_set)
summary(model_aic)
#Prediction and Validation
test_set$Item_Identifier = as.numeric(test_set$Item_Identifier)
linearmod_predict <- predict(model_aic, newdata = test_set)
Rmse_lin <- rmse(linearmod_predict,test_set$Item_Outlet_Sales)
Rmse_lin #0.5380099


#Regression Trees
tree_model <- rpart(Item_Outlet_Sales ~., data = train_set)
plot(tree_model)
plot(as.party(tree_model))

tree_model$cptable
cp_6 <- tree_model$cptable[6,1]
cp_6

tree_model_prune <- prune(tree_model, cp = cp_6)
plot(as.party(tree_model_prune))
tree_model_predict <- predict(tree_model_prune, newdata = test_set, type = 'vector')
head(tree_model_predict)
head(test_set$Item_Outlet_Sales)

RMSE_Tree <- rmse(tree_model_predict, test_set$Item_Outlet_Sales)
RMSE_Tree #0.5757531

# SVM

mod_svm_linear = svm(Item_Outlet_Sales ~ ., data = train_set, type = 'nu-regression', kernel = 'linear')
mod_svm_linear_predict <- predict(mod_svm_linear,newdata = test_set)
RMSE_svm_linear = rmse(mod_svm_linear_predict,test_set$Item_Outlet_Sales)
RMSE_svm_linear #0.5419028

mod_svm_radial = svm(Item_Outlet_Sales ~ ., data = train_set, type = 'nu-regression', kernel = 'radial')
mod_svm_radial_predict <- predict(mod_svm_radial,newdata = test_set)
RMSE_svm_radial = rmse(mod_svm_radial_predict,test_set$Item_Outlet_Sales)
RMSE_svm_radial #0.5245224

mod_svm_sigmoid = svm(Item_Outlet_Sales ~ ., data = train_set, type = 'nu-regression', kernel = 'sigmoid')
mod_svm_sigmoid_predict <- predict(mod_svm_sigmoid,newdata = test_set)
RMSE_svm_sigmoid = rmse(mod_svm_sigmoid_predict,test_set$Item_Outlet_Sales)
RMSE_svm_sigmoid # 0.5499457

mod_svm_polynomial = svm(Item_Outlet_Sales ~ ., data = train_set, type = 'nu-regression', kernel = 'polynomial')
mod_svm_polynomial_predict <- predict(mod_svm_polynomial,newdata = test_set)
RMSE_svm_polynomial = rmse(mod_svm_polynomial_predict,test_set$Item_Outlet_Sales)
RMSE_svm_polynomial #  0.7973409

test_data$Item_Identifier = as.numeric(test_data$Item_Identifier)
ext_pred <- predict(model_svm_radial, newdata = test_data)
ext_pred

predicted_DV = exp(ext_pred)
predicted_DV
write.table(predicted_DV, file = 'file:///C:/Users/RAKESH REDDY/Desktop/SVM.csv', sep = ',', row.names = FALSE)


#Random Forest

set.seed(300)
forest_mod<- randomForest(Item_Outlet_Sales ~ ., data = train_set, ntree = 500)
plot(model_forest)
forest_mod$mse
min_error <- which.min(forest_mod$mse)
min_error

model_forest_predict <- predict(forest_mod, newdata = test_set, ntree = min_error)
head(model_forest_predict)
head(test$Item_Outlet_Sales)

rmse_forest <- rmse(model_forest_predict, test_set$Item_Outlet_Sales)
rmse_forest #0.5372863

















