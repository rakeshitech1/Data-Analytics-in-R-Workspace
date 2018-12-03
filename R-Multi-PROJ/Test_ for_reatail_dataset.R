# Importing

data <- read.csv("file:///C:/Users/RAKESH REDDY/Downloads/Train_UWu5bXk (1).csv", na.strings = c(" ","","NA"))
View(data)

# Exploratory Data Analysis

# 1) Checking for missing values

any(is.na(data))
colSums(is.na(data))

# 2) Convering the type to factors

data[,8] = as.factor(data[,8])
str(data)

# 3) checking for normal distribution of the dependent variable


library(ggplot2)
ggplot(data = data, aes(x = Item_Outlet_Sales))+ geom_histogram(bins = 50)
ggplot(data = data, aes(x = Item_Outlet_Sales))+ geom_freqpoly(bins = 10)

# Taking log transformation to convert to normally distribued

ggplot(data = data, aes(x = log(Item_Outlet_Sales))) + geom_histogram(bins = 50)
ggplot(data = data, aes(x = log(Item_Outlet_Sales))) + geom_freqpoly(bins = 10)

data$Item_Outlet_Sales = log(data$Item_Outlet_Sales)
# 4) Replacing the missing values

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


# 5)scaling

summary(data)

normalize = function(x){
  return ( ( x - min(x) )/ ( max(x) - min(x) ) )
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

# All the quantitative variables are scales

# 6) Checking for outliers

ggplot(data = data, aes(x = Item_Weight, y = Item_Outlet_Sales)) + geom_boxplot()
ggplot(data = data, aes(x = Item_MRP, y = Item_Outlet_Sales)) + geom_boxplot()

boxplot(data$Item_Outlet_Sales)

# Removing the outliers

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

for(k in 1:11){
  if(!(is.factor(data[,k])))
  {
    data[,k] = ifelse(data[,k]<floors(data[,k]),floors(data[,k]),data[,k])
    data[,k] = ifelse(data[,k]>ceilings(data[,k]),ceilings(data[,k]),data[,k])
  }
  else
  {
    data[,k] = data[,k]
  }
}
boxplot(data[,c(2,4,6)], main = 'Boxplot of Item_Weight,Item_Visibility,Item_MRP', col = c('skyblue','cadetblue','burlywood'))


#--------------------------------------------------------------------------------------------------------------

# Testing dataset

external <- read.csv("file:///C:/Users/RAKESH REDDY/Downloads/Test_u94Q5KV (1).csv", na.strings = c(" ","","NA"))

# Exploratory Data Analysis

# 1) Checking for missing values

any(is.na(external))
colSums(is.na(external))

# 2) Convering the type to factors

external[,8] = as.factor(external[,8])
str(external)

# 3) Replacing the missing values

for(l in 1:length(external)){
  if(is.factor(external[,l]))
  {
    external[,l][is.na(external[,l])] = modes(external[,l])
  }
  else
  {
    external[,l][is.na(external[,l])] = median(external[,l],na.rm = TRUE)
  }
}
any(is.na(external))
colSums(is.na(external))

# # 5)scaling

boxplot(external[,c(2,4,6)],main = 'Boxplot of Item_Weight,Item_Visibility,Item_MRP', col = c('skyblue','cadetblue','burlywood'))

for(m in 1:11){
  if(!(is.factor(external[,m])))
  {
    external[,m] = normalize(external[,m])
  }
  else
  {
    external[,m] = external[,m]
  }
}        

boxplot(external[,c(2,4,6)], main = 'Boxplot of Item_Weight,Item_Visibility,Item_MRP', col = c('skyblue','cadetblue','burlywood'))        

# Removing the outliers

for(n in 1:11){
  if(!(is.factor(external[,n])))
  {
    external[,n] = ifelse(external[,n]<floors(external[,n]),floors(external[,n]),external[,n])
    external[,n] = ifelse(external[,n]>ceilings(external[,n]),ceilings(external[,n]),external[,n])
  }
  else
  {
    external[,n] = external[,n]
  }
}

boxplot(external[,c(2,4,6)], main = 'Boxplot of Item_Weight,Item_Visibility,Item_MRP', col = c('skyblue','cadetblue','burlywood'))        

# ----------------------------------------------------------------------------------------------------------------------------------------

# Spliting the data for training and testing
set.seed(123)
split = sample(2, nrow(data), prob = c(0.7,0.3), replace = TRUE)
train <- data[split == 1,]
test <- data[split == 2,]

# MODELS::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# 1) LINEAR REGRESSION

train$Item_Identifier = as.numeric(train$Item_Identifier)
model_linear <- lm(Item_Outlet_Sales ~ Item_Identifier + Item_Visibility + Outlet_Identifier, data = train)
summary(model_linear)

model_all = lm(Item_Outlet_Sales ~ ., data = train)
summary(model_all)


step(model_all, direction = 'both')

model_aic <- lm(Item_Outlet_Sales ~ Item_MRP + Outlet_Identifier, 
                data = train)
summary(model_aic)


#------------------------------------------------------------------------------------------------------------------

attach(data)
chisq.test(Item_Identifier,Item_Fat_Content)
chisq.test(Item_Identifier,Item_Type)
chisq.test(Outlet_Type,Outlet_Size)
chisq.test(Outlet_Type,Outlet_Establishment_Year)

ggplot(data = data, aes(x = Outlet_Type)) + geom_bar()
ggplot(data = data, aes(x = Outlet_Type, y = Item_Outlet_Sales)) + geom_boxplot()
ggplot(data = data, aes(x = Outlet_Type, y = Item_Outlet_Sales)) + facet_grid(~Outlet_Establishment_Year)+ geom_boxplot() # no

#--------------------------------------------------------------------------------------------------------------

# Prediction

test$Item_Identifier = as.numeric(test$Item_Identifier)
model_linear_predict <- predict(model_aic, newdata = test)

# Validation

library(hydroGOF)
RMSE_linear <- rmse(model_linear_predict,test$Item_Outlet_Sales)
RMSE_linear

# REGRESSION TREES

library(rpart)
model_tree <- rpart(Item_Outlet_Sales ~., data = train)
plot(model_tree)
text(model_tree)

library(partykit)
plot(as.party(model_tree))

# Pruning

model_tree$cptable
cp6 <- model_tree$cptable[6,1]
cp6

model_tree_prune <- prune(model_tree, cp = cp6)
plot(as.party(model_tree_prune))

# prediction

model_tree_predict <- predict(model_tree_prune, newdata = test, type = 'vector')
head(model_tree_predict)
head(test$Item_Outlet_Sales)

# Validation

library(hydroGOF)
RMSE_tree <- rmse(model_tree_predict, test$Item_Outlet_Sales)
RMSE_tree

# RANDOM FOREST

library(randomForest)
set.seed(2)
model_forest <- randomForest(Item_Outlet_Sales ~ ., data = train, ntree = 500)
plot(model_forest)

# optimization
model_forest$mse
min_error <- which.min(model_forest$mse)
min_error

# Prediction

model_forest_predict <- predict(model_forest, newdata = test, ntree = min_error)
head(model_forest_predict)
head(test$Item_Outlet_Sales)

# RMSE

library(hydroGOF)
RMSE_forest <- rmse(model_forest_predict, test$Item_Outlet_Sales)
RMSE_forest

#-------------------------------------------------------------------------------------------------------------

# SUPPORT VECTOR MACHINE

library("e1071")

model_svm_linear = svm(Item_Outlet_Sales ~ ., data = train, type = 'nu-regression', kernel = 'linear')

model_svm_linear_predict <- predict(model_svm_linear,newdata = test)

library("hydroGOF")
RMSE_svm_linear = rmse(model_svm_linear_predict,test$Item_Outlet_Sales)
RMSE_svm_linear # 0.5625724

model_svm_radial = svm(Item_Outlet_Sales ~ ., data = train, type = 'nu-regression', kernel = 'radial')

model_svm_radial_predict <- predict(model_svm_radial,newdata = test)

RMSE_svm_radial = rmse(model_svm_radial_predict,test$Item_Outlet_Sales)
RMSE_svm_radial # 0.5511917

model_svm_sigmoid = svm(Item_Outlet_Sales ~ ., data = train, type = 'nu-regression', kernel = 'sigmoid')

model_svm_sigmoid_predict <- predict(model_svm_sigmoid,newdata = test)

RMSE_svm_sigmoid = rmse(model_svm_sigmoid_predict,test$Item_Outlet_Sales)
RMSE_svm_sigmoid # 0.5707701

model_svm_polynomial = svm(Item_Outlet_Sales ~ ., data = train, type = 'nu-regression', kernel = 'polynomial')

model_svm_polynomial_predict <- predict(model_svm_polynomial,newdata = test)

RMSE_svm_polynomial = rmse(model_svm_polynomial_predict,test$Item_Outlet_Sales)
RMSE_svm_polynomial #  0.8287754

# From all the regression models support vactor machine(radial) has minimum RMSE
# Predict the external dataset on the model

external$Item_Identifier = as.numeric(external$Item_Identifier)
external_predict <- predict(model_svm_radial, newdata = external)
external_predict

predicted_dependent_variable = exp(external_predict)
predicted_dependent_variable

write.table(predicted_dependent_variable, file = 'G:/imarticus/R/Datasets/Test/predicted_retail.csv', sep = ',', row.names = FALSE)
