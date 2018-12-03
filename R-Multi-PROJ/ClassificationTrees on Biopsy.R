# Classification Tress
library("MASS")
data <- biopsy
View(data)

# Removing the unnecessary columns
data <- data[,-1]

# checking for missing values
any(is.na(data))
colSums(is.na(data))

# V6 has missing values replace it with median
data$V6[is.na(data$V6)] = median(data$V6, na.rm = TRUE)
colSums(is.na(data)) # No missing values

# changing the column names
names_biopsy = c("thick","u.size","u.shape","adhsn","s.size","nucl","chrom","n.nuc","mit","class")
names(data) = names_biopsy
View(data)
str(data)

# checking for scaling
boxplot(data[,1:9]) 

# No need for scaling because every values are in the same range

# Removing outliers with floor and ceiling

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

for(i in 1:9){
  data[,i] = ifelse(data[,i]<floors(data[,i]),floors(data[,i]),data[,i])
  data[,i] = ifelse(data[,i]>ceilings(data[,i]),ceilings(data[,i]),data[,i])
}

boxplot(data[,1:9])

# Spliting the data into trianing and testing
library("caTools")
set.seed(2)
spliting = sample.split(data$class,SplitRatio = 0.8)
train = subset(data, spliting == TRUE)
test = subset(data, spliting == FALSE)

# Building a model
library("rpart")
model <- rpart(class ~.,data = train)
plot(model)
text(model)

# To view the plot nicely we use another library
library("partykit")
plot(as.party(model))

# Going for cross-validation by purning
model$cptable # ( From that we cannot choose the stabilize one we need to build the model with 2 and 3 splits and which give high accuracy we choose that )

# First based on 2nd cp we build the model
cp2 <- model$cptable[2,1]
cp2

prune_model_cp2 <- prune(model,cp = cp2)
plot(as.party(prune_model_cp2))

cp3 <- model$cptable[3,1]
cp3

prune_model_cp3 <- prune(model, cp = cp3)
plot(as.party(prune_model_cp3))

# Prediction

model_predict_cp2 <- predict(prune_model_cp2, newdata = test, type = 'class')
model_predict_cp2

model_predict_cp3 <- predict(prune_model_cp3, newdata = test, type = 'class')
model_predict_cp3

# Precision
precision = function(x,y){
  t <- table(x,y)
  p <- t[2,2] / ( t[2,1] + t[2,2] )
  return (p)
}

precision_cp2 <- precision(model_predict_cp2,test$class)
precision_cp2

precision_cp3 <- precision(model_predict_cp3,test$class)
precision_cp3

accuracy = function(x,y){
  t <- table(x,y)
  a <- ( t[2,2] + t[1,1] ) / ( t[1,1] + t[1,2] + t[2,1] + t[2,2])
  return(a)
}

accuracy_cp2 <- accuracy(model_predict_cp2,test$class)
accuracy_cp2

accuracy_cp3 <- accuracy(model_predict_cp3,test$class)
accuracy_cp3

confusionMatrix(model_predict_cp3,test$class)

# we choose cp3 model becuase it has high accuracy and precision

library("randomForest")
set.seed(2)
model_forest <- randomForest(class ~., data = train)
model_forest
plot(model_forest)

model_forest$err.rate
a=which.min(model_forest$err.rate[,1])

set.seed(2)
model_forest_tune <- randomForest(class ~., data = train, ntree = a )
model_forest_tune

# Prediction
model_forest_predict <- predict(model_forest_tune, newdata = test, type = 'class')
head(model_forest_predict)

head(test$class)

library("caret")
confusionMatrix(model_forest_predict, test$class)

varImpPlot(model_forest_tune)
varUsed(model_forest_tune)
importance(model_forest_tune)
