# Regression Tress
library("ElemStatLearn")
data <- prostate
View(data)

# Checking for missing values
colSums(is.na(data))

# Spliting The data
train = subset(data, data$train == TRUE)[,1:9]
test = subset(data, data$train == FALSE)[,1:9]

# MODEL
library("rpart")
model <- rpart(lpsa ~ ., data = train)
plot(model)
text(model)

library("partykit")
plot(as.party(model))

model$cptable # For pruning

# choosing the fifth one

cp5 <- model$cptable[5,1]
cp5

# PRUNE

model_prune <- prune(model, cp = cp5)
plot(as.party(model_prune))

# Predict

model_predict <- predict(model_prune, newdata = test, type = "vector")
model_predict

# RMSE
library("hydroGOF")
RMSE <- rmse(model_predict, test$lpsa)
RMSE

# RANDOM FOREST
library("randomForest")

# Model
set.seed(2)

training <- data[data$train == 'TRUE',]
testing <- data[data$train == 'FALSE',]

model_tree <- randomForest(lpsa ~.-train, data = training, ntree = 500)
model_tree

plot(model_tree)

# Reducing the number of trees with the least error
which.min(model_tree$mse)

# build the model with 354 trees
set.seed(2)
model_tree1 <- randomForest(lpsa ~.-train, data = training, ntree = 354)
model_tree1

varImpPlot(model_tree1, scale = TRUE)
importance(model_tree1)

# Prediction
model_forest_predict <- predict(model_tree1, newdata = testing)
head(model_forest_predict)
head(testing$lpsa)

# RMSE
RMSE_forest <- rmse(model_forest_predict, testing$lpsa)
RMSE_forest


# My
set.seed(123)
modela <- randomForest(lpsa ~., data = train)
modela
plot(modela)
which.min(modela$mse) #423
tune <- tuneRF(train[,-c(9,10)],train[,9], stepFactor = 0.5, plot = TRUE, ntreeTry = 423, trace = TRUE, improve = 0.05)
modelb <- randomForest(lpsa ~., data = train, ntree = 300, mtry = 2)
modelb
varImpPlot(modelb)
varUsed(modelb)
getTree(modelb,1,labelVar = TRUE)
