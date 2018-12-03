# German Dataest
data <- read.csv("G:/imarticus/R/Datasets/German/German credit _Data_comma.csv") # German dataset
View(data)
any(is.na(data))
# Converting a variables to categorical (by the definition in the word file german)
data$Creditability = as.factor(data$Creditability)
levels(data$Creditability)
data$Account.Balance = as.factor(data$Account.Balance)
levels(data$Account.Balance)
data$Payment.Status.of.Previous.Credit = as.factor(data$Payment.Status.of.Previous.Credit)
levels(data$Payment.Status.of.Previous.Credit)
data$Purpose = as.factor(data$Purpose)
levels(data$Purpose)
data$Value.Savings.Stocks = as.factor(data$Value.Savings.Stocks)
levels(data$Value.Savings.Stocks)
data$Length.of.current.employment = as.factor(data$Length.of.current.employment)
levels(data$Length.of.current.employment)
data$Sex...Marital.Status = as.factor(data$Sex...Marital.Status)
levels(data$Sex...Marital.Status)
data$Guarantors = as.factor(data$Guarantors)
levels(data$Guarantors)
data$Most.valuable.available.asset = as.factor(data$Most.valuable.available.asset)
levels(data$Most.valuable.available.asset)
data$Concurrent.Credits = as.factor(data$Concurrent.Credits)
levels(data$Concurrent.Credits)
data$Type.of.apartment = as.factor(data$Type.of.apartment)
levels(data$Type.of.apartment)
data$Occupation = as.factor(data$Occupation)
levels(data$Occupation)
data$Telephone = as.factor(data$Telephone)
levels(data$Telephone)
data$Foreign.Worker = as.factor(data$Foreign.Worker)
levels(data$Foreign.Worker)

# needed independent variables are coverted to categorical variables
str(data)

# Checking for outliers in the quantative variables
boxplot(data[,c(3,6,9,12,14,17,19)])
summary(data)

# Values are not in the same range
# Normalization 
normalize <- function(x){
  a <- ( x - min(x) ) / ( max(x) - min(x) )
  return(a)
}
data[,c(3,6,9,12,14,17,19)] <- lapply(data[,c(3,6,9,12,14,17,19)], FUN = normalize)
boxplot(data[,c(3,6,9,12,14,17,19)])

# Spliting the data for training and testing

set.seed(123)
library(caTools)
split = sample.split(train[,1], SplitRatio = 0.7)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

# Model

library("class")
n = sqrt(705)
model <- knn(train = train[,-1], test = test[,-1], cl = train[,1],k = round(n))
model

Accuracy = mean(model == test[,1])
Accuracy

# Optimization

Accuracy_pre <- vector()

for(i in 1:30){
  model1 <- knn(train = train[,-1], test = test[,-1], cl = train[,1],k = i)
  Accuracy_pre[i] <- mean(model1 == test[,1])
}

a <- c(1:30)
plot(a,Accuracy_pre, type = 'b', xaxt = "n")
axis(1,at = seq(from = 1, to = 30, by = 2), las =1)
abline(v = which.max(Accuracy_pre))

model_knn = knn(train = train[,-1], test = test[,-1], cl = train[,1], k = 5)
Accuracy_knn = mean(model_knn == test[,1])
Accuracy_knn


# Naive Bayes

library(e1071)
model_bayes <- naiveBayes(x = train[,-1], y = train[,1])
model_bayes

# Prediction

model_predict <- predict(model_bayes,newdata = test, type = 'class')
model_predict

# Accuracy

Accuracy_bayes = mean(model_predict == test[,1])
Accuracy_bayes

