library("MASS")
data <- biopsy
View(biopsy)
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
# No need for outliers

boxplot(data[,1:9])

# Spliting the data into testing and training

set.seed(123)
split = sample(2,nrow(data), prob = c(0.7,0.3), replace = TRUE)
train = data[split == 1,]
test = data[split == 2,]

# Building the model

library("class")
n = sqrt(488)
model <- knn(train = train[,-10], test = test[,-10], cl = train[,10], k = round(n) )
model

Accuracy = mean(model == test$class)
Accuracy

# Optimizing the model by choosing the correct value of n by grid search method
p = 0
Accuracy_pred <- vector()
for(i in seq(from = 1, to = 488, by = 5)){
  p = p+1
  print(p)
  prediction <- knn(train = train[,-10],test = test[,-10], cl = train[,10], k = i)
  Accuracy_pred[p] <- mean(prediction == test$class)
}


a = seq(from = 1, to = 488, by = 5)
plot(a,Accuracy_pred)

# Optimization

model1 <- knn(train = train[,-10], test = test[,-10], cl = train[,10], k = 6)
model1

Accuracy1 <- mean(model1 == test[,10])
Accuracy1
