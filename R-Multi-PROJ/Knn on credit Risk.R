# Credit Risk
data <- read.csv( "G:/imarticus/R/Datasets/credit risk/R_Module_Day_10.2_Credit_Risk_Train_data (1).csv",na.strings = c(" ","","NA"))
View(data)
data <-  data[,-1]
data[,c(9,10)] = lapply(data[,c(9,10)], FUN = as.factor)

modes = function(x){
  t <- table(x)
  m <- names(t)[t == max(t)]
}

for(i in 1:length(data)){
  if(is.factor(data[,i]))
  {
    data[,i][is.na(data[,i])] = modes(data[,i])
  }
  else
  {
    data[,i][is.na(data[,i])] = median(data[,i], na.rm = TRUE)
  }
}

any(is.na(data))

boxplot(data[,6:8])

normalize <- function(x) {
  return ( ( x - min(x) ) / ( max(x) - min(x) ) )
}

for(j in 1:length(data)){
  if(!(is.factor(data[,j])))
  {
    data[,j] = normalize(data[,j])
  }
  else
  {
    data[,j] = data[,j]
  }
}

# Testing Dataset

test<- read.csv("G:/imarticus/R/Datasets/credit risk/R_Module_Day_8.2_Credit_Risk_Validate_data.csv", na.strings = c(" ","","NA") )
View(test)

# 1) Removing the irrelevant variable

test <-  test[,-1]

# 2) Converting to a factor

test[,c(9,10)] = lapply(test[,c(9,10)], FUN = as.factor)

# 3) Checking for missing values

any(is.na(test))
colSums(is.na(test))

for(m in 1:length(test)){
  if(is.factor(test[,m]))
  {
    test[,m][is.na(test[,m])] = modes(test[,m])
  }
  else
  {
    test[,m][is.na(test[,m])] = median(test[,m], na.rm = TRUE)
  }
}

# 4) checking for the scaling of quantitaive variables

boxplot(test[,6:8])

for(n in 1:length(test)){
  if(!(is.factor(test[,n])))
  {
    test[,n] = normalize(test[,n])
  }
  else
  {
    test[,n] = test[,n]
  }
}

boxplot(test[,6:8])

# External Dataset

external = read.csv("G:/imarticus/R/Datasets/credit risk/R_Module_Day_10.3_Credit_Risk_Test_data.csv", na.strings = c(" ","","NA"))
View(external)

# 1) Removing the irrelevant variable

external <-  external[,-1]

# 2) Converting to a factor

external[,c(9,10)] = lapply(external[,c(9,10)], FUN = as.factor)

# 3) Checking for missing values

any(is.na(external))
colSums(is.na(external))

for(o in 1:length(external)){
  if(is.factor(external[,o]))
  {
    external[,o][is.na(external[,o])] = modes(external[,o])
  }
  else
  {
    external[,o][is.na(external[,o])] = median(external[,o], na.rm = TRUE)
  }
}

# 4) checking for the scaling of quantitaive variables

boxplot(external[,6:8])

for(p in 1:length(external)){
  if(!(is.factor(external[,p])))
  {
    external[,p] = normalize(external[,p])
  }
  else
  {
    external[,p] = external[,p]
  }
}

boxplot(external[,6:8])

# Knn

#library("class")
#n = sqrt(100)
#data[,12] <- lapply(data[,12], as.numeric)
#test[,12] <- lapply(test[,12],as.numeric)


 z <- function(x)
    {
    if(x[a,b] == 'Urban')
{
  return(0)
}
else if(x[a,b]=='Semiurban')
{
  return(1)
}
else
  return(2)
 }
 
for (c in 1:614){
  data[c,11] = z(data[c,11])
}
 
 

 
 
model_knn = knn(train = data[,-12], test = test[,-12], cl = data[,12], k = round(n) )
model_knn

table(model_knn,test[,12])
