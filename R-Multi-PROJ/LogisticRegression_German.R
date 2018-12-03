# Logistic Regression on German dataset
data <- read.csv("G:/imarticus/R/Datasets/German/German credit _Data_comma.csv") # German dataset
View(data)

# checking for missing values
colSums(is.na(data))
summary(data)
str(data)

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

# Addressing outliers with floor and ceiling
floors <- function(x){
  b <- summary(x)[2]
  c <- summary(x)[5]
  f <- b - 1.5*(c - b)
  return (f)
}
ceilings <- function(x){
  d <- summary(x)[2]
  e <- summary(x)[5]
  t <- e + 1.5*(e - d)
  return(t)
}

# Floor and ceiling for Duration.of.credit.Month
data$Duration.of.Credit..month. <- ifelse(data$Duration.of.Credit..month.>ceilings(data$Duration.of.Credit..month.),ceilings(data$Duration.of.Credit..month.),data$Duration.of.Credit..month.)
boxplot(data[,c(3,6,9,12,14,17,19)])

# Floor and ceiling for Credit amount
data$Credit.Amount <- ifelse(data$Credit.Amount>ceilings(data$Credit.Amount),ceilings(data$Credit.Amount),data$Credit.Amount)
boxplot(data[,c(3,6,9,12,14,17,19)])

# Floor and ceiling for Age.Years
data$Age..years. <- ifelse(data$Age..years.>ceilings(data$Age..years.),ceilings(data$Age..years.),data$Age..years.)
boxplot(data[,c(3,6,9,12,14,17,19)])

# Floor and ceiling for No.of.Credites.at.this.bank
data$No.of.Credits.at.this.Bank <- ifelse(data$No.of.Credits.at.this.Bank>ceilings(data$No.of.Credits.at.this.Bank),ceilings(data$No.of.Credits.at.this.Bank),data$No.of.Credits.at.this.Bank)
boxplot(data[,c(3,6,9,12,14,17,19)])

# Floor and ceiling for No.of Dependnets
data$No.of.dependents <- ifelse(data$No.of.dependents>ceilings(data$No.of.dependents),ceilings(data$No.of.dependents),data$No.of.dependents)
boxplot(data[,c(3,6,9,12,14,17,19)])

# Outliers are removed

# VARIABLE SELECTION
attach(data)

# 1) Linear relationship between dependent and independnet variables

chisq.test(Creditability,Account.Balance) # significant
t.test(Duration.of.Credit..month.~Creditability) # significant
chisq.test(Payment.Status.of.Previous.Credit,Creditability) # significant
chisq.test(Creditability,Purpose) # significant
t.test(Credit.Amount~Creditability) # significant
chisq.test(Creditability,Value.Savings.Stocks) # significant
chisq.test(Length.of.current.employment,Creditability) # significant
t.test(Instalment.per.cent~Creditability) # significant
chisq.test(Sex...Marital.Status,Creditability) # significant
chisq.test(Guarantors,Creditability) # significant
t.test(Duration.in.Current.address~Creditability) # Non-significant
chisq.test(Most.valuable.available.asset,Creditability) # significant
t.test(Age..years.~Creditability) # significant
chisq.test(Concurrent.Credits,Creditability) # significant
chisq.test(Type.of.apartment,Creditability) # significant
t.test(No.of.Credits.at.this.Bank~Creditability) # Non-significant
chisq.test(Occupation,Creditability) # Non-significant
t.test(No.of.dependents~Creditability) # It is not necessary we remove this column
data <- data[,-19]
View(data)
chisq.test(Telephone,Creditability) # Non-significant
chisq.test(Foreign.Worker,Creditability) # significant

# 2) No relationship between the independnet variables (No multicollinearity)

summary(aov(Duration.of.Credit..month.~Account.Balance)) # significant ( Any one)
chisq.test(Account.Balance,Payment.Status.of.Previous.Credit) # significant (Any one)
chisq.test(Account.Balance,Purpose) # significant (Any one)
summary(aov(Credit.Amount~Account.Balance)) # significant (Any one)
chisq.test(Value.Savings.Stocks,Account.Balance) # signifiant (Any one)
chisq.test(Length.of.current.employment,Account.Balance) # Non-signifiant
summary(aov(Instalment.per.cent~Account.Balance)) # Non-significant
chisq.test(Sex...Marital.Status,Account.Balance) # non-significant
chisq.test(Guarantors,Account.Balance) # signifiant (Any one)
summary(aov(Duration.in.Current.address~Creditability)) # Non-significant
chisq.test(Most.valuable.available.asset,Creditability) # Non-significant
summary(aov(Age..years.~Creditability)) # significant (Any one)
chisq.test(Concurrent.Credits,Creditability) # signifiacant (Any one)
chisq.test(Type.of.apartment,Creditability) # significant (Any one)
summary(aov(No.of.Credits.at.this.Bank~Creditability)) # non-signifiant
chisq.test(Occupation,Creditability) # Non-significant
chisq.test(Telephone,Creditability) # non-significant
chisq.test(Foreign.Worker,Creditability) # significant ( Any one)

# Account.balance is considered

summary(aov(Instalment.per.cent~Length.of.current.employment)) # significant (Any one)

# Account.balance,Instalment.Per.Cent is considerd

summary(Instalment.per.cent~Sex...Marital.Status) # significnat (Any one)
cor.test(Instalment.per.cent,Duration.in.Current.address) # non-significant

# Account.balance,Instalment.Per.Cent, Duration.in.Current.address is considerd

summary(aov(Instalment.per.cent~Most.valuable.available.asset)) # Non-significant

# Account.balance,Instalment.Per.Cent, Duration.in.Current.address, Most.valuable.available.asset is considerd

cor.test(Instalment.per.cent,No.of.Credits.at.this.Bank) # Non-signifiant

#Account.balance,Instalment.Per.Cent, Duration.in.Current.address, Most.valuable.available.asset,No.of.Credits.at.this.Bank is considerd

summary(aov(Instalment.per.cent~Occupation)) # significant (Any one)
summary(aov(Instalment.per.cent~Telephone)) # Non-signicant

#Account.balance,Instalment.Per.Cent, Duration.in.Current.address, Most.valuable.available.asset,No.of.Credits.at.this.Bank, Telephone is considerd

# SPLITING THE DATA FOR TRAINING AND TESTING
library("caTools")
set.seed(2)
spliting <- sample.split(Creditability,SplitRatio = 0.8)
train <- subset(data, spliting == TRUE)
test <- subset(data, spliting == FALSE)

# BUILDING A MODEL
model <- glm(Creditability ~ Account.Balance + Instalment.per.cent + Duration.in.Current.address + Most.valuable.available.asset
             + No.of.Credits.at.this.Bank + Telephone, data = train, family = binomial(link = 'logit'))
summary(model)

model1 <- glm(Creditability ~ .,data = train, family = binomial(link = 'logit'))
summary(model1)

step(model1,direction = 'both') # StepAIC

# From the stepAIC we got a model and choose a less AIC model

model_aic <- glm(formula = Creditability ~ Account.Balance + Duration.of.Credit..month. + 
                   Payment.Status.of.Previous.Credit + Purpose + Value.Savings.Stocks + 
                   Length.of.current.employment + Instalment.per.cent + Sex...Marital.Status + 
                   Age..years. + Concurrent.Credits, family = binomial(link = "logit"), 
                 data = train)
summary(model_aic)

model_filter <- glm(Creditability ~ Account.Balance + Duration.of.Credit..month. + Payment.Status.of.Previous.Credit +
                      Purpose + Value.Savings.Stocks + Length.of.current.employment + Instalment.per.cent + Concurrent.Credits, family = binomial(link = "logit"),data = train)

summary(model_filter)

# PREDICTION

model_predict <- predict(model_aic,newdata = test, type = 'response')
model_predict

# Every value are in probability we covert them into 0 and 1

model_predict <- ifelse(model_predict>0.5,1,0)
model_predict

# VALIDATION

# All the predicte values are converted into 0 and 1

# Precision

precision <- function(x,y){
  s <- table(x,y)
  p <- s[2,2] / ( s[2,2] + s[2,1] )
  return (c(s,p))
  
}
model_precision <- precision(model_predict,test$Creditability)
model_precision

accuracy <- function(x,y){
  j <- table(x,y)
  a <- ( j[1,1] + j[2,2]) / ( j[1,1] + j[1,2] + j[2,1] + j[2,2] )
  return (c(j,a))
}
model_accuracy <- accuracy(model_predict,test$Creditability)
model_accuracy

# ROC Curve

library("ROCR")

#-----------------------------------------------------------------------------------------------------------------------------
model_predict1 <- predict(model_aic,newdata = train, type = 'response')
ROCPrediction <- prediction(model_predict1,train$Creditability)
ROCPerformance <- performance(ROCPrediction,"tpr","fpr")
plot(ROCPerformance, colorize = TRUE, print.cutoffs.at = seq(from=0.1, to=1, by=0.1))

# The aim of the ROC Curve is to choose high true positive rate and less false positive rate
# In this case we go with 0.7 because it has more true positives and less false positives

model_predict_roc <- predict(model_aic, newdata = test, type = 'response')
model_predict_roc
model_predict_roc <- ifelse(model_predict_roc > 0.7, 1, 0)
model_predict_roc

precision_ROC <- precision(model_predict_roc,test$Creditability) 
precision_ROC
accuracy_ROC <- precision(model_predict_roc,test$Creditability)
accuracy_ROC

#-----------------------------------------------------------------------------------------------------------------------------

# Compare to Previous our accuracy and Precision has improved and also False Negatives has decreased

# But for this case false positives has to be reduced ( Bectaul Actual there is a no creditability but our model predicted as creditablity)
# From this go thereshold with 0.5 is more good 

pre = prediction(model_predict,test$Creditability)
per = performance(pre, measure = "tpr", x.measure = "fpr")
plot(per)
per_auc = performance(pre,measure = "auc")
per_auc
per_auc = per_auc@y.values[[1]]
per_auc



# Decision Trees
library("rpart")
model_tree = rpart(Creditability ~ ., data = train)
model_tree
plot(model_tree)
text(model_tree)

library("partykit")
plot(as.party(model_tree))

# Pruning the trees
model_tree$cptable

cp7 = model_tree$cptable[7,1]
cp7

cp8 = model_tree$cptable[8,1]
cp8

model_prune_cp7 = prune(model_tree, cp = cp7)
model_prune_cp7

model_prune_cp8 = prune(model_tree, cp = cp8)
model_prune_cp8

plot(as.party(model_prune_cp7))
plot(as.party(model_prune_cp8))

# Prediction

model_predict_cp7 = predict(model_prune_cp7, newdata = test, type = 'class')
model_predict_cp8 = predict(model_prune_cp8, newdata = test, type = 'class')

# Validation

precision_cp7 = precision(model_predict_cp7, test$Creditability)
precision_cp7

precision_cp8 = precision(model_predict_cp8, test$Creditability)
precision_cp8

# We go with model_predict_cp7 because both are some (cp7  and  cp8)

accuracy_cp7 = accuracy(model_predict_cp7, test$Creditability)
accuracy_cp7
