dataset=read.csv("file:///C:/Users/RAKESH REDDY/Downloads/Social_Network_Ads.csv")
View(dataset)
dataset=dataset[2:5]
colSums(is.na(dataset))

dataset$Purchased=factor(dataset$Purchased)
dataset[,c(2,3)]=scale(dataset[,c(2,3)])

library(caTools)
set.seed(123)

split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

library(e1071)



svmmodel = svm(formula=Purchased~.,data=training_set,type='C-classification',kernel='radial')

y_pred=predict(svmmodel,newdata=test_set[-4])

cm=table(test_set[,4],y_pred)
cm

(58+22)/100 #linear
(53+23)/100 #sigmoid
(60+20)/100 #polynomial
(58+31)/100 #radial

svmmodel = svm(formula=Purchased~.,data=training_set,type='C-classification',kernel='radial')

y_pred=predict(svmmodel,newdata=test_set[-4])

cm=table(test_set[,4],y_pred)
cm


