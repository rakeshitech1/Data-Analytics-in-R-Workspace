#install.packages('ISLR')
library(ISLR)
library(neuralnet)


View(College)
data("College")

scaled.data<-as.data.frame(scale(College[,2:18]))
colSums(is.na(scaled.data))

library(MASS)

Private=as.numeric(College$Private)-1
data=cbind(Private,scaled.data)

library(caTools)

set.seed(101)
split=sample.split(data$Private,SplitRatio = 0.70)

train=subset(data,split==TRUE)
test=subset(data,split==FALSE)

f<-names(scaled.data)
f<-paste(f,collapse = '+')
f<-paste('Private ~',f)

f<-as.formula(f)

str(train)

library(neuralnet)

nn<-neuralnet(f,train,hidden=c(3,2),linear.output=FALSE)
plot(nn)

predicted.nn.values<-compute(nn,test[2:18])

predicted.nn.values$net.result=ifelse(predicted.nn.values$net.result>=0.5,1,0)

cf<-table(test$Private,predicted.nn.values$net.result)

(59-163)/233

plot(nn)





