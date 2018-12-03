library(caTools)#used for spliiting
library(rpart)#used for decission trees
library(partykit)
library(randomForest)#random forest
library(hydroGOF)# rmse
library(Amelia)# miss map graph
library(class)# Knn
library(e1071)# Naive bayes
library(ElemStatLearn)
cr=read.csv("file:///C:/Users/RAKESH REDDY/Downloads/R_Module_Day_10.2_Credit_Risk_Train_data (1).csv",na.strings = c(""," ","NA"))
cr1=read.csv("file:///C:/Users/RAKESH REDDY/Downloads/R_Module_Day_10.3_Credit_Risk_Test_data (1).csv",na.strings = c(""," ","NA"))
cr2=read.csv("file:///C:/Users/RAKESH REDDY/Downloads/R_Module_Day_8.2_Credit_Risk_Validate_data.csv",na.strings = c(""," ","NA"))
colSums(is.na(cr))
View(cr)
cr=cr[,-1]
#removing missing values of train data set
cr$LoanAmount[is.na(cr$LoanAmount)]=median(cr$LoanAmount,na.rm=T)
cr$Loan_Amount_Term[is.na(cr$Loan_Amount_Term)]=median(cr$Loan_Amount_Term,na.rm=T)
cr$Credit_History[is.na(cr$Credit_History)]=mode(cr$Credit_History)
cr$Gender[is.na(cr$Gender)]=mode(cr$Gender)
cr$Married[is.na(cr$Married)]=mode(cr$Married)
cr$Dependents[is.na(cr$Dependents)]=mode(cr$Dependents)
cr$Self_Employed[is.na(cr$Self_Employed)]=mode(cr$Self_Employed)
cr$Credit_History[is.na(cr$Credit_History)]=mode(cr$Credit_History)

any(is.na(cr))

#removing missing values of validate data set
colSums(is.na(cr1))
cr1$LoanAmount[is.na(cr1$LoanAmount)]=median(cr1$LoanAmount,na.rm=T)
cr1$Loan_Amount_Term[is.na(cr1$Loan_Amount_Term)]=median(cr1$Loan_Amount_Term,na.rm=T)
cr1$Credit_History[is.na(cr1$Credit_History)]=mode(cr1$Credit_History)
cr1$Gender[is.na(cr1$Gender)]=mode(cr1$Gender)
cr1$Married[is.na(cr1$Married)]=mode(cr1$Married)
cr1$Dependents[is.na(cr1$Dependents)]=mode(cr1$Dependents)
cr1$Self_Employed[is.na(cr1$Self_Employed)]=mode(cr1$Self_Employed)
cr1$Credit_History[is.na(cr1$Credit_History)]=mode(cr1$Credit_History)

any(is.na(cr1))

#removing missing values of test data set
colSums(is.na(cr2))

cr2$LoanAmount[is.na(cr2$LoanAmount)]=median(cr2$LoanAmount,na.rm=T)
cr2$Loan_Amount_Term[is.na(cr2$Loan_Amount_Term)]=median(cr2$Loan_Amount_Term,na.rm=T)
cr2$Credit_History[is.na(cr2$Credit_History)]=mode(cr2$Credit_History)
cr2$Gender[is.na(cr2$Gender)]=mode(cr2$Gender)
cr2$Married[is.na(cr2$Married)]=mode(cr2$Married)
cr2$Dependents[is.na(cr2$Dependents)]=mode(cr2$Dependents)
cr2$Self_Employed[is.na(cr2$Self_Employed)]=mode(cr2$Self_Employed)
cr2$Credit_History[is.na(cr2$Credit_History)]=mode(cr2$Credit_History)

any(is.na(cr2))

#checking for outliers
boxplot(cr[,c(6,7,8,9)])
summary(cr$ApplicantIncome)

q1=2878
q3=5795
floor=q1-1.5*(q3-q1)
ceiling=q3+1.5*(q3-q1)
cr$ApplicantIncome<-ifelse(cr$ApplicantIncome<floor,floor,cr$ApplicantIncome)
cr$ApplicantIncome<-ifelse(cr$ApplicantIncome>ceiling,ceiling,cr$ApplicantIncome)
boxplot(cr[,6])

summary(cr$CoapplicantIncome)

q1=0
q3=2297
floor=q1-1.5*(q3-q1)
ceiling=q1+1.5*(q3+q1)
cr$CoapplicantIncome<-ifelse(cr$CoapplicantIncome<floor,floor,cr$CoapplicantIncome)
cr$CoapplicantIncome<-ifelse(cr$CoapplicantIncome>ceiling,ceiling,cr$CoapplicantIncome)
boxplot(cr[,7])


summary(cr$LoanAmount)

q1=100.2
q3=164.8
floor=q1-1.5*(q3-q1)
ceiling=q1+1.5*(q3-q1)
cr$LoanAmount<-ifelse(cr$LoanAmount<floor,floor,cr$LoanAmount)
cr$LoanAmount<-ifelse(cr$LoanAmount>ceiling,ceiling,cr$LoanAmount)
boxplot(cr[,8])

summary(cr$Loan_Amount_Term)

q1=360
q3=360
floor=q1-1.5*(q3-q1)
ceiling=q1+1.5*(q3-q1)
cr$Loan_Amount_Term<-ifelse(cr$Loan_Amount_Term<floor,floor,cr$Loan_Amount_Term)
cr$Loan_Amount_Term<-ifelse(cr$Loan_Amount_Term>ceiling,ceiling,cr$Loan_Amount_Term)
boxplot(cr[,9])

boxplot(cr)

#removing outlires for test data
View(cr1)
cr1=cr1[,-1]
boxplot(cr1[,c(6,7,8,9)])

summary(cr1$ApplicantIncome)

q1=2864
q3=5060
floor=q1-1.5*(q3-q1)
ceiling=q3+1.5*(q3-q1)
cr1$ApplicantIncome<-ifelse(cr1$ApplicantIncome<floor,floor,cr1$ApplicantIncome)
cr1$ApplicantIncome<-ifelse(cr1$ApplicantIncome>ceiling,ceiling,cr1$ApplicantIncome)
boxplot(cr1[,6])

summary(cr1$CoapplicantIncome)

q1=0
q3=2430
floor=q1-1.5*(q3-q1)
ceiling=q1+1.5*(q3+q1)
cr1$CoapplicantIncome<-ifelse(cr1$CoapplicantIncome<floor,floor,cr1$CoapplicantIncome)
cr1$CoapplicantIncome<-ifelse(cr1$CoapplicantIncome>ceiling,ceiling,cr1$CoapplicantIncome)
boxplot(cr1[,7])


summary(cr1$LoanAmount)

q1=101
q3=157.5
floor=q1-1.5*(q3-q1)
ceiling=q1+1.5*(q3-q1)
cr1$LoanAmount<-ifelse(cr1$LoanAmount<floor,floor,cr1$LoanAmount)
cr1$LoanAmount<-ifelse(cr1$LoanAmount>ceiling,ceiling,cr1$LoanAmount)
boxplot(cr1[,8])

summary(cr1$Loan_Amount_Term)

q1=360
q3=360
floor=q1-1.5*(q3-q1)
ceiling=q1+1.5*(q3-q1)
cr1$Loan_Amount_Term<-ifelse(cr1$Loan_Amount_Term<floor,floor,cr1$Loan_Amount_Term)
cr1$Loan_Amount_Term<-ifelse(cr1$Loan_Amount_Term>ceiling,ceiling,cr1$Loan_Amount_Term)
boxplot(cr1[,9])

boxplot(cr1)


#removing outliers of validate data
colSums(is.na(cr2))
View(cr2)
cr2=cr2[,-1]
str(cr2)
boxplot(cr2[,c(6,7,8,9)])

summary(cr2$ApplicantIncome)

q1=2864
q3=5060
floor=q1-1.5*(q3-q1)
ceiling=q3+1.5*(q3-q1)
cr2$ApplicantIncome<-ifelse(cr2$ApplicantIncome<floor,floor,cr2$ApplicantIncome)
cr2$ApplicantIncome<-ifelse(cr2$ApplicantIncome>ceiling,ceiling,cr2$ApplicantIncome)
boxplot(cr2[,6])

summary(cr2$CoapplicantIncome)

q1=0
q3=2430
floor=q1-1.5*(q3-q1)
ceiling=q1+1.5*(q3+q1)
cr2$CoapplicantIncome<-ifelse(cr2$CoapplicantIncome<floor,floor,cr2$CoapplicantIncome)
cr2$CoapplicantIncome<-ifelse(cr2$CoapplicantIncome>ceiling,ceiling,cr2$CoapplicantIncome)
boxplot(cr2[,7])


summary(cr2$LoanAmount)

q1=101
q3=157.5
floor=q1-1.5*(q3-q1)
ceiling=q1+1.5*(q3-q1)
cr2$LoanAmount<-ifelse(cr2$LoanAmount<floor,floor,cr2$LoanAmount)
cr2$LoanAmount<-ifelse(cr2$LoanAmount>ceiling,ceiling,cr2$LoanAmount)
boxplot(cr2[,8])

summary(cr2$Loan_Amount_Term)

q1=360
q3=360
floor=q1-1.5*(q3-q1)
ceiling=q1+1.5*(q3-q1)
cr2$Loan_Amount_Term<-ifelse(cr2$Loan_Amount_Term<floor,floor,cr2$Loan_Amount_Term)
cr2$Loan_Amount_Term<-ifelse(cr2$Loan_Amount_Term>ceiling,ceiling,cr2$Loan_Amount_Term)
boxplot(cr2[,9])

boxplot(cr2)



#LOGISTIC REGRESSION
#Fitting the model for train data set
model=glm(Loan_Status~Gender+Married+Dependents+Education+Self_Employed
          +ApplicantIncome+CoapplicantIncome
           +Credit_History+Property_Area,family=binomial(link='logit'),data=cr)

summary(model)
#Fitting  the validate data,cr2=validate data set
modelval=predict(model,newdata = cr2,type='response')
modelval=ifelse(modelval>0.5,1,0)
table(modelval,cr2$outcome)
(58+289)/(58+1+19+289) #0.9455041

#roc curve
#as a last step,we are going to plot the roc curve and calculate the AUC(area under curve)
#a model with good predictive ability sould have an auc closerto 1(1 is ideal)
library(ROCR)
pr=prediction(modelval,cr2$outcome)
prf=performance(pr,measure="tpr",x.measure="fpr")
plot(prf)

#auc curve
auc=performance(pr,measure="auc")
auc1<-auc@y.values[[1]]
auc1

#predicting the test data,cr1=test data
modeltest=predict(model,newdata = cr1,type = 'response')

modeltest=ifelse(modeltest>0.5,1,0)
table(modeltest)#0=59,1=308



#Decission tree credit risk 
tree.cr=rpart(Loan_Status~.,data = cr) #train data
attach(cr)
tree.cr$cptable
cp=0.010000
prune.tree.cr=prune(tree.cr,cp=cp)
plot(as.party(prune.tree.cr))
attach(cr1)
pre.cr2=predict(prune.tree.cr,newdata = cr2,type="class")#predicting for validate data
table(pre.cr2,cr2$outcome)
(58+289)/(289+58+1+19) #0.9455041

pre.cr1=predict(prune.tree.cr,newdata = cr1,type = "class")#predicting for test data
newpred=data.frame(cr1,pre.cr1)
table(newpred$pre.cr1)#N=59,Y=308


#RANDOM FOREST
#install randomforest library
set.seed(123)
forest.cr=randomForest(Loan_Status~.,data=cr)
forest.cr
plot(forest.cr)
which.min(forest.cr$err.rate[,1])

forest1.cr=randomForest(Loan_Status~.,data = cr,ntree=228)
forest1.cr
varImpPlot(forest1.cr)
importance(forest1.cr)
#validate data
forest2.cr=predict(forest1.cr,newdata=cr2,type='class')
forest2.cr
table(forest2.cr,cr2$outcome)
(61+288)/(61+288+2+16)#0.9509537

#test data
forest3.cr=predict(forest.cr,newdata = cr1,type = 'class')
forest3.cr
table(forest3.cr)#N=62,Y=305

#KNN (K-NEAREST NEIGHBOUR)
#library(class)
View(cr)
cr[6:8]=scale(cr[6:8])
crt=cr
crt[6:8]=scale(crt[6:8])
#converting as.numeric train data
crt$Gender=as.numeric(crt$Gender)
crt$Married=as.numeric(crt$Married)
crt$Dependents=as.numeric(crt$Dependents)
crt$Education=as.numeric(crt$Education)
crt$Self_Employed=as.numeric(crt$Self_Employed)
crt$ApplicantIncome=as.numeric(crt$ApplicantIncome)
crt$CoapplicantIncome=as.numeric(crt$CoapplicantIncome)
crt$LoanAmount=as.numeric(crt$LoanAmount)
crt$Loan_Amount_Term=as.numeric(crt$Loan_Amount_Term)
crt$Credit_History=as.numeric(crt$Credit_History)
crt$Property_Area=as.numeric(crt$Property_Area)


#converting as.numeric validate data
crv=cr2
crv$Gender=as.numeric(crv$Gender)
crv$Married=as.numeric(crv$Married)
crv$Dependents=as.numeric(crv$Dependents)
crv$Education=as.numeric(crv$Education)
crv$Self_Employed=as.numeric(crv$Self_Employed)
crv$ApplicantIncome=as.numeric(crv$ApplicantIncome)
crv$CoapplicantIncome=as.numeric(crv$CoapplicantIncome)
crv$LoanAmount=as.numeric(crv$LoanAmount)
crv$Loan_Amount_Term=as.numeric(crv$Loan_Amount_Term)
crv$Credit_History=as.numeric(crv$Credit_History)
crv$Property_Area=as.numeric(crv$Property_Area)

#converting as.numeric test data
crte=cr1
crte$Gender=as.numeric(crte$Gender)
crte$Married=as.numeric(crte$Married)
crte$Dependents=as.numeric(crte$Dependents)
crte$Education=as.numeric(crte$Education)
crte$Self_Employed=as.numeric(crte$Self_Employed)
crte$ApplicantIncome=as.numeric(crte$ApplicantIncome)
crte$CoapplicantIncome=as.numeric(crte$CoapplicantIncome)
crte$LoanAmount=as.numeric(crte$LoanAmount)
crte$Loan_Amount_Term=as.numeric(crte$Loan_Amount_Term)
crte$Credit_History=as.numeric(crte$Credit_History)
crte$Property_Area=as.numeric(crte$Property_Area)

View(crt)
View(crv)
View(crte)
crt[6:8]=scale(crt[6:8])
crv[6:8]=scale(crv[6:8])
crte[6:8]=scale(crte[6:8])

n=520

modelknn=knn(train=crt[,-12],test=crte[,-12],cl=crt$Loan_Status,k=round(sqrt(n)) )
table(crv$outcome,modelknn)
(9+290)/(9+0+68+290)#0.8147139

k=1:30
for(x in k){
  prediction=knn(crt[,-12],crv[,-12], crt$Loan_Status,k=x)
  accuracy[x]=mean(prediction==crv[,12])
}

plot(k,accuracy,type = 'b')
accuracy

prediction1=knn(train = crt[,-12],test=crv[,-12],cl=crt$Loan_Status,k=17) #here 12=D.V
table(crv$outcome,prediction1)
(15+287)/(15+62+3+287)#0.8228883
error=mean(prediction1!=crv[,12])
accuracy=1-error
accuracy
#predicting outcomes on test data
prediction2=knn(train = crt[,-12],test=crte,cl=crt$Loan_Status,k=17)
newprediction=data.frame(crte,prediction2)
View(newprediction)

#NAIVE BAYES
#library (e1071)
#Naive Bayes method
 
nbmodel=naiveBayes(x=crt[-12],y=crt$Loan_Status)
nbmodel
#predict the test results
ypred=predict(nbmodel,newdata=crv[-12])
ypred


precision(crv[,12],ypred) #0.9965517
accuracy=mean(ypred==crv[,12])
accuracy #0.9455041


ypredfinal=predict(nbmodel,newdata=crte[-12])
newpredcr=data.frame(crte,ypredfinal)
View(newpredcr)


