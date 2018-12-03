library(rpart)
library("ElemStatLearn")

data(prostate)
view(prostate)
colsums(is.na(prostate))
pros.train<-subset(prostate,train==TRUE)[,1:9]
pros.test<-subset(prostate,train==FALSE)[,1:9]

tree.pros<-rpart(lpsa ~.,data=pros.train)
print(tree.pros$cptable)
plotcp(tree.pros)
cp<-min(tree.pros$cptable[5,1])



prune.tree<-prune(tree.pros,cp=cp)

library("partykit")

plot(as.party(tree.pros))
plot(as.party(prune.tree))

party.pros.test<-predict(prune.tree,newdata=pros.test)

library(hydroGOF)
rmse1<-rmse(party.pros.test,pros.test$lpsa)
rmse1

modelplm<-lm(lpsa~lcavol+lweight,data=pros.train)
summary(modelplm)

plm<-predict(modelplm,data=pros.test)
rmse(pros.train$lpsa,plm)










