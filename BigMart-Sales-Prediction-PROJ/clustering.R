library(fpc)
library(cluster)
library(ks)
data("unicef",package="ks")
colSums(is.na(unicef))
summary(unicef)
boxplot(unicef)
x<-scale(unicef)
boxplot(x)
set.seed(213)
fit<-kmeans(x,6)
attributes(fit)
wssplot<-function(data,nc,seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type="b",xlab="number of clusters",ylab="within groups sum of squares")
}
wssplot(x,nc=15)
fitkm<-kmeans(x,4)$cluster

fitasw<-kmeansruns(x,krange=2:15,criterion = "asw",critout = FALSE,runs=100)
fitasw$cluster

clusplot(x,fitasw$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

clusplot(x,fitkm,color=TRUE,shade=TRUE,labels=2,lines=0)





