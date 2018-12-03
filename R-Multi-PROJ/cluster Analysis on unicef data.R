# Cluster analysis
library("ks")
data("unicef")
View(unicef)

colSums(is.na(unicef))

boxplot(unicef)

normalize <- function(x) {
  return ( ( x - min(x) ) / ( max(x) - min(x) ) )
}
unicef[,c(1,2)] = lapply(unicef[,c(1,2)], FUN = normalize)
summary(unicef)
boxplot(unicef)

# K means clustering
set.seed(123)
fit <- kmeans(unicef,3)
attributes(fit)
fit$withinss
fit$cluster

# To know the value of k

wssplot <- function(x,n,seed = 123){
  wss <- kmeans(unicef,1)$withinss
  for(i in 2:n){
    set.seed(seed)
    wss[i] = sum(kmeans(x,centers = i)$withinss)
  }
  plot(1:n,wss,type = 'b')
}
wssplot(unicef,n=15) # assigning any number for n

# model fitting

fitkm <- kmeans(unicef,4)
fitkm$cluster


# Another method: Average silhouthe method
library("fpc")
fit_avg <- kmeansruns(unicef,krange = 2:15,criterion = 'asw', critout = FALSE, runs = 100)
fit_avg

# clustering plot
library("cluster")
clusplot(unicef,fit_avg$cluster,color = TRUE, shade = TRUE,labels = 2, lines = 0, main = "Cluster plot")

unicef <- data.frame(unicef,fit_avg$cluster)

boxplot(unicef$Under.5 ~ fit_avg$cluster)
summary(aov(unicef$Under.5 ~ fit_avg$cluster))
tapply(unicef$Under.5,fit_avg$cluster,FUN = mean)

boxplot(unicef$Ave.life.exp ~ fit_avg$cluster)
summary(aov(unicef$Ave.life.exp ~ fit_avg$cluster))
tapply(unicef$Ave.life.exp,fit_avg$cluster, FUN = mean)
