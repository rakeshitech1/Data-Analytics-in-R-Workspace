wine_data<- read.csv("G:/imarticus/R/Datasets/wine/wine.csv")
View(wine_data)

wine_data <- wine_data[,-1]

names(wine_data) = c("alcohol","malic acid","ash","alkalinity of ash","magnesium","total phenols","flavonoids",
                     "non-flavonoid phenols","proanthocyanins","color intensity","hue","OD280/OD315","proline")
any(is.na(wine_data))

normalize <- function (x) {
  return ( ( x - min(x) ) / ( max(x) - min(x) ) )
}

wine_data[,] = lapply(wine_data[,], FUN = normalize)

# K means clustering

set.seed(123)
model <- kmeans(wine_data,centers = 5)
attributes(model)
model$withinss
model$cluster

# Going for elbow plot to fing centres
wss <- vector()
for(i in 1:20){
  set.seed(123)
  wss[i] = sum(kmeans(wine_data,centers = i)$withinss)
}
plot(1:20,wss)

# average silhouette method

library("fpc")

avg_sil <- kmeansruns(wine_data,1:20,criterion = 'asw',critout = FALSE, runs = 100)
avg_sil

#plotting

library("cluster")
clusplot(wine_data,avg_sil$cluster, color = TRUE, shade = TRUE, lines = 0, main = 'cluster plot for wine data',labels = 2)
