dat<-read.csv("file:///C:/Users/RAKESH REDDY/Downloads/Tractor-Sales.csv")
library(forecast)

data<-ts(dat[,2],start = c(2003,1),frequency = 12)
data

plot(data,xlab="Year",ylab="")