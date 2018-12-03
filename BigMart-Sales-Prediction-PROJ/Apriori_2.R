library(arules)
library(arulesViz)
setwd("C:\Users\RAKESH REDDY\Downloads")
data1 <- read.csv('file:///C:/Users/RAKESH REDDY/Downloads/Market_Basket_Optimisation.csv',header = FALSE)
View(data1)

data1=read.transactions('Market_Basket_Optimisation.csv',sep=',',rm.duplicates = TRUE)

class(data1)
View(data1)


itemFrequencyPlot(data1,topN=20)

rules=apriori(data=data1,parameter = list(support=0.04,confidence=0.2))
inspect(rules)

rules=sort(rules,by='lift',decreasing = TRUE)
ruleset=rules[1:10]

plot(ruleset,method="graph")








