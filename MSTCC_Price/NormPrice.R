rm(list = ls())

library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(timeSeries)
library(xtable)
library(igraph)
library(tcltk2)
library(MTS)
library(matrixcalc)
library(Matrix)
library(fPortfolio)
library(IntroCompFinR)  #install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
require(quadprog)
library(pracma)
library(glasso)

setwd("~/Documents/METIS/Minimum Spanning Tree/Codes/MSTCC_Price")

# import normalized prices
norm_prices<-read.table("def1.csv", header=TRUE, sep=",", dec=".")
ZOO_norm<- zoo(norm_prices[,-1], order.by=as.Date(as.character(norm_prices$Date), format='%m/%d/%Y'))
# to plot normalized prices
png("Norm_Price_1.png", width=500, height=400, bg = "transparent")
plot(ZOO_norm[,c(1,2,4,5,6)], ylab="normalized prices",xlab="",plot.type= "single",col=c("#000000","#FF0000","#00FF00","#9900FF","#00FFFF"))
dev.off()
# to plot normalized prices
png("Norm_Price_2.png", width=500, height=400, bg = "transparent")
plot(ZOO_norm[,c(3,7,8,9,10)], ylab="normalized prices",xlab="", plot.type= "single", col=c("#FF00FF","#00790E","#CCCCCC","#FF6600","#0000FF"))
dev.off()

# import price
prices<-read.table("def.csv", header=TRUE, sep=",", dec=".")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Date), format='%m/%d/%Y'))
head(prices)

# returns
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
class(return)
dim(return)

# summary statistics
summary<-cbind(colMeans(return),colStdevs(return),colSkewness(return),colKurtosis(return))
colnames(summary)<-c("Mean", "std", "skewness","kurtosis")
xtable(round(summary,4))
summary

