# install.packages('ndtv', dependencies=T)

# dynamic network

rm(list = ls())

setwd("~/Documents/Code/Metis_Minimum Spanning Tree/MSTCC_MST/update 20240622")

# Load Functions and other Files
# source('./PackagesNetworkPortfolio.R')
# source('./FunctionsNetworkPortfolio.R')

# load data
prices<-read.table("def.csv", header=TRUE, sep=",", dec=".")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Date), format='%m/%d/%Y'))

#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
p=dim(return)[2]

# rolling window
W<-list()
for(t in 0: 92){
  W[[(t+1)]]=returnstd[(1+t*7):(120+t*7),]
}

W_in<-list()
W_out<-list()

for(t in 1: 93){
  W_in[[(t)]]=W[[t]][c(1:113),]
  W_out[[(t)]]=W[[t]][c(114:120),]
}

T.windows<-length(W)
# correlation matrix, Expected return, covariance matrix
d=data.frame()
for (t in 1:T.windows) {
  d[t,1]=index(W_in[[t]])[1]
  d[t,2]=index(W_in[[t]])[dim(W_in[[t]])[1]]
}
write.table(d, "FromTo.txt",  sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)
