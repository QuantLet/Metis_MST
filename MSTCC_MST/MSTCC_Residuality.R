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

setwd("~/Documents/METIS/Minimum Spanning Tree/Codes/MSTCC_MST")
prices<-read.table("def.csv", header=TRUE, sep=",", dec=".")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Date), format='%m/%d/%Y'))

#return
return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
class(return)
dim(return)

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

# RMT: get lambda_max

M<-matrix(rnorm(10*113, mean=0,sd=1),10,113)
E<-t(M)
O<-M%*%E
L<-1/113
R<-L*O
eigen(R, symmetric = TRUE)
eigen_R<-eigen(R, symmetric = TRUE)$values
Q<-113/10

lambda_max<-(1+1/Q+2*sqrt(1/Q))   ##taking into account the behaviour of the first eigenvalue
lambda_max
lambda_min<-(1+1/Q-2*sqrt(1/Q))
lambda_min

# Get filtered eigenvalue and eigenvector
C <- list()        # correlation matrix
lambda_C<-list()   # eigenvalue and eigenvector of correlation matrix
eigen_C<-list()    # eigenvalue of correlation matrix
eigenvec_C<-list() # eigenvector of correlation matrix

for(t in 1: length(W_in)){
  #Wt[[(t)]]=returnstd[(t):(tw+t),]
  C[[(t)]] =cor(W_in[[(t)]])
  lambda_C[[t]]<-eigen(C[[t]], symmetric = TRUE)
  eigen_C[[t]]<-lambda_C[[t]]$values
  eigenvec_C[[t]]<-lambda_C[[t]]$vectors
}

# sort "eigen_C" by ascent
for(i in 1:10){
  for (t in 1:93) {
    if(eigen_C[[t]][i]<lambda_max){eigen_C[[t]][i]=0}
    eigen_C[[t]]<-sort(eigen_C[[t]])  # I think we do NOT need to sort
  }
}

filtered_diagonal_C<-list()  # filtered diagnal matrix of eigenvalue
V<-list()                    # eigenvector from 10 to 1
f<-list()                    # transport of V
C_1<-list()                  # filtered correlation matrix
Dist <- list()               # distance matrix

for(t in 1: length(W_in)){
  filtered_diagonal_C[[t]]<-diag(eigen_C[[t]])
  V[[t]]<-eigenvec_C[[t]]
  V[[t]]<-eigenvec_C[[t]][,10:1]  # I think we do NOT need to sort from 10 to 1
  f[[t]]<-t(V[[t]])
  C_1[[t]]<-V[[t]]%*%filtered_diagonal_C[[t]]%*%f[[t]]
  diag(C_1[[t]])<-1               # I think we do NOT need to sign 1 to diagonal 
  C_1[[t]]<-as.matrix(C_1[[t]])
  diag(C_1[[t]])<-1
  Dist[[t]]<-sqrt(2-2*C_1[[t]])
  Dist[[t]]<-as.matrix(Dist[[t]])
  Dist[[t]][is.nan(Dist[[t]])]<-0              # why nan? I think it may be set as inf
  colnames(Dist[[(t)]])<-colnames(returnstd)
  rownames(Dist[[(t)]])<-colnames(returnstd)
}
# what's the difference between Dist and ciao? I think it is the SAME
ciao<-list()
for(t in 1: length(W)){
  ciao[[t]]<-as.numeric(unlist(Dist[[t]]))
  ciao[[t]]<-matrix(ciao[[t]],10,10)
  colnames(ciao[[t]])<-colnames(returnstd)
  rownames(ciao[[t]])<-colnames(returnstd)
}

# create network of distance matrix
A<-list()
network<-list()  # network of filtered correlation matrix
Edgelist<-list() # edges of network
weight<-list()   # weight of network
links2<-list()   # links of network

for(t in 1: length(W)){
  network[[t]]=graph_from_adjacency_matrix(ciao[[t]],weighted=T, mode="undirected", diag=F)
  Edgelist[[t]]<-get.edgelist(network[[t]])
  weight[[t]]<-E(network[[t]])$weight
  A[[t]]<-cbind(Edgelist[[t]],weight[[t]])
  A[[t]]<-as.matrix(A[[t]])
  links2[[t]]<-as.data.frame(A[[t]])
  colnames(links2[[t]])<-c("from","to","weight")
}

# generate MST
weightmst<-list()      # maximum weight of MST
net<-list()            # net of data
mst<-list()            # minimum spanning tree
deg<-list()            # number of edges connecting to one point
root<-list()           # name of crypto with most edges in MST
deg_vert<-list()       # minus "deg"
centralization<-list() # centralize a graph according to the eigenvector centrality ofvertices
def_matrix<-list()
red<-list()
res<-list()
for(t in  1: length(W)){
  net[[t]] <- graph_from_data_frame(d=links2[[t]], directed=F)
  mst[[t]] <- minimum.spanning.tree(net[[t]])
  weightmst[[t]]<-max(E(mst[[t]])$weight)
  wei<-unlist(weightmst[[t]])
  deg[[t]]<-degree(mst[[t]])
  centralization[[t]]<-centr_eigen(mst[[t]])$centralization # not know "centr_eigen"
  centr<-as.matrix(unlist(centralization))
  deg_vert[[t]]<- -(deg[[t]])
  root[[t]]<-names(deg[[t]])[deg[[t]]== max(deg[[t]])]
  def_matrix[[t]]<-ciao[[t]]-(as_adjacency_matrix(mst[[t]])*ciao[[t]])
  def_matrix[[t]][def_matrix[[t]] == 0] <- 5 # diagonal and edges in MST are set as 5. but WHY?
  def_m<-as.matrix(unlist(def_matrix[[t]]))
  de<-vec(def_m)
  red[[t]]<-sum(de < wei )/sum(de > wei )    # did NOT use "red"
  a<-subset(de,de<wei)
  b<-subset(de,de>wei)
  res[[t]]<-sum(b^-1)/sum(a^-1)
}

# generate timeline "cri" for later residuality
date<-as.Date.factor(prices$Date, format="%m/%d/%Y")
meanlayer1<-list() # the first day of every week
for(t in 1: 93){
  meanlayer1[[t]]<-date[[t*7]]
}
meanlayer1<-as.Date(unlist(meanlayer1))
psi<-data.frame(meanlayer1)
fixi<-"2017-09-14"
fixi<-data.frame(fixi) # can not combine
psi<-t(psi)            # can not combine
gloxi<-t(data.frame(fixi$fixi,psi))  # can not combine
gloxia<-data.frame(gloxi)            # can not combine
glox<-as.data.frame(gloxia$meanlayer1) # can not combine
glox<-data.frame(glox)                 # can not combine
glox<-as.matrix(glox$gloxia.meanlayer1)
glox<-glox[-94]
cri<-as.Date(as.character(glox)) # from "2017-09-14" to "2019-06-18"
# generate residuality 
weight_mst<-as.matrix(unlist(weightmst))
threshold_mst<-zoo(weight_mst,cri)
residuality<-zoo(as.matrix(unlist(res)),cri)
# plot residuality
png("MSTCC_Residuality.png", width=500, height=400, bg = "transparent")
plot(threshold_mst, main="",ylab = "", type = "l",xlab="",ylim=c(1.04,1.55))
par(new = T)
plot(residuality, main="",ylab = "", type = "l", yaxt="n", xlab = "", col="red")
axis(4,col.axis="red")
dev.off()







