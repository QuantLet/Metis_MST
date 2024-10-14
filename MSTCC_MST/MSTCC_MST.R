# codes originate from Giudici P, et al. (2020). "Network Models to Enhance Automated Cryptocurrency Portfolio Management."

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
library(glasso)# Load Functions and other Files
source('./PackagesNetworkPortfolio.R')

setwd("~/Documents/Code/Metis_Minimum Spanning Tree/MSTCC_MST/update_20241014")
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
weightmst<-list() # maximum weight of MST
net<-list()
mst<-list() # minimum spanning tree
deg<-list() # number of edges coneectinh one point
root<-list()
deg_vert<-list() # minus "deg"
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
# animation of efficient frontier
fig = image_graph(width = 800, height = 640, res = 150, bg = "transparent")
## plot MST of each window
for (t in 1 : length(mst)){
  verticesdegreeall<-degree(mst[[t]])
  node.size<-as.matrix(verticesdegreeall)*4
  
  # Find the vertex with the largest degree
  max_deg_node <- which.max(verticesdegreeall)
  
  # Get the layout using layout_with_fr and set the max degree node at the center
  layout <- layout_components(mst[[t]])
  
  centered_layout <- sweep(layout, 2, layout[max_deg_node, ], FUN = "-")
  
  # Get the current range of x and y coordinates
  x_range <- range(centered_layout[, 1])
  y_range <- range(centered_layout[, 2])
  
  # Define the desired range
  desired_range <- c(-5, 5)
  
  # Scale the coordinates to the desired range
  centered_layout[(centered_layout[, 1])>0, 1] <- centered_layout[(centered_layout[, 1])>0, 1] / x_range[2] * desired_range[2]
  centered_layout[(centered_layout[, 1])<0, 1] <- centered_layout[(centered_layout[, 1])<0, 1] / x_range[1] * desired_range[1]
  centered_layout[(centered_layout[, 2])>0, 2] <- centered_layout[(centered_layout[, 2])>0, 2] / y_range[2] * desired_range[2]
  centered_layout[(centered_layout[, 2])<0, 2] <- centered_layout[(centered_layout[, 2])<0, 2] / y_range[1] * desired_range[1]
  
  print(range(centered_layout[, 1]))
  print(range(centered_layout[, 2]))
  
  
  #png(paste("MSTCC_MST_",t,".png",sep = ""), width=500, height=400, bg = "transparent")
  set.seed(123)
  d1=index(W_in[[t]])[1]
  d2=index(W_in[[t]])[dim(W_in[[t]])[1]]
  plot(mst[[t]], edge.color= "black", vertex.size= node.size, vertex.color="orange",
       vertex.label.dist=1.5,layout=centered_layout)
  # Add title at the bottom using mtext()
  mtext(
    text = paste(d1, "to", d2), 
    side = 1,                # 1 = bottom
    line = 1,                # Distance from the plot
    cex = 1.2,               # Text size
    col = "black",            # Text color
    font = 2                 # Text style (2 = bold)
  )
} 
dev.off()
animation <- image_animate(fig, fps = 5)
image_write(animation, paste0(getwd(), "/MSTCC_MST_movie.gif"))

# # Function to plot MST with the largest node at the center
# plot_mst_center <- function(mst, file_name) {
#   # Calculate the degree of each vertex
#   verticesdegreeall <- degree(mst)
# 
#   # Find the vertex with the largest degree
#   max_deg_node <- which.max(verticesdegreeall)
# 
#   # Get the layout using layout_with_fr and set the max degree node at the center
#   layout <- layout_with_fr(mst)
# 
#   # Adjust the layout to center the max degree node
#   layout <- layout - layout[max_deg_node, ]
# 
#   # Scale the node sizes based on their degree
#   node.size <- verticesdegreeall * 6
# 
#   # Plot and save the MST
#   png(file_name, width = 500, height = 400, bg = "transparent")
#   plot(mst,
#        edge.color = "black",
#        vertex.size = node.size,
#        vertex.color = "orange",
#        layout = layout,
#        main = "MST with Central Node")
#   dev.off()
# }
# 
# for (t in 1:length(mst)) {
#   file_name <- paste("MSTCC_MST_centered_", t, ".png", sep = "")
#   plot_mst_center(mst[[t]], file_name)
# }
# 
# #########
# 
# # Function to generate a custom layout with the central node
# custom_layout <- function(graph) {
#   # Calculate the degree of each vertex
#   degrees <- degree(graph)
# 
#   # Find the vertex with the highest degree
#   max_deg_node <- which.max(degrees)
# 
#   # Generate a basic layout
#   layout <- layout_with_fr(graph)
# 
#   # Center the max degree node
#   layout <- layout - layout[max_deg_node, ]
# 
#   # Optional: further spread out other nodes
#   layout <- layout * 1.5
# 
#   # Ensure the max degree node is at the center
#   layout[max_deg_node, ] <- c(0, 0)
# 
#   return(layout)
# }
# 
# # Function to plot MST with the custom layout
# plot_mst_custom_center <- function(mst, file_name) {
#   # Calculate the degree of each vertex
#   verticesdegreeall <- degree(mst)
# 
#   # Generate the custom layout
#   layout <- custom_layout(mst)
# 
#   # Scale the node sizes based on their degree
#   node.size <- verticesdegreeall * 6
# 
#   # Plot and save the MST
#   png(file_name, width = 500, height = 400, bg = "transparent")
#   plot(mst,
#        edge.color = "black",
#        vertex.size = node.size,
#        vertex.color = "orange",
#        layout = layout,
#        main = "MST with Central Node")
#   dev.off()
# }
# 
# # Plot MST for each window with the custom layout
# for (t in 1:length(mst)) {
#   file_name <- paste("MSTCC_MST_custom_centered_", t, ".png", sep = "")
#   plot_mst_custom_center(mst[[t]], file_name)
# }




## plot MST of the whole sample time from Sep14, 2017 to Oct 17, 2019
network_whole=graph_from_adjacency_matrix(Dist_whole,weighted=T,
                                          mode="undirected", diag=F)  # network of filtered correlation matrix
Edgelist_whole<-get.edgelist(network_whole)                           # edges of network
weight_whole<-E(network_whole)$weight                                 # weight of network
A<-cbind(Edgelist_whole,weight_whole)
A<-as.matrix(A)
links2_whole<-as.data.frame(A)                                # links of network
colnames(links2_whole)<-c("from","to","weight")
net_whole<- graph_from_data_frame(d=links2_whole, directed=F) # net of whole data
mst_whole<- minimum.spanning.tree(net_whole)                  # minimum spanning tree
verticesdegreeall<-degree(mst_whole)
node.size<-as.matrix(verticesdegreeall)*4
png("MSTCC_MST_whole.png", width=500, height=400, bg = "transparent")
set.seed(123)
plot(mst_whole, edge.color= "black", vertex.size= node.size, vertex.color="orange",
     vertex.label.dist=1.5,layout=layout_components)          # layout is very important
#layout=layout_components
#layout=layout_with_kk
#layout=layout_with_lgl
dev.off()
E(mst_whole)$weight
E(mst_whole)
