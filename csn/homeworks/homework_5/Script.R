######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 4
## Dae: 2020-11-16
####################################################

library(igraph)

##exploring community structure
karate <- graph.famous("Zachary")
##view friendship relation among members of Karate club
plot(karate)
##Find cluster partition according to Walktrap algorithm:
#distance is based on random walks, and similarity==shortest random walk
wc <- walktrap.community(karate)
modularity(wc)
membership(wc)
plot(wc, karate)

##An alternative way of plotting communities without the shaded regions:
plot(karate, vertex.color=membership(wc))

##to view hierarchical structure (dendogram) given by algorithm that works by hierarchical construction (as fastgreedy)
 karate <- graph.famous("Zachary")
##fastgreedy.community : clustering via greedy optimization of modularity
fc <- fastgreedy.community(karate)
dendPlot(fc)

##compare with Girvan-Newman edge betweeness:
GN<-edge.betweenness.community(karate)
dendPlot(GN)

modularity(GN); modularity(fc)

##HIERARCHICAL CLUST on dissimilarity graph
IBEX<-read.table("ibex_dataset.txt",sep="",header=T)
dd <-as.dist(2*(1-cor(IBEX)))
met="ward.D2" ##  complete,single,average,median,mcquitty
hc <-hclust(dd,method=met)
plot(hc,main=paste(met," method"),axes=TRUE,xlab="",sub="")
#compute the cut at mean level K
l <-length(hc$height);hh <- sort(hc$height);K <- mean(hh[1:l])
abline(h=K,lty=2,lwd=2) ##draw the cut
#branches below K make clusters, above go to singletons
groups <- cutree(hc, h = K)  ##obtain  clusters
numgp <- max(groups) #number of clusters.
#extract the names of each group and convert to list
W <- list(names(groups[groups==1]))
##recursively concatenate lists
for (i in 2:numgp){W <- c(W,list(names(groups[groups==i])))}
W
##Xtras
plot(hc,hang=-1) ##hang=-1 places labels at bottom

##Obtain adjacency matrix from dissimilarity relation (dist)
A<-as.matrix(dd)
##create igraph graph object from adjacency matrix
G <-graph.adjacency(A,mode="undirected",weighted = TRUE)
plot(G)
##to view weights
E(G)$weight
##BE AWARE when applying clustering algorithms from igraph to graph object G
##which are based on maximizing modularity (wrto random graph). Problem: G is complete
##keep in mind is a weighted graph so it must be indicated
fG<-fastgreedy.community(G,weights = E(G)$weight)
dendPlot(fG)
sizes(fG) ##give community sizes (by max modularity, will see is too rough: gives 1 community)
modularity(fG)
##almost 0 :  the problem is the graph is complete so modularity is almost 0 for any partition
##fix: consider Hamiltonian with parameter gamma > 1.


