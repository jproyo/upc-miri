##############################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy Campderros
## Title: Final Work
## Dae: 2020-12-21
##############################################################

library(sna)
library(igraph)
library("data.table") 
library("DirectedClustering")


 

dataFromGraph <- function(graph) {
  table <- data.frame()
  E = length(E(graph))
  N = length(V(graph))
  k = 2 * E / N
  delta = 2 * E / (N * (N - 1))
  MGD = average.path.length(graph)
  DIAM = diameter(graph)
  
  
  
  table <- rbind(table, list(N, E, round(k, 2), round(delta, 6),MGD,DIAM))
  colnames(table) = c("N", "E", "k", "delta","avarage path distance","diameter")
  return(table)
}


plotSome <- function(communities, graph, vecToShow) {
  vertices_membership <- communities$membership
  vertices_pos <- seq(length(communities$membership))
  
  for (sub_graph_index in vecToShow) {
    vertices_sub_comm <- vertices_pos[vertices_membership == sub_graph_index]
    sub_graph = induced_subgraph(graph, vids = vertices_sub_comm)
    plot(sub_graph, main = paste("Community Number: ", sub_graph_index)
         , layout = layout.auto,vertex.color = "blue"
         , edge.color = "black")
  }
}

main <- function(){
  fp_graphs <- c( "fp_graphs/small.dot", 
                  "fp_graphs/cryptol.dot", 
                  "fp_graphs/bad_design.dot", 
                  "fp_graphs/pandoc.dot", 
                  "fp_graphs/cabal.dot")
  oop_graphs <- c( "oop_graphs/bad_design.dot", 
                   "oop_graphs/spring.dot",
                   "oop_graphs/joda_time.dot",
                   "oop_graphs/guava.dot", 
                   "oop_graphs/akka.dot",
                   "oop_graphs/jetty.dot")
  
  
  selected <- oop_graphs[1]
  adj_matrix <- read.dot(selected)
  colnames(adj_matrix) <- c(1:length(adj_matrix[,1]))
  rownames(adj_matrix) <- c(1:length(adj_matrix[1,]))
  graph <- graph_from_adjacency_matrix(adjmatrix = adj_matrix, c("undirected")) 
  
  #Isolated = which(degree(graph)==0)
  #graph = delete.vertices(graph, Isolated)
  
  
  print(dim(adj_matrix))
  
  summary <- dataFromGraph(graph)
  
  print.data.frame(summary, right=FALSE)
  
  communities <- walktrap.community(graph) 
  
  print(paste("Modularity: ",modularity(communities)))
  
  count <- max(communities$membership)
  
  print(paste("Number of Communities: ", count))
  
  #plot(communities, graph, main = "Communities", layout = layout.auto,vertex.color = "blue", edge.color = "black")
  
  print(paste("mean closesness centrality coef: ", sum(sna::closeness(adj_matrix,gmode="graph",cmode="suminvundir"))/summary$N)) ## uoooh treure disconnected vertices! i fer mitja  
  
  TRANS = transitivity(graph, type = "average")
  #A<-get.adjacency(graph, sparse=FALSE)
  #TRANS = ClustF(A, type = "undirected", isolates = "zero", norm=1)
  print(paste("mean local clustering coef:", TRANS))
  
  
  
  
  
  graph_d = degree.distribution(graph)
  print(graph_d[0:5])
  plot(graph_d)
}


## d_ij = shortest distance between i and j = geodesic distance

## Interesantes a calcular: 
                            #diameter (longest d_ij)
                            #average shortest-path length = avarage d_ij = mean geodesic distance
                            
                            #sum(degree_sequence)   (but difficult because...)
                            #mean degree = sum(degree_sequence)/dim(degree_sequence)[1]
                            #max degree = max(degree_sequence$V1) 
                            
                            ## hereeeeeee!!!!!!

                            #mean closesness centrality coef. (lab3) - lo centrals que son els nodes.
                            #mean local clustering coef. (lab3) - lo clusteritzat que està el graf.
                            #que no tenen res a veure amb modularity/asdf/asdf/asdf (que s'usen un cop aplicat un algorisme)

                            #degree distribution, n(k)/N, number of vertices of degree k --> MODEL SELECTION

                            



main()

