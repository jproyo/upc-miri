######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 5
## Dae: 2020-11-16
####################################################

library(igraph)
library("data.table") 
setwd("~/Projects/upc/upc-miri/csn/homeworks/homework_5")

dataFromGraph <- function(graph) {
  table <- data.frame()
  E = length(E(graph))
  N = length(V(graph))
  k = 2 * E / N
  delta = 2 * E / (N * (N - 1))
  table <- rbind(table, list(N, E, round(k, 2), round(delta, 6)))
  colnames(table) = c("N", "E", "k", "delta")
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
  graph <- read.graph("wikipedia.gml", format = "gml")

  summary <- dataFromGraph(graph)
  print.data.frame(summary, right=FALSE)
  
  communities <- walktrap.community(graph)
  
  count <- max(communities$membership)
  print(paste("Number of Communities: ", count))
  
  plotSome(communities, graph, sample(1:count, 50))

}

main()
