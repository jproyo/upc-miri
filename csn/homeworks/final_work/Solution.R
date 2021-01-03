##############################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy Campderros
## Title: Final Work
## Dae: 2020-12-21
##############################################################

library(sna)
library(igraph)
library("data.table")
setwd("~/Projects/upc/upc-miri/csn/homeworks/final_work")

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

  selected <- fp_graphs[1]
  selected <- oop_graphs[5] # Another comment
  adj_matrix <- read.dot(selected)
  colnames(adj_matrix) <- c(1:length(adj_matrix[,1]))
  rownames(adj_matrix) <- c(1:length(adj_matrix[1,]))
  graph <- graph_from_adjacency_matrix(adjmatrix = adj_matrix, c("undirected"))

  summary <- dataFromGraph(graph)
  print.data.frame(summary, right=FALSE)

  communities <- walktrap.community(graph)

  count <- max(communities$membership)

  print(paste("Number of Communities: ", count))

  plot(communities, graph, main = "Communities"
       , layout = layout.auto,vertex.color = "blue"
       , edge.color = "black")

}

main()
