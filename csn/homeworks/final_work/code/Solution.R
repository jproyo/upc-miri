##############################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy Campderros
## Title: Final Work
## Dae: 2021-01-22
##############################################################

library(sna)
library(igraph)
library("data.table") 
library("DirectedClustering")

setwd("~/Projects/upc/upc-miri/csn/homeworks/final_work")

get_graph_name <- function(file){
  return(tools::file_path_sans_ext(basename(file)))
}

run_metrics <- function(file){
  graphName <- get_graph_name(file)
  adj_matrix <- read.dot(file)
  colnames(adj_matrix) <- c(1:length(adj_matrix[,1]))
  rownames(adj_matrix) <- c(1:length(adj_matrix[1,]))
  graph <- graph_from_adjacency_matrix(adjmatrix = adj_matrix, c("undirected")) 
  
  table <- data.frame()
  E = length(E(graph))
  N = length(V(graph))
  k = 2 * E / N
  delta = 2 * E / (N * (N - 1))
  MGD = average.path.length(graph)
  DIAM = diameter(graph)
  
  communities <- walktrap.community(graph) 

  modul <- modularity(communities)
  
  count_communities <- max(communities$membership)
  
  mean_cc_coef <- sum(sna::closeness(adj_matrix,gmode="graph",cmode="suminvundir"))/N
  
  mean_lc_coef <- transitivity(graph, type = "average")

  graph_d = degree.distribution(graph)
  plot(graph_d, log = "xy", ylab="Prob.", xlab="Dependency Call", main = paste("Program: ", graphName))
  return(c(N, E, round(k, 2), round(delta, 4),round(MGD, 4),DIAM, round(modul, 4), count_communities, round(mean_cc_coef,4), round(mean_lc_coef,4)))
}

process_graphs <- function(path_graph, label){
  print(paste("Running", label, "......."))
  table <- NULL
  rows <- NULL
  files <- list.files(path = path_graph, pattern = "*.dot", full.names = TRUE)
  for (f in files) {
    table <- rbind(table, run_metrics(f))
  }
  df <- data.frame(table)
  colnames(df) = c("N", "E", "K", "Delta", "MGD", "Diameter", "Modularity", "Communities", "Mean CC Coef", "Mean LC Coef")
  rownames(df) = lapply(files, get_graph_name)
  print.data.frame(df, right=FALSE)
  summary(df)
}

main <- function(){
  #process_graphs("fp_graphs", "FP GRAPHS")
  #process_graphs("oop_graphs", "OOP GRAPHS")
  process_graphs("to_plot", "TP PLOT")
}

main()

