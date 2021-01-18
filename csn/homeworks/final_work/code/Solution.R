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
  process_graphs("to_test", "FP GRAPHS")
  #process_graphs("fp_graphs", "FP GRAPHS")
  #process_graphs("oop_graphs", "OOP GRAPHS")
}

blox_pot_modularity <- function(){
  data <- c(0.4826, 0.4875, 0.3403, 0.2458, 0.6461, 0.6363, 0.4939, 0.6203, 0.4621, 0.4138, 0.6054, 0.5320, 0.5825, 0.5832, 0.5749, 0.7069, 0.5792, 0.5655, 0.4633, 0.4207, 0.3388, 0.6245, 0.5310, 0.4466, 0.4614, 0.3251, 0.6063, 0.5651, 0.4640, 0.5238, 0.7759, 0.6871, 0.5633, 0.5939, 0.3033, 0.5757, 0.3191, 0.1458, 0.5858,0.8347, 0.6676, 0.6542, 0.7835, 0.8219, 0.7564, 0.6508, 0.6670, 0.6385, 0.7583, 0.9013, 0.6779, 0.7138, 0.8836, 0.7270, 0.8157, 0.7034, 0.6716, 0.7095, 0.7110, 0.6709, 0.7439, 0.7672, 0.7154, 0.7519, 0.6193, 0.7091, 0.6403, 0.6890, 0.7582, 0.7695, 0.6456, 0.7279, 0.8531, 0.6879, 0.6956, 0.6660, 0.7786, 0.6624)
  type <- c(rep("FP",39),rep("OOP", 39))
  df <- data.frame(data, type)
  boxplot(df$data ~ df$type, data = df, xlab="Paradigm", ylab="Modularity", main="Modularity by Paradigm")
}

main()

#blox_pot_modularity()
