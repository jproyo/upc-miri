######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 5
## Dae: 2020-11-16
####################################################

library(igraph)
setwd("~/Projects/upc/upc-miri/csn/homeworks/homework_5")

computeSummaryTableForWiki <- function(graph) {
  table <- data.table(
    "Graph" = character(),
    "N" = numeric(),
    "E" = numeric(),
    "k" = numeric(),
    "delta" = numeric(),
    stringsAsFactors = FALSE
  )
  
  E = length(E(graph))
  N = length(V(graph))
  k = 2 * E / N
  delta = 2 * E / (N * (N - 1))
  
  table <-
    rbind(table, list("Wikipedia", N, E, round(k, 2), round(delta, 6)))
  
  return(table)
}

computeWalktrapComm <- function(graph) {
  start_time = Sys.time()
  walktrap_comms = walktrap.community(graph)
  delta_time = Sys.time() - start_time
  
  cat("Elapsed time: ", delta_time, "\n")
  return(walktrap_comms)
}


communitiesCount <- function(communities) {
  num_comms = max(communities$membership)
  cat("Total number of communities found: ", num_comms, "\n")
  return(num_comms)
  
}

plotCommunitiesGraph <- function(communities, graph, vecToShow) {
  vertices_membership <- communities$membership
  vertices_pos <- seq(length(communities$membership))
  
  for (sub_graph_index in vecToShow) {
    vertices_sub_comm <-
      vertices_pos[vertices_membership == sub_graph_index]
    # Create subgraph of subcommunity sub_graph_index
    sub_graph = induced_subgraph(graph, vids = vertices_sub_comm)
    plot(
      sub_graph,
      main = paste("Wikipedia - Community N?", sub_graph_index),
      layout = layout.auto,
      vertex.size = 20,
      vertex.label.color = "darkred",
      vertex.shape = "sphere",
      vertex.label.font = 2,
      vertex.color = "green",
      edge.color = "black"
    )
    box(which = "plot")
  }
}

wiki_graph <- read.graph("wikipedia.gml", format = "gml")

wiki_summary_table <- computeSummaryTableForWiki(wiki_graph)
wiki_walktrap_comms <- computeWalktrapComm(wiki_graph)
wiki_num_comms <- communitiesCount(wiki_walktrap_comms)
plotCommunitiesGraph(wiki_walktrap_comms, wiki_graph, sample(1:wiki_num_comms, 1))
