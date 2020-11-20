######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 5
## Dae: 2020-11-16
####################################################

library(igraph)
setwd("~/Projects/upc/upc-miri/csn/homeworks/homework_5")

# Higher is best
calculate_tpr <- function(community, g) {
  # Get the sizes of Each Community that the algorithm is providing without the header of the table
  comm_sizes <- unname(sizes(community))
  # How many communities the algorithm built
  until <- length(community)
  # Original Graph size
  graph_size <- gsize(g)
  # Counter for non empty triangles
  count_triads <- 0
  for(i in 1:until) {
    # induced.subgraph https://igraph.org/r/doc/subgraph.html. Build a new graph with edges and vertices required
    # count_triangles - Provided by igraph
    count_triads = count_triads + sum(count_triangles(induced.subgraph(g, unname(unlist(community[i])))))*comm_sizes[i]
  }
  return(count_triads/graph_size)
}

# Lower the best f_c/n_c
calculate_expansion <- function(f_c, g){
  return(f_c/gsize(g))
}

calculate_modularity = modularity

# f_c is defined as the edges in the frontier C = |(u,v) | u \in C \land v \notin C|
# m_c is defined as the edges within cluster C = |(u,v) | u \land v \in C|
get_f_c_m_c <- function(community, g){
  # Take all members (vertices) of the community that we are calculating
  members <- membership(community)
  # Take all edges of the graph
  edges <- E(g)
  sum_m_c <- 0
  sum_f_c <- 0
  for(edge in edges) {
    e <- ends(g, edge)
    if(members[e[1]] == members[e[2]]){
      sum_m_c <- sum_m_c + 2
    }else{
      sum_f_c <- sum_f_c + 2
    }
  }
  return(c(sum_f_c, sum_m_c))
}

# Lower the best: Fraction of total edge volume that points outside the cluster, \frac{f_c}{2m_c+f_c}
calculate_conductance <- function(f_c, m_c) {
  return(f_c/((2*m_c)+f_c))
}

execute_algorithm <- function(algorithm, graph){
  community <- get(algorithm)(graph)
  tuple_f_c_m_c <- get_f_c_m_c(community, graph)
  f_c <- tuple_f_c_m_c[1]
  m_c <- tuple_f_c_m_c[2]
  tpr <- calculate_tpr(community, graph)
  expansion <- calculate_expansion(f_c, graph)
  conductance <- calculate_conductance(f_c, m_c)
  modularity <- calculate_modularity(community, graph)
  print(paste("Algorithm: ", algorithm, " was calculated"))
  return(c(tpr, expansion, conductance, modularity))
}

# Print extracted data for each algorithm and each metric
print_summary_model_results <- function(data, algos){
  df <- data.frame(data)
  colnames(df) = c("TPR", "Expansion", "Conductance", "Modularity")
  rownames(df) = c(algos)
  print.data.frame(df, right=FALSE)
}

run_graph <- function(name, graph, algos){
  print("")
  print("####################################")
  print(paste("          ", name, " GRAPH         "))
  print("####################################")
  print("")
  
  result <- NULL
  for(algo in algos){
    result <- rbind(result, execute_algorithm(algo, graph))
  }
  print_summary_model_results(result, algos)
}


communities_algo_small <- c( "edge.betweenness.community"
                           , "fastgreedy.community"
                           , "label.propagation.community"
                           , "leading.eigenvector.community"
                           , "multilevel.community"
                           , "optimal.community"
                           , "spinglass.community"
                           , "walktrap.community"
                           , "infomap.community"
                          )

communities_algo_big <- c( "edge.betweenness.community"
                        , "fastgreedy.community"
                        , "label.propagation.community"
                        , "leading.eigenvector.community"
                        , "multilevel.community"
                        , "spinglass.community"
                        , "walktrap.community"
                        , "infomap.community"
                        )

main <- function(){
 
  graph_zachary <- graph.famous("Zachary")
  
  # Starred Repositories in Github https://snap.stanford.edu/data/github_stargazers.html
  graph_git <- graph.data.frame(read.csv('git_target.csv', header=TRUE), directed = FALSE)
  
  # Arxiv GR-QC (General Relativity and Quantum Cosmology)  https://snap.stanford.edu/data/ca-GrQc.html
  graph_gr_qc <- read.graph("ca-GrQc.txt", format = "edgelist", directed = FALSE)
  
  #run_graph("Zachary", graph_zachary, communities_algo_small)
  
  # Comment this if you want to run something quick
  #run_graph("Github Starred Repos", graph_git, communities_algo_big)
  
  # Comment this if you want to run something quick
  run_graph("GR-QC", graph_gr_qc, communities_algo_big)
  
}


main()


