######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 4
## Dae: 2020-11-16
####################################################

library(igraph)


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

fold_edge <- function(members, g, edge, acc, inside = FALSE){
  e <- ends(g, edge)
  if(inside && members[e[1]] == members[e[2]]){
    acc <- acc + 2
  }
  if(!inside && members[e[1]] != members[e[2]]){
    acc <- acc + 2
  }
  return(acc)
}

# f_c is defined as the edges in the frontier C = |(u,v) | u \in C \land v \notin C|
get_f_c <- function(community, g) {
  # Take all members (vertices) of the community that we are calculating
  members <- membership(community)
  # Take all edges of the graph
  edges <- E(g)
  sum_f_c <- 0
  for(i in 1:length(edges)) {
    sum_f_c <- fold_edge(members, g, edges[i], sum_f_c) 
  }
  return(sum_f_c)
}

# Lower the best f_c/n_c
calculate_expansion <- function(community, g){
  return(get_f_c(community, g)/gsize(g))
}

calculate_modularity = modularity

# m_c is defined as the edges within cluster C = |(u,v) | u \land v \in C|
get_m_c <- function(community, g){
  # Take all members (vertices) of the community that we are calculating
  members <- membership(community)
  # Take all edges of the graph
  edges <- E(g)
  sum_m_c <- 0
  for(i in 1:length(edges)) {
    sum_m_c <- fold_edge(members, g, edges[i], sum_m_c, TRUE) 
  }
  return(sum_m_c)
}

# Lower the best: Fraction of total edge volume that points outside the cluster, \frac{f_c}{2m_c+f_c}
calculate_conductance <- function(community, g) {
  f_c <- get_f_c(community, g)
  m_c <- get_m_c(community, g)
  return(f_c/((2*m_c)+f_c))
}

execute_algorithm <- function(algorithm, graph){
  community <- get(algorithm)(graph)
  tpr <- calculate_tpr(community, graph)
  expansion <- calculate_expansion(community, graph)
  conductance <- calculate_conductance(community, graph)
  modularity <- calculate_modularity(community, graph)
  return(c(tpr, expansion, conductance, modularity))
}

# Print extracted data for each algorithm and each metric
print_summary_model_results <- function(data, algos){
  df <- data.frame(data)
  colnames(df) = c("TPR", "Expansion", "Conductance", "Modularity")
  rownames(df) = c(algos)
  print.data.frame(df, right=FALSE)
}


main <- function(){
  communities_algo <- c( "edge.betweenness.community"
                       , "fastgreedy.community"
                       , "label.propagation.community"
                       , "leading.eigenvector.community"
                       , "multilevel.community"
                       , "optimal.community"
                       , "spinglass.community"
                       , "walktrap.community"
                       , "infomap.community"
                       )
  
  #graph <- graph.famous("Zachary")
  graph <- read.graph("wikipedia.gml", format="gml")
  #graph <- read_graph("email-Eu-core.txt")
  result <- NULL
  for(algo in communities_algo){
    result <- rbind(result, execute_algorithm(algo, graph))
  }
  print_summary_model_results(result, communities_algo)
}

main()


