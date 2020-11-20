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
calculate_expansion <- function(f_c, g_size){
  return(sum(f_c)/g_size)
}

calculate_modularity = modularity

# f_c is defined as the edges in the frontier C = |(u,v) | u \in C \land v \notin C|
# m_c is defined as the edges within cluster C = |(u,v) | u \land v \in C|
get_f_c_m_c <- function(community, g){
  # Take all members (vertices) of the community that we are calculating
  members <- membership(community)
  n <- length(community)
  # Take all edges of the graph
  edges <- E(g)
  sum_m_c <- 0
  sum_f_c <- 0
  out_degree <- rep(0, n)
  in_degree <- rep(0, n)
  
  for(edge in edges) {
    e <- ends(g, edge)
    x <- e[1]
    y <- e[2]
    if(members[x] == members[y]){
      in_degree[members[x]] = in_degree[members[x]] + 1
      in_degree[members[y]] = in_degree[members[y]] + 1
    }else{
      out_degree[members[x]] = out_degree[members[x]] + 1
      out_degree[members[y]] = out_degree[members[y]] + 1
    }
  }
  return(list(in_degree, out_degree))
}

# Lower the best: Fraction of total edge volume that points outside the cluster, \frac{f_c}{2m_c+f_c}
calculate_conductance <- function(f_c, m_c, comm_sizes, g_size) {
  n <- length(comm_sizes)
  s <- 0
  for(i in 1:n) {
    s = s + f_c[i]*comm_sizes[i]/(m_c[i] + f_c[i])
  }
  return(s/g_size)
}

execute_algorithm <- function(algorithm, graph){
  community <- get(algorithm)(graph)
  tuple_f_c_m_c <- get_f_c_m_c(community, graph)
  m_c <- tuple_f_c_m_c[[1]]
  f_c <- tuple_f_c_m_c[[2]]
  comm_sizes <- unname(sizes(community))
  g_size <- gsize(graph)
  tpr <- calculate_tpr(community, graph)
  expansion <- calculate_expansion(f_c, g_size)
  conductance <- calculate_conductance(f_c, m_c, comm_sizes, g_size)
  modularity <- calculate_modularity(community, graph)
  print(paste("Algorithm: ", algorithm, " was calculated"))
  return(c(round(tpr, 4), round(expansion, 4), round(conductance, 4), round(modularity,4)))
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
  
  # Facebook combined https://snap.stanford.edu/data/ego-Facebook.html
  graph_facebook <- read.graph("facebook_combined.txt", format="edgelist", directed = FALSE)
  
  run_graph("Zachary", graph_zachary, communities_algo_small)
  
  # Comment this if you want to run something quick
  run_graph("Github Starred Repos", graph_git, communities_algo_big)

  # Comment this if you want to run something quick
  run_graph("Facebook", graph_facebook, communities_algo_big)

}


main()


