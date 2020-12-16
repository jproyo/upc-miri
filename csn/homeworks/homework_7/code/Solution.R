######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 7
## Dae: 2020-12-28
####################################################

require("igraph")
require("Matrix")

setwd("~/Projects/upc/upc-miri/csn/homeworks/homework_7/code")

eigen_value_mnax <- function(graph){
  max(abs(eigen(as_adj(graph))$values))
}

run_virus_simulation <- function(adj_matrix_graph,initial_infected,tmax,beta,gamma){
  n <- length(initial_infected)
  adj_matrix_in_time <- matrix(nrow=tmax,ncol=n)
  adj_matrix_in_time[1,] <- initial_infected
  for(i in 2:tmax){
    previous_state <- adj_matrix_in_time[i-1,]
    infected_vector <- adj_matrix_graph%*%previous_state
    infected <- infected_vector > rgeom(n,beta)
    recovered <- as.logical(rbinom(n=n,size=1,prob=gamma)*previous_state) 
    adj_matrix_in_time[i,] <- as.logical((previous_state | infected) & !recovered) 
  }
  return (adj_matrix_in_time)
}

fraction_infected_people <- function(sim){
  infected <- c()
  for(i in 1:length(sim[,1])){
    infected[i] <- length(which(sim[i,]==T))
  }
  return (infected)
}

simulate <- function(graph,beta=.4,gamma=.8,p0=.05,tmax=150){
  infect_init <- sample(c(1,0),length(V(graph)),replace=T,prob=c(p0,1-p0))
  virus_simulation <- run_virus_simulation(as_adj(graph),initial_infected=infect_init,tmax=tmax,beta=beta,gamma=gamma)
  return (fraction_infected_people(virus_simulation)/length(V(graph)))
}

plot_infected <- function(y_vals, graph_name){
  plot(y_vals,ylim = c(0,max(y_vals)),
       main=graph_name,
       xlab="Time",
       ylab="Fraction Infected People",type="l")
}

run_simulation <- function(graph,name,gamma,epsilon,threshold){
  beta_threshold <- gamma*threshold
  
  beta_high <- min(1,beta_threshold*(1+epsilon))
  y_vals <- simulate(graph,beta=beta_high,gamma=gamma,tmax=250)

  plot_infected(y_vals, name)

  beta_low <- max(0,beta_threshold*(1-epsilon))
  lines(simulate(graph,beta=beta_low,gamma=gamma,tmax=250), col="red")
  
  return (c(beta_low,beta_high,gamma))
}

task_1 <- function(graphs, names, beta=0.005, gamma=0.05, t_max=300){
  for(i in 1:length(names)){
    y_vals <- simulate(graphs[[i]],beta=beta,gamma=gamma,tmax=t_max)
    plot_infected(y_vals, names[i])
  }
}

task_2 <- function(graphs, names){
  set.seed(15)
  thresholds = 1/sapply(graphs,eigen_value_mnax)
  df<-data.frame("Low Beta" = double(), "High Beta" = double(), "Gamma" = double())
  for(i in 1:length(names)){
    df[i,] <- run_simulation(graphs[[i]],names[i],gamma=.1,epsilon=0.01,thresholds[[i]])
  }
  rownames(df) <- names
  print.data.frame(df)
}

main <- function(){
  
  set.seed(3)
  n <- 2000
  tree <- make_tree(n,children=2,mode = c("undirected"))
  star <- make_star(n,mode=c("undirected"))
  scale_free_barabasi <- barabasi.game(n,.05,directed=F)
  erdos_renyi <- erdos.renyi.game(n,p=0.05,directed = F)
  watts_strogatz <- sample_smallworld(1, n, 4, 0.05)
  
  graphs <- list(star, tree, scale_free_barabasi, erdos_renyi, watts_strogatz)
  names <- c("Star", "Tree","Scale Free Barabasi-Albert","Erdos-Renyi","Small World - Watts Strogatz")
  
  beta <- 0.005
  gamma <- 0.05
  t_max <- 300
  print(paste("Running Task 1: Fixed Beta and Gamma parameters. Beta:", beta, " - Gamma:", gamma, " - Tmax:", t_max))
  task_1(graphs, names)
  
  print(paste("Running Task 2: Try to find threshold"))
  task_2(graphs, names)

}

main()


