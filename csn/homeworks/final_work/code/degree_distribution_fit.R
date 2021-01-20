##############################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy Campderros
## Title: Final Work
## Dae: 2021-01-22
##############################################################

require("stats4") # for MLE
require("VGAM") # for the Riemann-zeta function
library(igraph)
library("data.table") 
library("DirectedClustering")

setwd("~/Projects/upc/upc-miri/csn/homeworks/final_work")


get_graph_name <- function(file){
  return(tools::file_path_sans_ext(basename(file)))
}

#Calculate C 
calculate_c <- function(x,n){
  C= 0
  for (i in 1:n){
    for (j in 2:x[i]){
      C= C + log(j)
    }
  } 
  C
}

# Extra data info of each language
extract_info <- function(language, degree_sequence) {
  degree_sequence <- as.integer(degree_sequence)
  m <- sum(degree_sequence)
  n <- length(degree_sequence)
  max <- max(degree_sequence)
  mn <- m/n
  nm <- n/m
  mp <- sum(log(degree_sequence))
  c  <- calculate_c(degree_sequence,n)
  c(language,m,n,max,mn,nm,mp,c)
}

# Plot data for each language 
plot_language <- function(language, file){
  degree_sequence = read.table(file, header = FALSE)
  degree_spectrum = table(degree_sequence)
  barplot(degree_spectrum, main = language,xlab = "degree", ylab = "number of vertices",log = "xy")
}

# Print extracted data for each language in a table
print_summary_table <- function(data){
  df <- data.frame(data)
  colnames(df) <- c("Language","M", "N", "MAX", "M/N","N/M", "MP", "C")
  print.data.frame(df, right=FALSE, row.names=FALSE)
}

# Print extracted data for each language in a table
print_summary_model_results <- function(data, languages){
  df <- data.frame(data)
  colnames(df) = c("displaced_poisson_lambda", "displaced_geometric_q", "zeta_gamma", "zeta_truncated_kmax", "altmann_gamma", "altmann_delta", "aic_displaced_poison", "aic_displaced_geometric", "aic_zeta_2", "aic_zeta", "aic_zeta_truncated")
  rownames(df) = c(languages)
  print.data.frame(df, right=FALSE)
}



# Fit model with ZETA distribution
fit_with_zeta <- function(N, M_P){
  minus_log_likelihood_zeta <- function(gamma) {
    N * log(zeta(gamma)) + gamma * M_P
  }
  mle( minus_log_likelihood_zeta
       , start = list(gamma = 2)
       , method = "L-BFGS-B"
       , lower = c(1.0000001))
}

# Fit model with ZETA TRUNCATED distribution
fit_with_zeta_truncated <- function(N, M_P, MAX){
  minus_log_likelihood_right_truncated_zeta <- function(gamma,k_max) {
    harmonic=0
    for (i in 1:k_max){
      harmonic=harmonic+ 1/(i^(gamma))
    }
    N * log(harmonic) + gamma * M_P
  }
  mle( minus_log_likelihood_right_truncated_zeta
       , start = list(gamma = 2, k_max= N + 50000)
       , method = "L-BFGS-B"
       , lower = c(1.0000001,MAX)
  )
}

# Fit model with DISPLACED POISSON distribution
fit_with_displaced_poisson <- function(N, M, C){
  minus_log_likelihood_displaced_poisson <- function(lamda) {
    -(M*log(lamda)) + N*(lamda + log(1-exp(-lamda))) + C
  }
  mle( minus_log_likelihood_displaced_poisson
       , start = list(lamda = M/N)
       , method = "L-BFGS-B"
       , lower = c(0.0000001)
  )
}

# Fit model with DISPLACED GEOMETRIC distribution
fit_with_displaced_geometric <- function(N, M){
  minus_log_likelihood_displaced_geometric <- function(q) {
    -(M-N)*log(1-q) - N*log(q)
  }
  mle( minus_log_likelihood_displaced_geometric
       , start = list(q = N/M)
       , method = "L-BFGS-B"
       , lower = c(0.0000001)
       , upper = c(0.9999999)
  )
}

# SIMULATE Fitting model with ALTMANN  distribution
fit_with_altmann_distribution <- function(N, M_P, M){
  minus_log_likelihood_altmann<- function(gamma, delta) {
    temp=0
    for (k in 1:N){
      temp=temp+ (k^(-gamma))*(exp(-delta*k))
    }
    c= 1/temp
    gamma*M_P + delta*M - N*log(c)
  }
  
  mle( minus_log_likelihood_altmann
       , start = list(gamma = 2,delta=3)
       , method = "L-BFGS-B"
       , lower = c(0.000001,0.000001)
       
  )
}

summary_all_mle <- function(row) {
  lang <- row[1]
  m <- as.numeric(as.character(row[2]))
  n <- as.numeric(as.character(row[3]))
  max <- as.numeric(as.character(row[4]))
  mn <- as.numeric(as.character(row[5]))
  nm <- as.numeric(as.character(row[6]))
  mp <- as.numeric(as.character(row[7]))
  c <- as.numeric(as.character(row[8]))
  zeta <- fit_with_zeta(n,mp)
  zeta_gamma <- coef(summary(zeta))[1]
  zeta_truncated <- fit_with_zeta_truncated(n,mp, max)
  zeta_truncated_kmax <- coef(summary(zeta_truncated))[2,1]
  displaced_poison <- fit_with_displaced_poisson(n,m, c)
  displaced_poisson_lambda <- coef(summary(displaced_poison))[1]
  displaced_geometric <- fit_with_displaced_geometric(n,m)
  displaced_geometric_q <- coef(summary(displaced_geometric))[1]
  altmann <- fit_with_altmann_distribution(n, mp, m)
  altmann_gamma <- coef(summary(altmann))[1]
  altmann_delta <- coef(summary(altmann))[2]
  models <- c(displaced_poisson_lambda, displaced_geometric_q, zeta_gamma, zeta_truncated_kmax, altmann_gamma, altmann_delta)
  aic <- aic_comparisson(n, mp, zeta, zeta_truncated, displaced_poison, displaced_geometric)
  c(models, aic)
}

get_AIC <- function(m2logL,K,N) {
  m2logL + 2*K*N/(N-K-1) # AIC with a correction for sample size
}

# Compare fitting with AIC
aic_comparisson <- function(N, M_P, mle_zeta, mle_righ_truncated_zeta, mle_displaced_poisson, mle_displaced_geometric){
  aic_zeta <- get_AIC(attributes(summary(mle_zeta))$m2logL, 1, N) # AIC for zeta distribution
  aic_zeta_truncated <- get_AIC(attributes(summary(mle_righ_truncated_zeta))$m2logL, 2, N) # AIC for r.truncated.zeta distribution
  aic_zeta_2 <- get_AIC(-2 *(-2*M_P-N*log(zeta(2))),0,N) # AIC for zeta distribution with gamma equal 2 
  aic_displaced_poison <- get_AIC(attributes(summary(mle_displaced_poisson))$m2logL, 1, N) # AIC for displaced poisson distribution
  aic_displaced_geometric <- get_AIC(attributes(summary(mle_displaced_geometric))$m2logL, 1, N) # AIC for displaced geometric distribution
  aic_best <- min(aic_zeta, aic_zeta_truncated, aic_zeta_2, aic_displaced_geometric, aic_displaced_poison)
  c(aic_displaced_poison-aic_best, aic_displaced_geometric-aic_best, aic_zeta_2-aic_best, aic_zeta-aic_best, aic_zeta_truncated-aic_best)
}

process_graphs <- function(path_graph, label){
  print(paste("Running", label, "......."))
  files <- list.files(path = path_graph, pattern = "*.dot", full.names = TRUE)
  result <- NULL
  for (f in files) {
    graphName <- get_graph_name(f)
    adj_matrix <- read.dot(f)
    colnames(adj_matrix) <- c(1:length(adj_matrix[,1]))
    rownames(adj_matrix) <- c(1:length(adj_matrix[1,]))
    graph <- graph_from_adjacency_matrix(adjmatrix = adj_matrix, c("undirected")) 
    degree_seq <- degree(graph)
    
    result <- rbind(result, extract_info(graphName, degree_seq))
  }
  
  print_summary_table(result)
  
  models <- NULL
  for (x in 1:nrow(result)) {
    models <- rbind(models, summary_all_mle(result[x,]))
  }
  print_summary_model_results(models, result[,1])
  
  
}


main <- function(){
  process_graphs("fp_graphs", "FP GRAPHS")
  #process_graphs("oop_graphs", "OOP GRAPHS")
}

## RUN PROGRAM
main()


