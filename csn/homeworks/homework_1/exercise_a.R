######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 1
## Exercise a
## Dae: 2020-09-21
####################################################

library('igraph')
library(plyr)

generate_model <- function(p){
  x <- 1:100
  C_sum <- 0
  L_sum <- 0
  for(i in x){
    m_x <- watts.strogatz.game(1, 1000, 4, p)
    C_sum <- C_sum + transitivity(m_x)
    L_sum <- L_sum + average.path.length(m_x)
  }
  return(c(C_sum/100, L_sum/100))
}

calculate_model <- function(p){
  m_0 <- watts.strogatz.game(1, 1000, 4, 0)
  C_0 <- transitivity(m_0)
  L_0 <- average.path.length(m_0)
  model <- generate_model(p)
  return(c(model[1]/C_0, model[2]/L_0))
}

draw_watt_strogatz_model <- function(ps, model){
  labels_x <- c(0.0001, 0.001, 0.01, 0.1, 1)
  plot(ps,model[1,], log='x', xlab="Probability", ylab="L vs C", type="p", xaxt='n')
  axis(1, at = labels_x,labels = sprintf("%.4f", labels_x))
  points(ps,model[2,], pch=18)
  text(x=0.0005,y=0.4,label = "L(p)/L(0)")
  text(x=0.006,y=0.8,label = "C(p)/C(0)")
}

probabilities <- c(0.0001, 0.0002, 0.0005, 0.0007, 0.001, 0.002, 0.005, 0.007, 0.01, 0.02, 0.05, 0.07, 0.1, 0.2, 0.5, 0.7, 1)
## Calculating this model is a little slow. You must wait at least 30 secs.
model <- sapply(probabilities, calculate_model)
draw_watt_strogatz_model(probabilities, model)

  