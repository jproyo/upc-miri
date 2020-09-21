######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 1
## Dae: 2020-09-21
####################################################

library('igraph')
library(plyr)

generate_model <- function(p){
  x <- 1:100
  rep(watts.strogatz.game(1, 1000, 4, p), 3)
}

a <- generate_model(0.0001)
transitivity(a[1])
transitivity(a[2])
transitivity(a[3])

  