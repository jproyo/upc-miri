######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 1
## Exercise b
## Dae: 2020-09-21
####################################################

library('igraph')
library(plyr)


calculate_probability <- function(n){
  return(((1+0.1)*log(n))/n)
}

generate_model <- function(n){
  return(average.path.length(erdos.renyi.game(n, calculate_probability(n))))
}

draw_erdos_rengi_model <- function(nodes, model){
  plot(nodes,model, xlab="Num of Nodes", ylab="average shortest path", type="b", ylim=c(min(model), max(model)))
}

## Calculating this model is a little slow. You must wait at least 30 secs.
nodes <- c(10,20,50,70,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,1000,1500,2000,2500,3000,3500,4000,4500,5000,10000,20000)
model <- sapply(nodes, generate_model)
draw_erdos_rengi_model(nodes, model)

  