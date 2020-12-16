######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 7
## Dae: 2020-12-28
####################################################

require("igraph")
require("Matrix")

setwd("~/Projects/upc/upc-miri/csn/homeworks/homework_7/code")

# Infection Function ------------------------------------------------------

#M is the adjacency matrix
#v is the vector of infected nodes (1->infected, 0->not infected)
#beta is the probability of infection, and gamma the probability of recovery

spread <- function(adj_matrix_graph,initial_infected,tmax,beta,gamma){
  n <- length(initial_infected)
  A <- matrix(nrow=tmax,ncol=n)
  A[1,] <- initial_infected
  for(i in 2:tmax){
    n_inf <- adj_matrix_graph%*%as.numeric(A[i-1,])  #a vector containing the number of infected neighbours of each node
    inf <- n_inf > rgeom(n,beta)
    cured <- as.logical(rbinom(n=n,size=1,prob=gamma)*A[i-1,]) #vector infected of nodes which are cured (with probability gamma)
    A[i,] <- as.logical((A[i-1,] | inf) & !cured) #an element is infected if it already was or gets infected this time step, and it hasn't been cured
  }
  return (A)
}



# Graphs ------------------------------------------------------------------
set.seed(1)
n <- 2000
tree <- make_tree(n,children=2,mode = c("undirected"))

scaleF <- barabasi.game(n,.05,directed=F)

erdosRenyi <- erdos.renyi.game(n,p=0.05,directed = F)

knMatrix <- matrix(data=1,n,n)
for(i in 1:n) knMatrix[i,i] <- 0

star <- make_star(n,mode=c("undirected"))


kn <- graph_from_adjacency_matrix(knMatrix,mode=c("undirected"))

graphs <- list(tree,scaleF,erdosRenyi,kn,star)
graphNames <- c("tree","scaleFree","Erdos-Renyi","complete","star")
# Generate eigenvalues -----------------------------------------------------


#Uses the arpack library built in in igraph to quickly compute the largest eigenvalue.
eigen_value_mnax <- function(graph){
  max(abs(eigen(as_adj(graph))$values))
}


# Simulate -------------------------------------------------------------------

# Given a matrix with all infection states, count the number of infected persons in every state. return this as a vector.
infectedEvolution <- function(spread){
  infected <- c()
  for(i in 1:length(spread[,1])){
    infected[i] <- length(which(spread[i,]==T))
  }
  infected
}

#Run a simple simulation. Returns a vector with the number of infected persons in every simulation step.
simulate <- function(graph,beta=.4,gamma=.8,p0=.05,tmax=30){
  #initialise an infection of 5%
  infect_init <- sample(c(1,0),length(V(graph)),replace=T,prob=c(p0,1-p0))
  
  treeSpread <- spread(as_adj(graph),initial_infected=infect_init,tmax=tmax,beta=beta,gamma=gamma)
  simulation <- infectedEvolution(treeSpread)
  #scale
  simulation/length(V(graph))
}


# beta,gamma close to threshold -------------------------------------------

#Run a double simulation with a beta-gamma value epsilon percentage above and below the threshold

fullSimulationPlot <- function(graph,graphName,gamma,epsilon,threshold){
  betaThreshold <- gamma*threshold
  
  #above threshold
  highBeta <- min(1,betaThreshold*(1+epsilon))
  yvalues <- simulate(graph,beta=highBeta,gamma=gamma,tmax=250)
  plot(yvalues,ylim = c(0,max(yvalues)),
       main=graphName,
       xlab="timestep",
       ylab="fraction infected persons",type="l")
  
  #below threshold
  lowBeta <- max(0,betaThreshold*(1-epsilon))
  lines(simulate(graph,beta=lowBeta,gamma=gamma,tmax=250),col="red")
  
  print(c(lowBeta,highBeta,gamma))
}

thresholds = 1/sapply(graphs,largestEigenvalue)

set.seed(6)
plotGraphs <- function(epsilon,save=F){
  for(i in 1:5){
    if(save) pdf(file=paste("images//thresholdSimulation_",graphNames[[i]],".png", sep=""))
    fullSimulationPlot(graphs[[i]],graphNames[i],gamma=.1,epsilon=epsilon,thresholds[[i]])
    if(save) dev.off()
  }
}
plotGraphs(epsilon=.1,save=F)



#task1 (fixed parameters, no threshold)
fixedbeta <- 0.005
fixedgamma <- 0.05
boolSave <- F
for(i in 1:5){
  if(boolSave) pdf(file=paste("images//task1_",graphNames[[i]],".png", sep=""))
  yvalues <- simulate(graphs[[i]],beta=fixedbeta,gamma=fixedgamma,tmax=500)
  plot(yvalues,ylim = c(0,max(yvalues)),
       main=graphNames[i],
       xlab="time",
       ylab="fraction infected people",type="l")
  if(boolSave) dev.off()
}