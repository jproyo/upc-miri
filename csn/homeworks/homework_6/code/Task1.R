######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 6
## Dae: 2020-12-13
####################################################

require("stats4") # for MLE
require("VGAM") # for the Riemann-zeta function

library(ggplot2)

## TAREA 1 

table = read.table("data/degree_distribution_no_pref_att", header = FALSE, sep = " ", dec = ".")
degree_sequence = read.table("data/degree_of_each_node_no_pref_att",header = FALSE)

M_0=1
N_0=2
 
table$V1[15]
table$V2[15]

degree_spectrum = table(degree_sequence)
degree_spectrum[15]


x <- degree_sequence$V1

MAX = max(x)
N = length(x)
M = sum(x)
M_P = sum(log(x))
MEAN = M/N

table$V2 = table$V2/N

sum(table$V2)


## MODEL 1##
x <- seq(0,MAX,1)
y <- (x^-2.295044)/gamma(2.295044)

harmonic=0
for (i in 1:MAX){
  harmonic=harmonic+ 1/(i^(3))
}
y_1 <- (x^-3)/harmonic

plot(log(table$V1),log(table$V2), col = "blue", pch = 20,main = "Growth + Pref.Attachment: Degree distribution at time tmax",
     xlab = "log degree",
     ylab = "log probability")
points(log(x), log(y), col = "red", pch = 20)
points(log(x), log(y_1), col = "green", pch = 20)


## MODEL 2##
q= 0.500005

x <- seq(0,MAX,1)
y <- q*(1-q)^(x-1)

plot(log(table$V1),log(table$V2), col = "blue", pch = 20,main = "Growth + Rand Att: Degree distribution at t_max", 
     xlab = "log degree",
     ylab = "log probability")
points(log(x), log(y), col = "red", pch = 20)


## MODEL 3 ##
lambda = 21.9726

x <- seq(0,MAX,1)
y <- (lambda^x * exp(-lambda))/(factorial(x)*(1-exp(-lambda)))

plot(log(table$V1),log(table$V2), col = "blue", pch = 20,main = "No Growth + Pref Att: Degree distribution at t_max", 
     xlab = "log degree",
     ylab = "log probability")
points(log(x), log(y), col = "red", pch = 20)


