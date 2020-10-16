

require("stats4") # for MLE
require("VGAM") # for the Riemann-zeta function

#Basicamente es hacer esto para cada uno de los idiomas dentro de la carpeta data_in
degree_sequence = read.table("./data_in/Catalan_in-degree_sequence.txt",header = FALSE)
#Y luego para la carpeta de las distribuciones ideales

# To do:

# Table 1 Yes
# Table 2 No
# Table 3 Yes
# Table 4 Yes


x <- degree_sequence$V1

x[54] # degree of node 1 (example)

N = dim(degree_sequence)[1] # number_of_nodes 
N
M = sum(degree_sequence)
M
MAX = max(degree_sequence$V1)
MAX
MEAN = sum(degree_sequence$V1)/length(degree_sequence$V1)
MEAN
INV_MEAN = length(degree_sequence$V1)/sum(degree_sequence$V1)
INV_MEAN






M_P = sum(log(x))
C= 0
for (i in 1:N){
  for (j in 2:x[i]){
    C= C + log(j)
  }
} 


degree_spectrum = table(degree_sequence)
barplot(degree_spectrum, main = "Catalan",xlab = "degree", ylab = "number of vertices")






# vale vamos a tener que elegir entre unos cuantos modelos cual nos parece mejor...
# vamos a estimar los parámetros para todos los modelos

#############################################################################
#############################################################################

## Estimating parameters...

                              ## ZETA ##

minus_log_likelihood_zeta <- function(gamma) {
  N * log(zeta(gamma)) + gamma * M_P
}


mle_zeta <- mle(minus_log_likelihood_zeta,
                start = list(gamma = 2),
                method = "L-BFGS-B",
                lower = c(1.0000001))


summary(mle_zeta) # nos da el valor estimado de gamma y también la L

#############################################################################
#############################################################################

                          ## ZETA TRUNCATED ##



minus_log_likelihood_right_truncated_zeta <- function(gamma,k_max) {
  
  harmonic=0
  
  for (i in 1:k_max){
    harmonic=harmonic+ 1/(i^(gamma))
  }
  
  N * log(harmonic) + gamma * M_P
}


mle_righ_truncated_zeta <- mle(minus_log_likelihood_right_truncated_zeta,
                           start = list(gamma = 2, k_max= N ),
                           method = "L-BFGS-B",
                           lower = c(1.0000001,MAX)
                           )

summary(mle_righ_truncated_zeta)





#############################################################################
#############################################################################

                                  ## DISPLACED POISSON ##

minus_log_likelihood_displaced_poisson <- function(lamda) {
  
  -(M*log(lamda)) + N*(lamda + log(1-exp(-lamda))) + C
}


mle_displaced_poisson <- mle(minus_log_likelihood_displaced_poisson,
                start = list(lamda = M/N),
                method = "L-BFGS-B"
                ,lower = c(0.0000001)
                )


summary(mle_displaced_poisson)




#############################################################################
#############################################################################

                              ## DISPLACED GEOMETRIC ##

minus_log_likelihood_displaced_geometric <- function(q) {
  
  -(M-N)*log(1-q) - N*log(q)
}

mle_displaced_geometric <- mle(minus_log_likelihood_displaced_geometric,
                             start = list(q = N/M),
                             method = "L-BFGS-B"
                             ,lower = c(0.0000001)
                             ,upper = c(0.9999999)
                             )


summary(mle_displaced_geometric)




#############################################################################
#############################################################################

## Comparing models...

get_AIC <- function(m2logL,K,N) {
  m2logL + 2*K*N/(N-K-1) # AIC with a correction for sample size
}

get_AIC(attributes(summary(mle_zeta))$m2logL, 1, N) # AIC for zeta distribution

get_AIC(attributes(summary(mle_righ_truncated_zeta))$m2logL, 2, N) # AIC for r.truncated.zeta distribution

get_AIC(-2 *(-2*M_P-N*log(zeta(2))),0,N) # AIC for zeta distribution with gamma equal 2 

get_AIC(attributes(summary(mle_displaced_poisson))$m2logL, 1, N) # AIC for displaced poisson distribution

get_AIC(attributes(summary(mle_displaced_geometric))$m2logL, 1, N) # AIC for displaced geometric distribution





#############################################################################
#############################################################################

## EXTRA

                      ## ALTMANN ##

minus_log_likelihood_altmann<- function(gamma, delta) {
  
  temp=0
  
  for (k in 1:N){
    temp=temp+ (k^(-gamma))*(exp(-delta*k))
  }
  
  c= 1/temp
  
  gamma*M_P + delta*M - N*log(c)
  
}

mle_altmann <- mle(minus_log_likelihood_altmann,
                               start = list(gamma = 0,delta=0),
                               method = "L-BFGS-B"
                               ,lower = c(0.000000,0.000000)
                                   
)


summary(mle_altmann)








