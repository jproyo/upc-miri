
require("stats4") # for MLE
require("VGAM") # for the Riemann-zeta function

library(ggplot2)

## TAREA 1 

table = read.table("/Users/mac/Desktop/netbeans_projects/csn_lab6/data/degree_distribution2", header = FALSE, sep = " ", dec = ".")
degree_sequence = read.table("/Users/mac/Desktop/netbeans_projects/csn_lab6/data/degree_of_each_node2",header = FALSE)

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
##segurament es per culpa de la variability quan degree puja...
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
#points(log(x), log(y_1), col = "green", pch = 20)


## MODEL 2##
q= 0.500005

x <- seq(0,MAX,1)
y <- q*(1-q)^(x-1)

plot(log(table$V1),log(table$V2), col = "blue", pch = 20,main = "Growth + Rand.Attachment: Degree distribution at time tmax", 
     xlab = "log degree",
     ylab = "log probability")
points(log(x), log(y), col = "red", pch = 20)
























## TAREA 2 MODEL 1

################################################################################################
degree_over_time_1 = read.table("/Users/mac/Desktop/netbeans_projects/csn_lab6/data/degree_over_time_11",header = FALSE)
degree_over_time_10 = read.table("/Users/mac/Desktop/netbeans_projects/csn_lab6/data/degree_over_time_101",header = FALSE)
degree_over_time_100 = read.table("/Users/mac/Desktop/netbeans_projects/csn_lab6/data/degree_over_time_1001",header = FALSE)
degree_over_time_1000 = read.table("/Users/mac/Desktop/netbeans_projects/csn_lab6/data/degree_over_time_10001",header = FALSE)

#plot(degree_over_time_1000$V1[0:10000],degree_over_time_1000$V2[0:10000], col = "blue", pch = 20)

plot(degree_over_time_1$V1,degree_over_time_1$V2, col = "blue", pch = 20)
plot(degree_over_time_10$V1,degree_over_time_10$V2, col = "blue", pch = 20)
plot(degree_over_time_100$V1,degree_over_time_100$V2, col = "blue", pch = 20)
plot(degree_over_time_1000$V1,degree_over_time_1000$V2, col = "blue", pch = 20)

degree_over_time_1$V2= 1^0.5*degree_over_time_1$V2
degree_over_time_10$V2= 10^0.5*degree_over_time_10$V2
degree_over_time_100$V2= 100^0.5*degree_over_time_100$V2
degree_over_time_1000$V2= 1000^0.5*degree_over_time_1000$V2

plot(degree_over_time_1$V1, degree_over_time_1$V2, col = "blue", pch = 20,main = "Time growth degree: Growth + pref.attachment", xlab = "Time",ylab = "Degree")
points(degree_over_time_10$V1, degree_over_time_10$V2, col = "red", pch = 20)
points(degree_over_time_100$V1, degree_over_time_100$V2, col = "yellow", pch = 20)
points(degree_over_time_1000$V1, degree_over_time_1000$V2, col = "green", pch = 20)
legend("bottomright", legend=c("ti=1","ti=10","ti=100","ti=1000"),col=c("blue", "red", "yellow", "green"),lty=1:2, cex=0.8)






################################################################################################

linear_model = lm(log(degree_over_time_10$V2)~log(degree_over_time_10$V1), degree_over_time_10)
a_initial = exp(coef(linear_model)[1])
b_initial = coef(linear_model)[2]

## at
nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1,data=degree_over_time_10,start = list(a = a_initial), trace = TRUE)

deviance(nonlinear_model)
AIC(nonlinear_model)
s= sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
coef(nonlinear_model)

plot(log(degree_over_time_10$V1), log(degree_over_time_10$V2),xlab = "log(vertices)", ylab = "log(mean dependency length)")
lines(log(degree_over_time_10$V1), log(fitted(nonlinear_model)), col = "green")

## at^(1/2) 
nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1^(1/2),data=degree_over_time_10,start = list(a = a_initial), trace = TRUE)

deviance(nonlinear_model)
AIC(nonlinear_model)
s= sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
coef(nonlinear_model)

plot(log(degree_over_time_10$V1), log(degree_over_time_10$V2),xlab = "log(vertices)", ylab = "log(mean dependency length)")
lines(log(degree_over_time_10$V1), log(fitted(nonlinear_model)), col = "green")

## at^(b)
nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1^b,data=degree_over_time_10,start = list(a = a_initial, b = b_initial), trace = TRUE)

deviance(nonlinear_model)
AIC(nonlinear_model)
s= sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
coef(nonlinear_model)

plot(log(degree_over_time_10$V1), log(degree_over_time_10$V2),xlab = "log(vertices)", ylab = "log(mean dependency length)")
lines(log(degree_over_time_10$V1), log(fitted(nonlinear_model)), col = "green")






linear_model = lm(log(degree_over_time_10$V2)~degree_over_time_10$V1, degree_over_time_10)
a_initial = exp(coef(linear_model)[1])
b_initial = coef(linear_model)[2]

## ae^(ct)
nonlinear_model = nls(degree_over_time_10$V2~a*exp(degree_over_time_10$V1*b),data=degree_over_time_10,start = list(a = a_initial, b = b_initial), trace = TRUE)

deviance(nonlinear_model)
AIC(nonlinear_model)
s= sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
coef(nonlinear_model)

plot(log(degree_over_time_10$V1), log(degree_over_time_10$V2),xlab = "log(vertices)", ylab = "log(mean dependency length)")
lines(log(degree_over_time_10$V1), log(fitted(nonlinear_model)), col = "green")





 

linear_model = lm(log(degree_over_time_10$V2)~log(degree_over_time_10$V1), degree_over_time_10)
a_initial = exp(coef(linear_model)[1])
b_initial = coef(linear_model)[2]

## a*log(t+d1)
nonlinear_model = nls(degree_over_time_10$V2~a*log(degree_over_time_10$V1 + b),data=degree_over_time_10,start = list(a = a_initial, b = b_initial), trace = TRUE,
                      algorithm="port", lower = 0.0000001)

deviance(nonlinear_model)
AIC(nonlinear_model)
s= sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
coef(nonlinear_model)

plot(log(degree_over_time_10$V1), log(degree_over_time_10$V2),xlab = "log(vertices)", ylab = "log(mean dependency length)")
lines(log(degree_over_time_10$V1), log(fitted(nonlinear_model)), col = "green")
















































## TAREA 2 MODEL 2

################################################################################################
degree_over_time_1 = read.table("/Users/mac/Desktop/netbeans_projects/csn_lab6/data/degree_over_time_12",header = FALSE)
degree_over_time_10 = read.table("/Users/mac/Desktop/netbeans_projects/csn_lab6/data/degree_over_time_102",header = FALSE)
degree_over_time_100 = read.table("/Users/mac/Desktop/netbeans_projects/csn_lab6/data/degree_over_time_1002",header = FALSE)
degree_over_time_1000 = read.table("/Users/mac/Desktop/netbeans_projects/csn_lab6/data/degree_over_time_10002",header = FALSE)

#plot(degree_over_time_1000$V1[0:10000],degree_over_time_1000$V2[0:10000], col = "blue", pch = 20)

plot(degree_over_time_1$V1,degree_over_time_1$V2, col = "blue", pch = 20)
plot(degree_over_time_10$V1,degree_over_time_10$V2, col = "blue", pch = 20)
plot(degree_over_time_100$V1,degree_over_time_100$V2, col = "blue", pch = 20)
plot(degree_over_time_1000$V1,degree_over_time_1000$V2, col = "blue", pch = 20)

degree_over_time_1$V2= degree_over_time_1$V2 + M_0*log(N_0+ 1 -1) -M_0
degree_over_time_10$V2= degree_over_time_10$V2 + M_0*log(N_0+ 10 -1) -M_0
degree_over_time_100$V2= degree_over_time_100$V2 + M_0*log(N_0+ 100 -1) -M_0
degree_over_time_1000$V2= degree_over_time_1000$V2 + M_0*log(N_0+ 1000 -1) -M_0

plot(degree_over_time_1$V1,degree_over_time_1$V2, col = "blue", pch = 20)
plot(degree_over_time_10$V1,degree_over_time_10$V2, col = "blue", pch = 20)
plot(degree_over_time_100$V1,degree_over_time_100$V2, col = "blue", pch = 20)
plot(degree_over_time_1000$V1,degree_over_time_1000$V2, col = "blue", pch = 20)



################################################################################################


linear_model = lm(log(degree_over_time_10$V2)~log(degree_over_time_10$V1), degree_over_time_10)
a_initial = exp(coef(linear_model)[1])
b_initial = coef(linear_model)[2]

## at
nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1,data=degree_over_time_10,start = list(a = a_initial), trace = TRUE)

deviance(nonlinear_model)
AIC(nonlinear_model)
s= sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
coef(nonlinear_model)

plot(log(degree_over_time_10$V1), log(degree_over_time_10$V2),xlab = "log(vertices)", ylab = "log(mean dependency length)")
lines(log(degree_over_time_10$V1), log(fitted(nonlinear_model)), col = "green")

## at^(1/2) 
nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1^(1/2),data=degree_over_time_10,start = list(a = a_initial), trace = TRUE)

deviance(nonlinear_model)
AIC(nonlinear_model)
s= sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
coef(nonlinear_model)

plot(log(degree_over_time_10$V1), log(degree_over_time_10$V2),xlab = "log(vertices)", ylab = "log(mean dependency length)")
lines(log(degree_over_time_10$V1), log(fitted(nonlinear_model)), col = "green")

## at^(b)
nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1^b,data=degree_over_time_10,start = list(a = a_initial, b = b_initial), trace = TRUE)

deviance(nonlinear_model)
AIC(nonlinear_model)
s= sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
coef(nonlinear_model)

plot(log(degree_over_time_10$V1), log(degree_over_time_10$V2),xlab = "log(vertices)", ylab = "log(mean dependency length)")
lines(log(degree_over_time_10$V1), log(fitted(nonlinear_model)), col = "green")






linear_model = lm(log(degree_over_time_10$V2)~degree_over_time_10$V1, degree_over_time_10)
a_initial = exp(coef(linear_model)[1])
b_initial = coef(linear_model)[2]

## ae^(ct)
nonlinear_model = nls(degree_over_time_10$V2~a*exp(degree_over_time_10$V1*b),data=degree_over_time_10,start = list(a = a_initial, b = b_initial), trace = TRUE)

deviance(nonlinear_model)
AIC(nonlinear_model)
s= sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
coef(nonlinear_model)

plot(log(degree_over_time_10$V1), log(degree_over_time_10$V2),xlab = "log(vertices)", ylab = "log(mean dependency length)")
lines(log(degree_over_time_10$V1), log(fitted(nonlinear_model)), col = "green")










