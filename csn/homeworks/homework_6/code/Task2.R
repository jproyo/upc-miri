######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 6
## Dae: 2020-12-13
####################################################

require("stats4") # for MLE
require("VGAM") # for the Riemann-zeta function

library(ggplot2)

setwd("~/Projects/upc/upc-miri/csn/homeworks/homework_6/code")

M_0=1
N_0=2

plot_degree_over_time <- function(filename, escalate_fn){
  degree_over_time_1 = read.table(paste("data/",filename,"_1",sep = ""),header = FALSE)
  degree_over_time_10 = read.table(paste("data/",filename,"_10",sep = ""),header = FALSE)
  degree_over_time_100 = read.table(paste("data/",filename,"_100",sep = ""),header = FALSE)
  degree_over_time_1000 = read.table(paste("data/",filename,"_1000",sep = ""),header = FALSE)
  
  degree_over_time_1$V2= get(escalate_fn)(1, degree_over_time_1$V2)
  degree_over_time_10$V2= get(escalate_fn)(10, degree_over_time_10$V2)
  degree_over_time_100$V2= get(escalate_fn)(100, degree_over_time_100$V2)  
  degree_over_time_1000$V2= get(escalate_fn)(1000, degree_over_time_1000$V2) 
  
  plot(degree_over_time_1$V1, degree_over_time_1$V2, col = "blue", pch = 20,main = paste("Time growth degree: ", filename), xlab = "Time",ylab = "Degree")
  points(degree_over_time_10$V1, degree_over_time_10$V2, col = "red", pch = 20)
  points(degree_over_time_100$V1, degree_over_time_100$V2, col = "yellow", pch = 20)
  points(degree_over_time_1000$V1, degree_over_time_1000$V2, col = "green", pch = 20)
  legend("bottomright", legend=c("ti=1","ti=10","ti=100","ti=1000"),col=c("blue", "red", "yellow", "green"),lty=1:2, cex=0.8)
}

escalate_pref_att <- function(time, degree){
  return (time^0.5*degree)
}

escalate_rand_att <- function(time, degree){
  return (degree + M_0*log(N_0+ time -1) -M_0)
}

plot_models <- function(){
  plot_degree_over_time("degree_over_time_pref_att", "escalate_pref_att")
  plot_degree_over_time("degree_over_time_rand_att", "escalate_rand_att")
}

plot_degree_over_time("degree_over_time_rand_att", "escalate_rand_att")



plot_best_aic <- function(filename){
  best_aic <- 0
  min_aic_model <- NULL
  min_model_name <- "Model 0"

  degree_over_time_10 = read.table(paste("data/",filename,"_10",sep = ""),header = FALSE)
  
  # Model 0
  linear_model = lm(log(degree_over_time_10$V2)~log(degree_over_time_10$V1), degree_over_time_10)
  a_initial = exp(coef(linear_model)[1])
  b_initial = coef(linear_model)[2]
  
  ## at
  nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1,data=degree_over_time_10,start = list(a = a_initial), trace = TRUE)
  
  rss_0 = deviance(nonlinear_model)
  aic_0 = AIC(nonlinear_model)
  s_0 = sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
  
  best_aic <- aic_0
  min_aic_model <- nonlinear_model
  min_model_name <- "Model 0"
    
  # Model 1
  ## at^(1/2) 
  nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1^(1/2),data=degree_over_time_10,start = list(a = a_initial), trace = TRUE)
  
  rss_1 = deviance(nonlinear_model)
  aic_1 = AIC(nonlinear_model)
  s_1 = sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
  
  if(best_aic >= aic_1){
    best_aic <- aic_1
    min_aic_model <- nonlinear_model
    min_model_name <- "Model 1"
  }
  
  # Model 2
  ## at^(b)
  nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1^b,data=degree_over_time_10,start = list(a = a_initial, b = b_initial), trace = TRUE)
  
  rss_2 = deviance(nonlinear_model)
  aic_2 = AIC(nonlinear_model)
  s_2 = sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
  
  if(best_aic >= aic_2){
    best_aic <- aic_2
    min_aic_model <- nonlinear_model
    min_model_name <- "Model 2"
  }
  
  
  # Model 3
  linear_model = lm(log(degree_over_time_10$V2)~degree_over_time_10$V1, degree_over_time_10)
  a_initial = exp(coef(linear_model)[1])
  b_initial = coef(linear_model)[2]
  
  ## ae^(ct)
  nonlinear_model = nls(degree_over_time_10$V2~a*exp(degree_over_time_10$V1*b),data=degree_over_time_10,start = list(a = a_initial, b = b_initial), trace = TRUE)
  
  rss_3 = deviance(nonlinear_model)
  aic_3 = AIC(nonlinear_model)
  s_3 = sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))

  if(best_aic >= aic_3){
    best_aic <- aic_3
    min_aic_model <- nonlinear_model
    min_model_name <- "Model 3"
  }
  
  # Model 4
  linear_model = lm(log(degree_over_time_10$V2)~log(degree_over_time_10$V1), degree_over_time_10)
  a_initial = exp(coef(linear_model)[1])
  b_initial = coef(linear_model)[2]

  ## a*log(t+d1)
  nonlinear_model = nls(degree_over_time_10$V2~a*log(degree_over_time_10$V1 + b),data=degree_over_time_10,start = list(a = a_initial, b = b_initial), trace = TRUE,
                        algorithm="port", lower = 0.0000001, control = nls.control(maxiter = 10, warnOnly= T))

  rss_4 = deviance(nonlinear_model)
  aic_4 = AIC(nonlinear_model)
  s_4 = sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))

  if(best_aic >= aic_4){
    best_aic <- aic_4
    min_aic_model <- nonlinear_model
    min_model_name <- "Model 4"
  }
  

  # Model +0
  linear_model = lm(log(degree_over_time_10$V2)~log(degree_over_time_10$V1), degree_over_time_10)
  a_initial = exp(coef(linear_model)[1])
  d_initial = 1
  
  ## at
  nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1+d,data=degree_over_time_10,start = list(a = a_initial, d = d_initial) , trace = TRUE)
  
  rss_p_0 = deviance(nonlinear_model)
  aic_p_0 = AIC(nonlinear_model)
  s_p_0 = sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
  
  if(best_aic >= aic_p_0){
    best_aic <- aic_p_0
    min_aic_model <- nonlinear_model
    min_model_name <- "Model 0+"
  }
  
  
  # Model +1
  ## at^(1/2) + d
  nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1^(1/2) + d,data=degree_over_time_10,start = list(a = a_initial, d = d_initial), trace = TRUE)
  
  rss_p_1 = deviance(nonlinear_model)
  aic_p_1 = AIC(nonlinear_model)
  s_p_1 = sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
  
  if(best_aic >= aic_p_1){
    best_aic <- aic_p_1
    min_aic_model <- nonlinear_model
    min_model_name <- "Model 1+"
  }
      
  
  # Model 2+
  ## at^(b) + d
  nonlinear_model = nls(degree_over_time_10$V2~a*degree_over_time_10$V1^b + d,data=degree_over_time_10,start = list(a = a_initial, b = b_initial, d = d_initial), trace = TRUE
                        ,control = nls.control(maxiter = 10, warnOnly= T))
  
  rss_p_2 = deviance(nonlinear_model)
  aic_p_2 = AIC(nonlinear_model)
  s_p_2 = sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
  
  if(best_aic >= aic_p_2){
    best_aic <- aic_p_2
    min_aic_model <- nonlinear_model
    min_model_name <- "Model 2+"
  }
  
  # Model 3+
  linear_model = lm(log(degree_over_time_10$V2)~degree_over_time_10$V1, degree_over_time_10)
  a_initial = exp(coef(linear_model)[1])
  b_initial = coef(linear_model)[2]
  d_initial = 1
  
  ## ae^(ct) + d
  nonlinear_model = nls(degree_over_time_10$V2~a*exp(degree_over_time_10$V1*b) + d,data=degree_over_time_10
                        ,start = list(a = a_initial, b = b_initial, d = d_initial), trace = TRUE
                        , algorithm="port", lower=0.0000001,control = nls.control(maxiter = 10, warnOnly= T))
  
  rss_p_3 = deviance(nonlinear_model)
  aic_p_3 = AIC(nonlinear_model)
  s_p_3 = sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
  
  if(best_aic >= aic_p_3){
    best_aic <- aic_p_3
    min_aic_model <- nonlinear_model
    min_model_name <- "Model 3+"
  }
  
  
  # Model 4+
  linear_model = lm(log(degree_over_time_10$V2)~log(degree_over_time_10$V1), degree_over_time_10)
  a_initial = exp(coef(linear_model)[1])
  b_initial = coef(linear_model)[2]
  d_initial = 1
  
  ## a*log(t+d1) + d2
  nonlinear_model = nls(degree_over_time_10$V2~a*log(degree_over_time_10$V1 + b) + d,data=degree_over_time_10,start = list(a = a_initial, b = b_initial, d = d_initial), trace = TRUE,
                        algorithm="port", lower = 0.0000001, control = nls.control(maxiter = 10, warnOnly= T))
  
  rss_p_4 = deviance(nonlinear_model)
  aic_p_4 = AIC(nonlinear_model)
  s_p_4 = sqrt(deviance(nonlinear_model)/df.residual(nonlinear_model))
  
  if(best_aic >= aic_p_4){
    best_aic <- aic_p_4
    min_aic_model <- nonlinear_model
    min_model_name <- "Model 4+"
  }
  
  print(filename)
  print("AIC Model & Value")
  print(paste("Model 0", " & ", aic_0))
  print(paste("Model 1", " & ", aic_1))
  print(paste("Model 2", " & ", aic_2))
  print(paste("Model 3", " & ", aic_3))
  print(paste("Model 4", " & ", aic_4))
  print(paste("Model 0+", " & ", aic_p_0))
  print(paste("Model 1+", " & ", aic_p_1))
  print(paste("Model 2+", " & ", aic_p_2))
  print(paste("Model 3+", " & ", aic_p_3))
  print(paste("Model 4+", " & ", aic_p_4))

  
  plot(log(degree_over_time_10$V1), log(degree_over_time_10$V2),xlab = "log(vertices)", ylab = "log(mean dependency length)", main = paste("Best Model", min_model_name, ":", filename))
  lines(log(degree_over_time_10$V1), log(fitted(min_aic_model)), col = "green")

}

plot_best_aic("degree_over_time_pref_att")
plot_best_aic("degree_over_time_rand_att")






