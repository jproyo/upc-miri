######################################################
## Authors: Juan Pablo Royo Sales & Guillem
## Title: Homework 4
## Dae: 2020-11-15
####################################################

require("stats4") # for MLE

# Validate Data
validate_metric <- function(language, N, k2, d){
  first <- k2 >= (4-6)/N && k2 <= N-1
  second <- ((N/(8*(N-1)))*k2)+0.5 <= d && d <= N - 1
  if(first && second){
    print(paste("Language ", language, " IS VALID"))
  }else{
    print(paste("Language ", language, " IS INVALID"))
  }
}

# Extra data info of each language
extract_info <- function(language, file) {
  dep_tree = read.table(file, header = FALSE)
  colnames(dep_tree) = c("vertices","degree_2nd_moment", "mean_length")
  k2 <- dep_tree$degree_2nd_moment
  d <- dep_tree$mean_length
  N <- length(dep_tree$vertices)
  mu_n <- mean(dep_tree$vertices)
  sd_n <- sd(dep_tree$vertices)
  mu_x <- mean(dep_tree$mean_length)
  sd_x <- sd(dep_tree$mean_length)
  validate_metric(language,N,k2,d)
  c(language,N, mu_n, sd_n, mu_x, sd_x)
}


# Print extracted data for each language in a table
print_summary_table <- function(data){
  df <- data.frame(data)
  colnames(df) <- c("Language", "N", "mu_n", "sigma_n","mu_x", "sigma_x")
  print.data.frame(df, right=FALSE, row.names=FALSE)
}


# Plot data for each language 
plot_language <- function(language, file){
  dep_tree = read.table(file, header = FALSE)
  colnames(dep_tree) = c("vertices","degree_2nd_moment", "mean_length")
  dep_tree = dep_tree[order(dep_tree$vertices), ]
  mean_dep_tree = aggregate(dep_tree, list(dep_tree$vertices), mean)
  plot(log(dep_tree$vertices), log(dep_tree$mean_length),main = paste(language, ' - Mean Length'),
       xlab = "log(vertices)", ylab = "log(mean dependency length)")
  lines(log(dep_tree$vertices),log(dep_tree$mean_length), col = "green")
  lines(log(dep_tree$vertices),log((dep_tree$mean_length+1)/3), col = "red")
  plot(log(mean_dep_tree$vertices), log(mean_dep_tree$mean_length),main = paste(language, ' - Average Mean Length'),
       xlab = "log(vertices)", ylab = "log(mean dependency length)")
}

# Check if the model has homocesdasticity
has_homocesdasticity <- function(ds){
  vars = aggregate(ds, list(ds$vertices), var)
  vars = vars[!apply(vars[, 3:4] == 0, 1, FUN = any, na.rm = TRUE),]
  vars = na.omit(vars) 
  
  f_max = max(vars$mean_length) / min(vars$mean_length)
  diff_f = 0.5
  
  return (f_max <= 1 + diff_f && delta >= 1 - diff_f)
}


# Fitting models
fit_models <- function(language, file){
  dep_tree = read.table(file, header = FALSE)
  colnames(dep_tree) = c("vertices","degree_2nd_moment", "mean_length")
  dep_tree = dep_tree[order(dep_tree$vertices), ]
  if(has_homocesdasticity(dep_tree)){
    print("Has Homocesdasticity")
  }else{
    print("DOES NOT HAVE Homocesdasticity. Using agregate")
    dep_tree = aggregate(dep_tree, list(dep_tree$vertices), mean)
  }
  
  min_aic <- NULL
  min_aic_model <- NULL
  min_model_name <- "DEFAULT"

  # Running Model 0
  print(paste("Running Non Linear Model 0 for ", language))
  rss_0 = sum((dep_tree$mean_length-(dep_tree$vertices+1)/3)^2)
  n = length(dep_tree$vertices)
  p = 0
  s_0 = sqrt(rss_0/(n - p))
  aic_0 = n*log(2*pi) + n*log(rss_0/n) + n + 2*(p + 1)
  print(paste("RSS: ", rss_0, " - AIC: ", aic_0, " - s:" , s_0))
  min_aic <- aic_0

  # Running Model 1 f(n) = (n/2)^b
  linear_model_1 = lm(log(mean_length)~log(vertices), dep_tree)
  b_initial_1 = coef(linear_model_1)[2]
  print(paste("Running Non Linear Model 1 for ", language))
  nonlinear_model_1 = nls(mean_length~(vertices/2)^b,data=dep_tree,
                          start = list(b = b_initial_1), trace = TRUE)
  
  rss_1 = deviance(nonlinear_model_1)
  aic_1 = AIC(nonlinear_model_1)
  s_1 = sqrt(deviance(nonlinear_model_1)/df.residual(nonlinear_model_1))
  print(paste("RSS: ", rss_1, " - AIC: ", aic_1, " - s:" , s_1))
  if(min_aic >= aic_1){
    min_aic <- aic_1
    min_aic_model <- nonlinear_model_1
    min_model_name <- "Model 1"
  }
      
  # Running Model 2 f(n) = an^b
  linear_model_2 = lm(log(mean_length)~log(vertices), dep_tree)
  a_initial_2 = exp(coef(linear_model_2)[1])
  b_initial_2 = coef(linear_model_2)[2]
  print(paste("Running Non Linear Model 2 for ", language))
  nonlinear_model_2 = nls(mean_length~a*vertices^b,data=dep_tree,
      start = list(a = a_initial_2, b = b_initial_2), trace = TRUE)
  
  rss_2 = deviance(nonlinear_model_2)
  aic_2 = AIC(nonlinear_model_2)
  s_2 = sqrt(deviance(nonlinear_model_2)/df.residual(nonlinear_model_2))
  print(paste("RSS: ", rss_2, " - AIC: ", aic_2, " - s:" , s_2))
  if(min_aic >= aic_2){
    min_aic <- aic_2
    min_aic_model <- nonlinear_model_2
    min_model_name <- "Model 2"
  }
  
  
  # Running Model 3 f(n) = ae^{cn}
  linear_model_3 = lm(log(mean_length) ~ vertices, dep_tree)
  a_initial_3 = exp(coef(linear_model_3)[1])
  c_initial_3 = coef(linear_model_3)[2]
  print(paste("Running Non Linear Model 3 for ", language))
  nonlinear_model_3 = nls(mean_length~a*exp(c*vertices),data=dep_tree,
                          start = list(a = a_initial_3, c = c_initial_3), trace = TRUE)
  
  rss_3 = deviance(nonlinear_model_3)
  aic_3 = AIC(nonlinear_model_3)
  s_3 = sqrt(deviance(nonlinear_model_3)/df.residual(nonlinear_model_3))
  print(paste("RSS: ", rss_3, " - AIC: ", aic_3, " - s:" , s_3))
  if(min_aic >= aic_3){
    min_aic <- aic_3
    min_aic_model <- nonlinear_model_3
    min_model_name <- "Model 3"
  }
  
  # Running Model 4 f(n) = an^b e^{cn}
  linear_model_4 = lm(log(mean_length) ~ vertices, dep_tree)
  a_initial_4 = exp(coef(linear_model_4)[1])
  b_initial_4 = coef(linear_model_4)[1]
  c_initial_4 = coef(linear_model_4)[2]
  print(paste("Running Non Linear Model 4 for ", language))
  nonlinear_model_4 = nls(mean_length~(a*(vertices)^b)*exp(c*vertices),data=dep_tree,
                          control = nls.control(warnOnly = TRUE),
                          start = list(a = a_initial_4,b = b_initial_4,c = c_initial_4), trace = TRUE, algorithm="port", lower = 0.0000001)
  
  rss_4 = deviance(nonlinear_model_4)
  aic_4 = AIC(nonlinear_model_4)
  s_4 = sqrt(deviance(nonlinear_model_4)/df.residual(nonlinear_model_4))
  print(paste("RSS: ", rss_4, " - AIC: ", aic_4, " - s:" , s_4))
  if(min_aic >= aic_4){
    min_aic <- aic_4
    min_aic_model <- nonlinear_model_4
    min_model_name <- "Model 4"
  }
  
    # Running Model +1 f(n) = (n/2)^b + d
  linear_model_plus_1 = lm(log(mean_length) ~ log(vertices), dep_tree)
  b_initial_p_1 = coef(linear_model_plus_1)[2]
  d_initial_p_1 = 0
  print(paste("Running Non Linear Model +1 for ", language))
  nonlinear_model_p_1 = nls(mean_length~((vertices/2)^b)+d,data=dep_tree,
                          start = list(b = b_initial_p_1, d = d_initial_p_1), trace = TRUE)
  
  rss_p_1 = deviance(nonlinear_model_p_1)
  aic_p_1 = AIC(nonlinear_model_p_1)
  s_p_1 = sqrt(deviance(nonlinear_model_p_1)/df.residual(nonlinear_model_p_1))
  print(paste("RSS: ", rss_p_1, " - AIC: ", aic_p_1, " - s:" , s_p_1))
  if(min_aic >= aic_p_1){
    min_aic <- aic_p_1
    min_aic_model <- nonlinear_model_p_1
    min_model_name <- "Model +1"
  }
  
  
  # Running Model +2 f(n) = an^b + d
  linear_model_plus_2 = lm(log(mean_length) ~ log(vertices), dep_tree)
  a_initial_p_2 = exp(coef(linear_model_plus_2)[1])
  b_initial_p_2 = coef(linear_model_plus_2)[2]
  d_initial_p_2 = 1
  print(paste("Running Non Linear Model +2 for ", language))
  nonlinear_model_p_2 = nls(mean_length~(a*vertices^b)+d,data=dep_tree,
                            start = list(a = a_initial_p_2, b = b_initial_p_2, d = d_initial_p_2)
                            , trace = TRUE)
  
  rss_p_2 = deviance(nonlinear_model_p_2)
  aic_p_2 = AIC(nonlinear_model_p_2)
  s_p_2 = sqrt(deviance(nonlinear_model_p_2)/df.residual(nonlinear_model_p_2))
  print(paste("RSS: ", rss_p_2, " - AIC: ", aic_p_2, " - s:" , s_p_2))
  if(min_aic >= aic_p_2){
    min_aic <- aic_p_2
    min_aic_model <- nonlinear_model_p_2
    min_model_name <- "Model +2"
  }
  
  
  # Running Model +3 f(n) = ae^{cn} + d
  linear_model_p_3 = lm(log(mean_length) ~ vertices, dep_tree)
  a_initial_p_3 = exp(coef(linear_model_p_3)[1])
  c_initial_p_3 = coef(linear_model_p_3)[2]
  d_initial_p_3 = 1
  print(paste("Running Non Linear Model +3 for ", language))
  nonlinear_model_p_3 = nls(mean_length~(a*exp(c*vertices))+d,data=dep_tree,
                          start = list(a = a_initial_p_3, c = c_initial_p_3, d = d_initial_p_3), trace = TRUE, algorithm="port", lower=0)
  
  rss_p_3 = deviance(nonlinear_model_p_3)
  aic_p_3 = AIC(nonlinear_model_p_3)
  s_p_3 = sqrt(deviance(nonlinear_model_p_3)/df.residual(nonlinear_model_p_3))
  print(paste("RSS: ", rss_p_3, " - AIC: ", aic_p_3, " - s:" , s_p_3))
  if(min_aic >= aic_p_3){
    min_aic <- aic_p_3
    min_aic_model <- nonlinear_model_p_3
    min_model_name <- "Model +3"
  }
  
  # Running Model +4 f(n) = an^b e^{cn} + d
  linear_model_p_4 = lm(log(mean_length) ~ vertices, dep_tree)
  a_initial_p_4 = exp(coef(linear_model_p_4)[1])
  b_initial_p_4 = coef(linear_model_p_4)[1]
  c_initial_p_4 = coef(linear_model_p_4)[2]
  d_initial_p_4 = 1
  print(paste("Running Non Linear Model +4 for ", language))
  nonlinear_model_p_4 = nls(mean_length~(a*(vertices)^b)*exp(c*vertices)+d,data=dep_tree,
                          control = nls.control(warnOnly = TRUE),
                          start = list(a = a_initial_p_4,b = b_initial_p_4,c = c_initial_p_4, d= d_initial_p_4), trace = TRUE, algorithm="port", lower=0.00001)
  
  rss_p_4 = deviance(nonlinear_model_p_4)
  aic_p_4 = AIC(nonlinear_model_p_4)
  s_p_4 = sqrt(deviance(nonlinear_model_p_4)/df.residual(nonlinear_model_p_4))
  print(paste("RSS: ", rss_p_4, " - AIC: ", aic_p_4, " - s:" , s_p_4))
  if(min_aic >= aic_p_4){
    min_aic <- aic_p_4
    min_aic_model <- nonlinear_model_p_4
    min_model_name <- "Model +4"
  }

  print(paste("Best AIC for language ",language, " = ", min_aic))
  
  plot(log(dep_tree$vertices), log(dep_tree$mean_length), main = paste("Language: ",language, " - ", min_model_name),
       xlab = "log(vertices)", ylab = "log(mean dependency length)")
  lines(log(dep_tree$vertices), log(fitted(min_aic_model)), col = "green")

  c(
    s_0, aic_0, aic_0-min_aic
    , s_1, aic_1, aic_1-min_aic
    , s_2, aic_2, aic_2-min_aic
    , s_3, aic_3, aic_3-min_aic
    , s_4, aic_4, aic_4-min_aic
    , s_p_1, aic_p_1, aic_p_1-min_aic
    , s_p_2, aic_p_2, aic_p_2-min_aic
    , s_p_3, aic_p_3, aic_p_3-min_aic
    , s_p_4, aic_p_4, aic_p_4-min_aic
    , coef(nonlinear_model_1)[1]
    , coef(nonlinear_model_2)[1], coef(nonlinear_model_2)[2]
    , coef(nonlinear_model_3)[1], coef(nonlinear_model_3)[2]
    , coef(nonlinear_model_4)[1], coef(nonlinear_model_4)[2], coef(nonlinear_model_4)[3]
    , coef(nonlinear_model_p_1)[1], coef(nonlinear_model_p_1)[2]
    , coef(nonlinear_model_p_2)[1], coef(nonlinear_model_p_2)[2], coef(nonlinear_model_p_2)[3]
    , coef(nonlinear_model_p_3)[1], coef(nonlinear_model_p_3)[2], coef(nonlinear_model_p_3)[3]
    , coef(nonlinear_model_p_4)[1], coef(nonlinear_model_p_4)[2], coef(nonlinear_model_p_4)[3], coef(nonlinear_model_p_4)[4]
  )
}

# Print extracted data for each language in a table
print_tables_2_3 <- function(data, languages){
  df <- data.frame(data)
  colnames(df) = c("s 0", "AIC 0", "AIC delta 0","s 1", "AIC 1", "AIC delta 1", "s 2", "AIC 2", "AIC delta 2", "s 3", "AIC 3", "AIC delta 3", "s 4", "AIC 4", "AIC delta 4", "s +1", "AIC +1", "AIC delta +1","s +2", "AIC +2", "AIC delta +2","s +3", "AIC +3", "AIC delta +3", "s +4", "AIC +4", "AIC delta +4", "Model 1 (b)", "Model 2 (a)", "Model 2 (b)",  "Model 3 (a)", "Model 3 (c)", "Model 4 (a)","Model 4 (b)","Model 4 (c)","Model +1 (b)", "Model +1 (d)",  "Model +2 (a)", "Model +2 (b)", "Model +2 (d)", "Model +3 (a)", "Model +3 (c)", "Model +3 (d)", "Model +4 (a)", "Model +4 (b)", "Model +4 (c)", "Model +4 (d)")
  rownames(df) = c(languages)
  df_1 = subset(df, select=c("s 0", "AIC 0", "AIC delta 0","s 1", "AIC 1", "AIC delta 1", "s 2", "AIC 2", "AIC delta 2", "s 3", "AIC 3", "AIC delta 3", "s 4", "AIC 4", "AIC delta 4", "s +1", "AIC +1", "AIC delta +1","s +2", "AIC +2", "AIC delta +2","s +3", "AIC +3", "AIC delta +3", "s +4", "AIC +4", "AIC delta +4"))
  df_2 = subset(df, select=c( "Model 1 (b)", "Model 2 (a)", "Model 2 (b)",  "Model 3 (a)", "Model 3 (c)",  "Model 4 (a)","Model 4 (b)","Model 4 (c)", "Model +1 (b)", "Model +1 (d)",  "Model +2 (a)", "Model +2 (b)", "Model +2 (d)", "Model +3 (a)", "Model +3 (c)", "Model +3 (d)", "Model +4 (a)", "Model +4 (b)", "Model +4 (c)", "Model +4 (d)"))
  print.data.frame(df_1, right=FALSE)
  print.data.frame(df_2, right=FALSE)
}


main <- function(){
  # Read all in-degree files to iterate each language
  source <- read.table("list.txt", header = TRUE, as.is=c("language","file"))
  
  # Print Summary table of all parameters
  result <- NULL
  for (x in 1:nrow(source)) {
    result <- rbind(result, extract_info(source$language[x], source$file[x]))
  }
  print_summary_table(result)

  # Potting
  # for (x in 1:nrow(source)) {
  #   plot_language(source$language[x], source$file[x])
  # }

  # Fitting models
  result_models <- NULL
  for (x in 1:nrow(source)) {
    result_models <- rbind(result_models, fit_models(source$language[x], source$file[x]))
  }
  print_tables_2_3(result_models,result[,1])
  
}

## RUN PROGRAM
main()


