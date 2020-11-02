######################################################
## Authors: Juan Pablo Royo Sales & Guillem
## Title: Homework 4
## Dae: 2020-11-15
####################################################

require("stats4") # for MLE
require("VGAM") # for the Riemann-zeta function


# Extra data info of each language
extract_info <- function(language, file) {
  dep_tree = read.table(file, header = FALSE)
  N <- length(dep_tree$V1)
  mu_n <- mean(dep_tree$V1)
  sd_n <- sd(dep_tree$V1)
  mu_x <- mean(dep_tree$V3)
  sd_x <- sd(dep_tree$V3)
  c(language,N, mu_n, sd_n, mu_x, sd_x)
}


# Print extracted data for each language in a table
print_summary_table <- function(data){
  df <- data.frame(data)
  colnames(df) <- c("Language", "N", "mu_n", "sigma_n","mu_x", "sigma_x")
  print.data.frame(df, right=FALSE, row.names=FALSE)
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

    
}

## RUN PROGRAM
main()


