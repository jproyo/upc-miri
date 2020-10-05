######################################################
## Authors: Juan Pablo Royo Sales & Francesc Roy
## Title: Homework 2
## Dae: 2020-10-05
####################################################

write_summary <- function(language,file) {
  degree_sequence = read.table(file, header = FALSE)
#  barplot(degree_sequence$V1, main = language,
#          xlab = "degree", ylab = "number of vertices", log = "xy")
  c(language,length(degree_sequence$V1),max(degree_sequence$V1),sum(degree_sequence$V1)/length(degree_sequence$V1),length(degree_sequence$V1)/sum(degree_sequence$V1))
}

source = read.table("list_in.txt", 
                    header = TRUE,               # this is to indicate the first line of the file contains the names of the columns instead of the real data
                    as.is = c("language","file") # this is need to have the cells treated as real strings and not as categorial data.
)
result <- NULL
for (x in 1:nrow(source)) {
  result <- rbind(result, write_summary(source$language[x], source$file[x]))
}

df <- data.frame(result)
colnames(df) <- c("Language","N", "Maximum Degree", "M/N","N/M")
print.data.frame(df, right=FALSE, row.names=FALSE)

