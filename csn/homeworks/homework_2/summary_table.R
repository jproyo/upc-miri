write_summary <- function(language,file) {
   degree_sequence = read.table(file, header = FALSE)
   cat(language,length(degree_sequence$V1),max(degree_sequence$V1),sum(degree_sequence$V1)/length(degree_sequence$V1),length(degree_sequence$V1)/sum(degree_sequence$V1),"\n")
}

source = read.table("list.txt", 
         header = TRUE,               # this is to indicate the first line of the file contains the names of the columns instead of the real data
         as.is = c("language","file") # this is need to have the cells treated as real strings and not as categorial data.
        )
for (x in 1:nrow(source)) {
    write_summary(source$language[x], source$file[x])
}

