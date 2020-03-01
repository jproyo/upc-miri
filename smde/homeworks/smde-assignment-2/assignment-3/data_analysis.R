simulation <- read.table("/Users/juan/Projects/upc/master/smde/smde-assignment-2/simulation_2.csv",
  header = FALSE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)

real_data <- read.table("/Users/juan/Projects/upc/master/smde/smde-assignment-2/real_data_2.csv",
  header = FALSE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)



sim_to_analyze = data.frame(x1 = simulation, x2 = "v1")

real_to_analytze = data.frame(x1 = real_data, x2 = "v2")

data = mergeRows(sim_to_analyze, real_to_analytze, common.only = FALSE)

AnovaModel.1 <- aov(V1 ~ x2, data = data)
summary(AnovaModel.1)
Boxplot(V1 ~ x2, data = data, id.method = "y")
