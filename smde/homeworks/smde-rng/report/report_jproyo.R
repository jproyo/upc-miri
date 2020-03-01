
fibflagged <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/500_random_numbers.csv", 
    header = FALSE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)

fibflagged$bins <- with(fibflagged, binVariable(V1, bins = 10, method = "intervals", 
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))

fibflagged_trans <- as.data.frame(with(fibflagged, table(bins)))

distribution_r <- as.data.frame(matrix(runif(500 * 1, min = 0, max = 1), ncol = 1))
rownames(distribution_r) <- paste("sample", 1:500, sep = "")
colnames(distribution_r) <- "obs"
distribution_r <- within(distribution_r, {
    mean <- rowMeans(distribution_r[, 1:1])
})

distribution_r$bins <- with(distribution_r, binVariable(obs, bins = 10, method = "intervals", 
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))


distribution_r_trans <- as.data.frame(with(distribution_r, table(frequency)))

merge_fibflagged_r_comb <- merge(fibflagged_trans, distribution_r_trans, all = TRUE, 
    by = "row.names")
rownames(merge_fibflagged_r_comb) <- merge_fibflagged_r_comb$Row.names
merge_fibflagged_r_comb$Row.names <- NULL



to_be_test_chi_r_fibflagged <- within(merge_fibflagged_r_comb, {
	bins <- NULL
	frequency <- NULL
})

test <- chisq.test(to_be_test_chi_r_fibflagged, correct = FALSE)

fibflagged$morebins <- with(fibflagged, binVariable(V1, bins = 20, method = "intervals", 
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")))

distribution_r$morebins <- with(distribution_r, binVariable(obs, bins = 20, method = "intervals", 
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")))

fibflagged_trans_20bins <- as.data.frame(with(fibflagged, table(morebins)))

distribution_r_trans_20bins <- as.data.frame(with(distribution_r, table(morebins)))


merge_fibflagged_r_comb_20bins <- merge(fibflagged_trans_20bins, distribution_r_trans_20bins, all = TRUE, 
    by = "row.names")
rownames(merge_fibflagged_r_comb_20bins) <- merge_fibflagged_r_comb_20bins$Row.names
merge_fibflagged_r_comb_20bins$Row.names <- NULL



test_chi_r_fibflagged_20bins <- within(merge_fibflagged_r_comb_20bins, {
	morebins.x <- NULL
	morebins.y <- NULL
})


chi_20bins <- chisq.test(test_chi_r_fibflagged_20bins, correct = FALSE)



Norm_m0_s1 <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/1500_normal_mu_0_sigma_1.csv", 
    header = FALSE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)

Norm_m10_s1 <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/1500_normal_mu_10_sigma_1.csv", 
    header = FALSE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)

Norm_m0_s1_2 <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/1500_normal_mu_0_sigma_1_2.csv", 
    header = FALSE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)


Norm_v1n=data.frame(x1=Norm_m0_s1, x2="v1")
Norm_v2n=data.frame(x1=Norm_m10_s1, x2="v2")
Norm_v3n=data.frame(x1=Norm_m0_s1_2, x2="v3")
data=mergeRows(Norm_v1n, Norm_v2n, common.only=FALSE)
data=mergeRows(as.data.frame(data), Norm_v3n, common.only=FALSE)

AnovaModel.1 <- aov(V1 ~ x2, data=data)
summary(AnovaModel.1)
Boxplot(V1~x2, data=data, id.method="y")
	

library("lmtest", lib.loc="~/R/win-library/3.0")

#The observations within each sample must be independent.
#Durbin Watson 
library("lmtest", lib.loc="~/R/win-library/3.0")
dwtest(AnovaModel.1, alternative ="two.sided")
#The populations from which the samples are selected must be normal.
#Shapiro test
shapiro.test(residuals(AnovaModel.1))
#The populations from which the samples are selected must have equal variances (homogeneity of variance)
#Breusch Pagan test
lmtest::bptest(AnovaModel.1)


wine_data <- data(wine, package = "FactoMineR")

wine_data_soil_odori <- wine[,c("Odor.Intensity.before.shaking", "Soil")]
colnames(wine_data_soil_odori) <- c("odori", "soil")
AnovaModel_soil_odori <- aov(odori ~ soil, data=wine_data_soil_odori)
summary(AnovaModel_soil_odori)


wine_data_soil_aromaq <- wine[,c("Aroma.quality.before.shaking", "Soil")]
colnames(wine_data_soil_aromaq) <- c("aromaq", "soil")
AnovaModel_soil_aromaq <- aov(aromaq ~ soil, data=wine_data_soil_aromaq)
summary(AnovaModel_soil_aromaq)


wine_data_label_odori <- wine[,c("Odor.Intensity.before.shaking", "Label")]
colnames(wine_data_label_odori) <- c("odori", "label")
AnovaModel_label_odori <- aov(odori ~ label, data=wine_data_label_odori)
summary(AnovaModel_label_odori)


wine_data_label_aromaq <- wine[,c("Aroma.quality.before.shaking", "Label")]
colnames(wine_data_label_aromaq) <- c("aromaq", "label")
AnovaModel_label_aromaq <- aov(aromaq ~ label, data=wine_data_label_aromaq)
summary(AnovaModel_label_aromaq)


data(decathlon, package = "FactoMineR")
names(decathlon) <- make.names(names(decathlon))

if(!require("caTools")){
    install.packages("caTools")
    library(caTools)
}
split = sample.split(decathlon, SplitRatio = 0.6)
training_decathlon_set = subset(decathlon, split == TRUE)
test_decathlon_set = subset(decathlon, split == FALSE)


RegModel.17 <- lm(X1500m ~ Discus + High.jump + Javeline + Long.jump + Points + Pole.vault + 
    Rank + Shot.put + X100m + X110m.hurdle + X400m, data = training_decathlon_set)
summary(RegModel.17)

RegModel.18 <- lm(X1500m ~ Discus + High.jump + Javeline + Long.jump + Points + Pole.vault + 
    Shot.put + X100m + X110m.hurdle + X400m, data = training_decathlon_set)
summary(RegModel.18)

RegModel.19 <- lm(X1500m ~ Discus + High.jump + Javeline + Long.jump + Pole.vault + 
    Shot.put + X100m + X110m.hurdle + X400m, data = training_decathlon_set)
summary(RegModel.19)

RegModel.21 <- lm(X1500m ~ Discus + High.jump + Long.jump + Points + Pole.vault + 
    Shot.put + X100m + X110m.hurdle + X400m, data = training_decathlon_set)
summary(RegModel.21)


RegModel.23 <- lm(X1500m ~ Discus + High.jump + Javeline + Long.jump + Points + Pole.vault + 
    Shot.put + X100m + X110m.hurdle + X400m, data = training_decathlon_set)
summary(RegModel.23)

library("lmtest", lib.loc="~/R/win-library/3.0")
dwtest(RegModel.23, alternative="two.sided")

shapiro.test(residuals(RegModel.23))

bptest(RegModel.23)

prediction <- predict(RegModel.23, newdata=test_decathlon_set, interval="prediction")

test_only_with_x1500m <- subset(test_decathlon_set, select=c("X1500m"))

predicted_data <- data.frame(prediction)
predicted_data["real_data_x1500m"] = test_only_with_x1500m["X1500m"]

decathlon.PCA <- decathlon[, c("X100m", "Long.jump", "Shot.put", "High.jump", "X400m", 
    "X110m.hurdle", "Discus", "Pole.vault", "Javeline", "X1500m", "Rank", "Points")]
res <- PCA(decathlon.PCA, scale.unit = TRUE, ncp = 5, graph = FALSE)
plot.PCA(res, axes = c(1, 2), choix = "ind", habillage = "none", col.ind = "black", 
    col.ind.sup = "blue", col.quali = "magenta", label = c("ind", "ind.sup", "quali"), 
    new.plot = TRUE)
plot.PCA(res, axes = c(1, 2), choix = "var", new.plot = TRUE, col.var = "black", 
    col.quanti.sup = "blue", label = c("var", "quanti.sup"), lim.cos2.var = 0)
summary(res, nb.dec = 3, nbelements = 10, nbind = 10, ncp = 3, file = "")
remove(decathlon.PCA)


boston_mar_men_30_40_training <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/boston_marathon_men_30_40_training.csv", 
    header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE, quote = "\"")
boston_mar_men_18_30 <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/boston_marathon_men_18_30.csv", 
    header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE, quote = "\"")
boston_mar_men_40_50 <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/boston_marathon_men_40_50.csv", 
    header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE, quote = "\"")

boston_mar_men_30_40_official_time = boston_mar_men_30_40_training[,c("Official.Time")]
boston_mar_men_18_30_official_time = boston_mar_men_18_30[,c("Official.Time")]
boston_mar_men_40_50_official_time = boston_mar_men_40_50[,c("Official.Time")]

Norm_boston_mar_30_40=data.frame(x1=boston_mar_men_30_40_official_time, x2="men_30_40")
Norm_boston_mar_18_30=data.frame(x1=boston_mar_men_18_30_official_time, x2="men_18_30")
Norm_boston_mar_40_50=data.frame(x1=boston_mar_men_40_50_official_time, x2="men_40_50")
boston_mar_merge=mergeRows(Norm_boston_mar_30_40, Norm_boston_mar_18_30, common.only=FALSE)
boston_mar_merge=mergeRows(as.data.frame(boston_mar_merge), Norm_boston_mar_40_50, common.only=FALSE)

boston_mar_merge$x1 = as.difftime(as.character(boston_mar_merge$x1))
boston_mar_merge$x1 = as.numeric(boston_mar_merge$x1, units = "secs")

AnovaModel.Boston.Marathon.Merge <- aov(x1 ~ x2, data=boston_mar_merge)
summary(AnovaModel.Boston.Marathon.Merge)


library("lmtest", lib.loc="~/R/win-library/3.0")

#The observations within each sample must be independent.
#Durbin Watson 
library("lmtest", lib.loc="~/R/win-library/3.0")
dwtest(AnovaModel.Boston.Marathon.Merge, alternative ="two.sided")
#The populations from which the samples are selected must be normal.
#Shapiro test
shapiro.test(residuals(AnovaModel.Boston.Marathon.Merge))
#The populations from which the samples are selected must have equal variances (homogeneity of variance)
#Breusch Pagan test
lmtest::bptest(AnovaModel.Boston.Marathon.Merge)


boston_mar_men_30_40_training$X5K = as.difftime(as.character(boston_mar_men_30_40_training$X5K))
boston_mar_men_30_40_training$X5K = as.numeric(boston_mar_men_30_40_training$X5K, units = "secs")

boston_mar_men_30_40_training$X10K = as.difftime(as.character(boston_mar_men_30_40_training$X10K))
boston_mar_men_30_40_training$X10K = as.numeric(boston_mar_men_30_40_training$X10K, units = "secs")

boston_mar_men_30_40_training$X15K = as.difftime(as.character(boston_mar_men_30_40_training$X15K))
boston_mar_men_30_40_training$X15K = as.numeric(boston_mar_men_30_40_training$X15K, units = "secs")

boston_mar_men_30_40_training$X20K = as.difftime(as.character(boston_mar_men_30_40_training$X20K))
boston_mar_men_30_40_training$X20K = as.numeric(boston_mar_men_30_40_training$X20K, units = "secs")

boston_mar_men_30_40_training$Half = as.difftime(as.character(boston_mar_men_30_40_training$Half))
boston_mar_men_30_40_training$Half = as.numeric(boston_mar_men_30_40_training$Half, units = "secs")

boston_mar_men_30_40_training$X25K = as.difftime(as.character(boston_mar_men_30_40_training$X25K))
boston_mar_men_30_40_training$X25K = as.numeric(boston_mar_men_30_40_training$X25K, units = "secs")

boston_mar_men_30_40_training$X30K = as.difftime(as.character(boston_mar_men_30_40_training$X30K))
boston_mar_men_30_40_training$X30K = as.numeric(boston_mar_men_30_40_training$X30K, units = "secs")

boston_mar_men_30_40_training$X35K = as.difftime(as.character(boston_mar_men_30_40_training$X35K))
boston_mar_men_30_40_training$X35K = as.numeric(boston_mar_men_30_40_training$X35K, units = "secs")

boston_mar_men_30_40_training$X40K = as.difftime(as.character(boston_mar_men_30_40_training$X40K))
boston_mar_men_30_40_training$X40K = as.numeric(boston_mar_men_30_40_training$X40K, units = "secs")

boston_mar_men_30_40_training$Pace = as.difftime(as.character(boston_mar_men_30_40_training$Pace))
boston_mar_men_30_40_training$Pace = as.numeric(boston_mar_men_30_40_training$Pace, units = "secs")

boston_mar_men_30_40_training$Official.Time = as.difftime(as.character(boston_mar_men_30_40_training$Official.Time))
boston_mar_men_30_40_training$Official.Time = as.numeric(boston_mar_men_30_40_training$Official.Time, units = "secs")

RegModel.boston_mar_men_30_40.1 <- lm(Official.Time ~ Age + Bib + Division + Gender + Half + Overall + 
    Pace + X + X5K + X10K + X15K + X20K + X25K + X30K + X35K + X40K, data = boston_mar_men_30_40_training)
summary(RegModel.boston_mar_men_30_40.1)

RegModel.boston_mar_men_30_40.2 <- lm(Official.Time ~ X30K + X35K + X40K, data = boston_mar_men_30_40_training)
summary(RegModel.boston_mar_men_30_40.2)

RegModel.boston_mar_men_30_40.3 <- lm(Official.Time ~ X35K + X40K, data = boston_mar_men_30_40_training)
summary(RegModel.boston_mar_men_30_40.3)

library("lmtest", lib.loc="~/R/win-library/3.0")
dwtest(RegModel.boston_mar_men_30_40.3, alternative="two.sided")

shapiro.test(residuals(RegModel.boston_mar_men_30_40.3))

bptest(RegModel.boston_mar_men_30_40.3)


boston_mar_men_30_40_test <- read.table("/Users/juan/Projects/upc/master/smde/smde-rng/boston_marathon_men_30_40_test.csv", 
    header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE, quote = "\"")

boston_mar_men_30_40_test$X5K = as.difftime(as.character(boston_mar_men_30_40_test$X5K))
boston_mar_men_30_40_test$X5K = as.numeric(boston_mar_men_30_40_test$X5K, units = "secs")

boston_mar_men_30_40_test$X10K = as.difftime(as.character(boston_mar_men_30_40_test$X10K))
boston_mar_men_30_40_test$X10K = as.numeric(boston_mar_men_30_40_test$X10K, units = "secs")

boston_mar_men_30_40_test$X15K = as.difftime(as.character(boston_mar_men_30_40_test$X15K))
boston_mar_men_30_40_test$X15K = as.numeric(boston_mar_men_30_40_test$X15K, units = "secs")

boston_mar_men_30_40_test$X20K = as.difftime(as.character(boston_mar_men_30_40_test$X20K))
boston_mar_men_30_40_test$X20K = as.numeric(boston_mar_men_30_40_test$X20K, units = "secs")

boston_mar_men_30_40_test$Half = as.difftime(as.character(boston_mar_men_30_40_test$Half))
boston_mar_men_30_40_test$Half = as.numeric(boston_mar_men_30_40_test$Half, units = "secs")

boston_mar_men_30_40_test$X25K = as.difftime(as.character(boston_mar_men_30_40_test$X25K))
boston_mar_men_30_40_test$X25K = as.numeric(boston_mar_men_30_40_test$X25K, units = "secs")

boston_mar_men_30_40_test$X30K = as.difftime(as.character(boston_mar_men_30_40_test$X30K))
boston_mar_men_30_40_test$X30K = as.numeric(boston_mar_men_30_40_test$X30K, units = "secs")

boston_mar_men_30_40_test$X35K = as.difftime(as.character(boston_mar_men_30_40_test$X35K))
boston_mar_men_30_40_test$X35K = as.numeric(boston_mar_men_30_40_test$X35K, units = "secs")

boston_mar_men_30_40_test$X40K = as.difftime(as.character(boston_mar_men_30_40_test$X40K))
boston_mar_men_30_40_test$X40K = as.numeric(boston_mar_men_30_40_test$X40K, units = "secs")

boston_mar_men_30_40_test$Pace = as.difftime(as.character(boston_mar_men_30_40_test$Pace))
boston_mar_men_30_40_test$Pace = as.numeric(boston_mar_men_30_40_test$Pace, units = "secs")

boston_mar_men_30_40_test$Official.Time = as.difftime(as.character(boston_mar_men_30_40_test$Official.Time))
boston_mar_men_30_40_test$Official.Time = as.numeric(boston_mar_men_30_40_test$Official.Time, units = "secs")

prediction_boston_mar_men_30_40 <- predict(RegModel.boston_mar_men_30_40.3, newdata=boston_mar_men_30_40_test, interval="prediction")

prediction_boston_mar_men_30_40_data <- data.frame(prediction_boston_mar_men_30_40)

boston_mar_30_40_test_only_with_official_time <- subset(boston_mar_men_30_40_test, select=c("Official.Time"))

prediction_boston_mar_men_30_40_data["real_data_official_time"] = boston_mar_30_40_test_only_with_official_time["Official.Time"]


boston_mar_men_30_40_training.PCA <- boston_mar_men_30_40_training[, c("X", "Bib", 
    "Age", "X5K", "X10K", "X15K", "X20K", "Half", "X25K", "X30K", "X35K", "X40K", 
    "Pace", "Official.Time", "Overall", "Gender", "Division")]
res <- PCA(boston_mar_men_30_40_training.PCA, scale.unit = TRUE, ncp = 5, graph = FALSE)
plot.PCA(res, axes = c(1, 2), choix = "ind", habillage = "none", col.ind = "black", 
    col.ind.sup = "blue", col.quali = "magenta", label = c("ind", "ind.sup", "quali"), 
    new.plot = TRUE)
plot.PCA(res, axes = c(1, 2), choix = "var", new.plot = TRUE, col.var = "black", 
    col.quanti.sup = "blue", label = c("var", "quanti.sup"), lim.cos2.var = 0)
summary(res, nb.dec = 3, nbelements = 10, nbind = 10, ncp = 3, file = "")


RegModel.boston_mar_men_30_40.4 <- lm(Official.Time ~ X30K + X35K, data = boston_mar_men_30_40_training)
summary(RegModel.boston_mar_men_30_40.4)

library("lmtest", lib.loc="~/R/win-library/3.0")
dwtest(RegModel.boston_mar_men_30_40.4, alternative="two.sided")

shapiro.test(residuals(RegModel.boston_mar_men_30_40.4))

bptest(RegModel.boston_mar_men_30_40.4)

prediction_boston_mar_men_30_40.2 <- predict(RegModel.boston_mar_men_30_40.4, newdata=boston_mar_men_30_40_test, interval="prediction")

prediction_boston_mar_men_30_40.2_data <- data.frame(prediction_boston_mar_men_30_40.2)

boston_mar_30_40_test_only_with_official_time.2 <- subset(boston_mar_men_30_40_test, select=c("Official.Time"))

prediction_boston_mar_men_30_40.2_data["real_data_official_time"] = boston_mar_30_40_test_only_with_official_time.2["Official.Time"]

