#1:   PACKAGE INSTALLATION
#install.packages("dplyr")
#install.packages("mice")
#install.packages("stringi")



#2:   PACKAGE LOADING
library(dplyr)
library(mice)
library(readr)
library(tidyr)
library(stringi)



#3:   DATA IMPORTATION
full_d <- read_delim("C:/Users/conno/Downloads/SF_ringlist.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
full_d_NA_proportion <- colSums(is.na(full_d))/nrow(full_d)
barplot(full_d_NA_proportion, main = "Missingness over full dataset")



#4:   DATA CLEANING
d <- full_d[,c(3:5,13:29,33)]
colnames(d) <- c("capture_method", "sex_assumed", "age", 
                 "capture_location", "brood_ID", "x_coordinate", "y_coordinate", 
                 "wing_length", "wing_length_method", "P1", "P8", "tarsus", 
                 "tail_length", "weight", "fat_score", "muscle_score", 
                 "bill_length", "bill_width", "bill_height", "ringer", "sex")
d %>% 
  filter(age > 1)

d_NA_proportion <- colSums(is.na(d))/nrow(d)
d_NA_proportion
d_val_proportion <- 1 - d_NA_proportion
barplot(d_NA_proportion, main = "Missingness over reduced dataset")
barplot(d_val_proportion, main = "Completeness over reduced dataset")

d$sex[(d$sex) == "m"] <- "M"
d$sex[(d$sex) == "f"] <- "F"

factor_vector <- c(1, 2, 4, 5, 9, 20, 21)
d <- mutate(d, across(factor_vector, ~replace_na(.x, "NA")))
d[sapply(d, is.character)] <- lapply(d[sapply(d, is.character)], as.factor)
View(d)
colSums(is.na(d))/nrow(d)
levels(d$sex_assumed)



#5:   DATA SUMMARISATION
# Sex
#barplot(table(d$sex[which(d$age > 1)]), main = "Sex distribution")
# Wing Length
#hist(d$wing_length[which(d$age > 1)], main = "Wing length distribution", ylab = "Count", xlab = "wing length / mm")
#boxplot(d$wing_length[which(d$age > 1)], main = "Wing length distribution")
# Tarsus Length
#hist(d$tarsus[which(d$age > 1)], main = "Tarsus length distribution", ylab = "Count", xlab = "tarsus length / mm")
#boxplot(d$tarsus[which(d$age > 1)], main = "Tarsus length distribution")
# Weight
#hist(d$weight[which(d$age > 1)], main = "Weight distribution", ylab = "Count", xlab = "weight / g")
#boxplot(d$weight[which(d$age > 1)], main = "Weight distribution")
# Bill length
#hist(d$bill_length[which(d$age > 1)], main = "Bill length distribution", ylab = "Count", xlab = "bill length / mm")
#boxplot(d$bill_length[which(d$age > 1)], main = "Bill length distribution")
# Feather length
#hist(d$P8[which(d$age > 1)], main = "Feather length distribution", ylab = "Count", xlab = "feather length / mm")
#boxplot(d$P8[which(d$age > 1)], main = "Feather length distribution")
# Fat
#barplot(table(d$fat_score[which(d$age > 1)]), main = "Fat score distribution")
# Muscle
#barplot(table(d$muscle_score[which(d$age > 1)]), main = "Muscle score distribution")



#6:   MISSING VALUE IMPUTATION
predMatVec <- function(n, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12) {
  output <- c()
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        output <- append(output, 0)
      } else {
        if (j == f1 | j == f2 | j == f3 | j == f4 | j == f5 | j == f6 | j == f7 | j == f8 | j == f9 | j == f10 | j == f11 | j == f12) {
          output <- append(output, 0)
        } else {
          output <- append(output, 1)
        }
      }
    }
  }
  return(output)
}
predMatVec(ncol(d), 2, 5, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19)
predMat <- matrix(predMatVec(ncol(d), 2, 5, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19), nrow = ncol(d), ncol = ncol(d))

method_vector <- c("", "", "norm", "", "", 
                   "norm", "norm", "norm", "", "norm", 
                   "norm", "norm", "norm", "rf", "norm", 
                   "norm", "rf", "norm", "norm", "", 
                   "")

mice_d <- mice(d, m = 5, method_vector, predictorMatrix = predMat, visitSequence = order(d_val_proportion), maxit = 10, ridge = TRUE)

View(complete(mice_d))
colnames(d[,order(d_val_proportion)])