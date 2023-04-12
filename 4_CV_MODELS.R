library(mice)
library(rpart)
library(randomForest)
library(caret)

# load the data
d.bird <- read_delim("C:/Users/joela/OneDrive - ETH Zurich/FS23/Snow-finches/Data/SF_ringlist.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Response variable transformation
d.bird$sex_genetics[d.bird$sex_genetics == "M"] <- "m"
d.bird$sex_genetics[d.bird$sex_genetics == "F"] <- "f"
d.bird$sex_genetics[d.bird$sex_genetics == "m"] <- 0
d.bird$sex_genetics[d.bird$sex_genetics == "f"] <- 1

# make new variable season for time of the catch
d.bird$season <- ifelse(d.bird$month >= 3 & d.bird$month <= 5, "spring",
                        ifelse(d.bird$month >= 6 & d.bird$month <= 8, "summer",
                               ifelse(d.bird$month >= 9 & d.bird$month <= 11, "autumn", "winter")))
d.bird$season <- factor(d.bird$season, levels = c("winter", "spring", "summer", "autumn"), labels = c("0", "1", "2", "3"))

# Remove duplicates 
d.bird <- d.bird[!duplicated(d.bird, fromLast = TRUE), ]

# Take values from P1 if possible to fill P8 (total of 32 cases)
if (sum(!is.na(d.bird$P1)) > 0) {
  d.bird$P8[is.na(d.bird$P8)] <- d.bird$P1[is.na(d.bird$P8)]
} else {
  print("NO impuatation possible as P1 is NA")
}

# check multiple caputres  
freq_table <- table(d.bird$ringnr)
freq_table <- freq_table[freq_table > 1]
freq_table <- sort(freq_table, decreasing = T)
double_caputre = as.data.frame(freq_table)
d.bird <- d.bird[!duplicated(d.bird$ringnr, fromLast = TRUE), ]

# select only the adult birds
df_adult <- d.bird[d.bird$Age>1,]

# select only rows with no NA in the response
df_adult <- df_adult[complete.cases(df_adult["sex_genetics"]),]

# select only the needed columns
cols <- c("season", "Age", "Wing", "P8", "Tarsus", "weight", "Fat", "Muscle", "Bill_length", "sex_genetics")
df_adult_sub = data.frame(df_adult[cols])

# chaning type of variables
str(df_adult_sub)
df_adult_sub$sex_genetics <- as.numeric(df_adult_sub$sex_genetics)
df_adult_sub$Fat <- as.factor(df_adult_sub$Fat)
df_adult_sub$Muscle <- as.factor(df_adult_sub$Muscle)

# Impute missing value with MICE using the best method from cv imputation with 5 iterations
data_imputed <- mice::complete(mice(df_adult_sub, m = 5, method = "norm.predict"))

n <- dim(data_imputed)[1]
KFold.error <- numeric(5)
# logit model
K <- 20
folds <- sample(cut(seq(1,n),breaks=K,labels=FALSE), replace = FALSE)
Fold.error <- numeric(K)
for (i in 1:K) {
  test.ind <- which(folds == i)
  glm.i <- glm(sex_genetics ~ ., family = binomial(link='logit'), data = data_imputed[-test.ind,])
  test_predictions.i <- predict(glm.i, data_imputed[test.ind,])
  Fold.error[i] <- mean(test_predictions.i == data_imputed[test.ind,]$sex_genetics)
}
Fold.error

# decision tree
Fold.error <- numeric(K)
for (i in 1:K) {
  test.ind <- which(folds == i)
  tree_model.i <- rpart(sex_genetics ~ ., data = data_imputed[-test.ind,], method = "class")
  test_predictions.i <- predict(tree_model.i, data_imputed[test.ind,], type = "class")
  Fold.error[i] <- mean(test_predictions.i == data_imputed[test.ind,]$sex_genetics)
}
KFold.error[2] <- mean(Fold.error)

#random forest
Fold.error <- numeric(K)
for (i in 1:K) {
  test.ind <- which(folds == i)
  rf.i <- randomForest(sex_genetics ~ ., data = data_imputed[-test.ind,], ntree = 500, importance = TRUE)
  test_predictions.i <- predict(rf.i, newdata = data_imputed[test.ind,])
  Fold.error[i] <- mean(test_predictions.i == data_imputed[test.ind,]$sex_genetics)
}
KFold.error[3] <- mean(Fold.error)
