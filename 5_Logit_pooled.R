library(readr)
library(mice)
library(rpart)
library(rpart.plot)
library(ROSE)

#util
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# load the data
d.bird <- read_delim("C:/Users/joela/OneDrive - ETH Zurich/FS23/Snow-finches/Data/SF_ringlist.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

d.bird<- read.csv("C:/Users/samir/OneDrive/Desktop/mice/SF_ringlist.csv", sep=";")

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

# calculate the proportion of NAs
d_val_proportion <- 1 - colSums(is.na(df_adult_sub))/nrow(df_adult_sub)
d_val_proportion

# changing type of variables
df_adult_sub$season <- as.factor(df_adult_sub$season)
df_adult_sub$Fat <- as.factor(df_adult_sub$Fat)
df_adult_sub$Muscle <- as.factor(df_adult_sub$Muscle)
df_adult_sub$sex_genetics <- as.factor(df_adult_sub$sex_genetics)
df_adult_sub$Age<- as.numeric(df_adult_sub$Age)

### split data   

#param
imp <- mice(df_adult_sub, seed = 123, print = T, m = 35)
imp <- mice::complete(imp, "all")
n <- NROW(df_adult_sub)
K <- 10
folds <- sample(cut(seq(1,n),breaks=K,labels=FALSE), replace = FALSE)
Fold.error <- numeric(K)
for (i in 1:K) {
  test.ind <- which(folds == i)
  
  train_data <- df_adult_sub[-test.ind,]
  test_data <- df_adult_sub[test.ind,]
  
  # mice 
  imp_train = mice(train_data, seed = 123, print = T, m=35)
  
  # fit model 
  fit_1 <- with(imp_train, glm(sex_genetics ~ season + Age + Wing + P8 + Tarsus + weight + Fat + Muscle + Bill_length, family = binomial))
  pooled <- pool(fit_1)
  
  # hack for predict 
  pooled_lm = fit_1$analyses[[1]]
  pooled_lm$coefficients = summary(pooled)$estimate
  
  size = nrow(imp_train[[1]][test.ind,-(10)]) # remove sex
  
  # loop over imputed data 
  dat = matrix(nrow = size, ncol = 35)
  for (k in 1:35)
  {
    predicted_values = predict(pooled_lm, newdata = imp[[k]][test.ind,-(10)], type="response")
    binary_predictions <- ifelse(predicted_values > 0.5, 1, 0)
    dat[,k] = binary_predictions
  }
  
  #majority vote
  test_predictions.i <- apply(dat,1,Mode)
  
  Fold.error[i] <- mean(test_predictions.i == df_adult_sub[test.ind,]$sex_genetics)
  
}

(KFold.error <- mean(Fold.error))
