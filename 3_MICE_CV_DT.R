library(readr)
library(mice)
library(rpart)
library(rpart.plot)
library(ROSE)

# load the data
d.bird <- read_delim("C:/Users/joela/OneDrive - ETH Zurich/FS23/Snow-finches/Data/SF_ringlist.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Response variable transformation
d.bird$sex_genetics[d.bird$sex_genetics == "M"] <- "m"
d.bird$sex_genetics[d.bird$sex_genetics == "F"] <- "f"

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
str(df_adult_sub)
df_adult_sub$season <- as.numeric(df_adult_sub$season)
#df_adult_sub$Fat <- as.character(df_adult_sub$Fat)
#df_adult_sub$Muscle <- as.character(df_adult_sub$Muscle)

# Create initial imputation as starting point, create prediction matrix
M_init <- df_adult_sub[,colnames(df_adult_sub) != "sex_genetics"]
for(j in colnames(M_init)){
  if(class(M_init[,j]) == "numeric") M_init[is.na(M_init[,j]),j] <- mean(
    x = M_init[,j],
    na.rm = TRUE
  )
  if(class(M_init[,j]) == "character"){
    M_init[is.na(M_init[,j]),j] <- names(sort(
      x = table(M_init[,j]),
      decreasing = TRUE
    ))[1]
  }
}

mice_mean <- mice::mice(
  data = df_adult_sub[,colnames(df_adult_sub) != "sex_genetics"],
  method = "mean",
  m = 1,
  maxit = 1,
  visitSequence = order(d_val_proportion),
  data.init = M_init,
  seed = 823
)

# Impute missing values with mice and store it in a list
v_method <- c(
  "pmm","sample","cart","rf",
  "2l.bin",
  "mean","norm","norm.nob","norm.boot","norm.predict","ri","2l.lmer"
)

gc()
list_mice <- lapply(
  X = v_method,
  FUN = function(x) try({
    gc()
    mice::mice(
      data = df_adult_sub[,colnames(df_adult_sub) != "sex_genetics"],
      method = x,
      predictorMatrix = mice_mean$predictorMatrix,
      m = 5,
      maxit = 5,
      data.init = M_init,
      seed = 823
    )
  })
)
gc()
names(list_mice) <- v_method

# Extract complete data, append target variable
list_complete <- lapply(
  X = list_mice,
  FUN = mice::complete
)
for(j in names(list_complete)) list_complete[[j]] <- data.frame(
  sex_genetics = df_adult_sub$sex_genetics,
  list_complete[[j]]
)

# Evaluate imputation using decision tree and CV
n <- dim(df_adult_sub)[1]
K <- 10
folds <- sample(cut(seq(1,n),breaks=K,labels=FALSE), replace = FALSE)
KFold.error <- numeric(5)
Fold.error <- numeric(K)
for (j in 1:12){
  d <- list_complete[[j]]
  for (i in 1:K) {
    test.ind <- which(folds == i)
    tree_model.i <- rpart(sex_genetics ~ ., data = d[-test.ind,], method = "class")
    test_predictions.i <- predict(tree_model.i, d[test.ind,], type = "class")
    Fold.error[i] <- mean(test_predictions.i == d[test.ind,]$sex_genetics)
  }
  KFold.error[j] <- mean(Fold.error)
}
KFold.error
KFold.error <- as.data.frame(KFold.error)
rownames(KFold.error) <- v_method
KFold.error
