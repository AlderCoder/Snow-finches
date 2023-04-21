library(readr)
library(rpart)
library(rpart.plot)

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

# chaning type of variables
str(df_adult_sub)
df_adult_sub$Fat <- as.factor(df_adult_sub$Fat)
df_adult_sub$Muscle <- as.factor(df_adult_sub$Muscle)

# Create training and testing datasets
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(df_adult_sub), round(0.8*nrow(df_adult_sub)), replace = FALSE)
train_data <- df_adult_sub[train_index,]
test_data <- df_adult_sub[-train_index,]

# Train a decision tree on the training data
tree_model <- rpart(formula = sex_genetics ~ ., data = train_data, method = "class",
                    parms = list(loss = matrix(c(0,1,2,0), nrow = 2)))

# Plot the decision tree
rpart.plot(tree_model)

# Make predictions on the test data using the trained decision tree
test_predictions <- predict(tree_model, test_data, type = "class")

# Evaluate the accuracy of the model on the test data
accuracy <- mean(test_predictions == test_data$sex_genetics)
cat("Accuracy:", accuracy)

# Compute the confusion matrix
confusion_matrix <- table(test_predictions, test_data$sex_genetics)
print(confusion_matrix)

