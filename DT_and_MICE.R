library(readr)
library(rpart)
library(rpart.plot)
library(mice)


###### TO DO: #######
## 1. Combine the P1 and P8, are there the same?
## 2. Look for double observation via ringnr and delete the double cases
## 3. Observe more the data, outliers, etc.


# load the data
d.bird <- read_delim("C:/Users/joela/OneDrive - ETH Zurich/FS23/Snow-finches/Data/SF_ringlist.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(d.bird)

# Response variable transformation
d.bird$sex_genetics[d.bird$sex_genetics == "M"] <- "m"
d.bird$sex_genetics[d.bird$sex_genetics == "F"] <- "f"

# select only the adult birds
df_adult <- d.bird[d.bird$Age>1,]

# select only rows with no NA in the response
df_adult <- df_adult[complete.cases(df_adult["sex_genetics"]),]

# select only the needed columns
cols <- c("Wing", "P8", "Tarsus", "weight", "Fat", "Muscle", "Bill_length", "sex_genetics")
df_adult_sub = data.frame(df_adult[cols])

# chaning type of variables
str(df_adult_sub)
df_adult_sub$Fat <- as.factor(df_adult_sub$Fat)
df_adult_sub$Muscle <- as.factor(df_adult_sub$Muscle)

# Impute missing value with MICE using random forest imputation with 5 iterations
data_imputed <- mice(df_adult_sub, m = 5, method = "rf")$data

# Create training and testing datasets
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(data_imputed), round(0.8*nrow(data_imputed)), replace = FALSE)
train_data <- data_imputed[train_index,]
test_data <- data_imputed[-train_index,]

# Train a decision tree on the training data
tree_model <- rpart(formula = sex_genetics ~ ., data = train_data, method = "class")

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
