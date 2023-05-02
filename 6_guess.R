imp <- mice(df_adult_sub, seed = 123, print = T, m = 35)
imp <- mice::complete(imp, "all")
fit <- with(imp, glm(sex_genetics ~ season + Age
                     + Wing + P8 + Tarsus + weight + 
                       Muscle + Bill_length,
                     family = binomial))
pooled <- pool(fit)
pooled_lm = fit_1$analyses[[1]]
pooled_lm$coefficients = summary(pooled)$estimate

guess_index <- which((df_adult$Sex == 1 | df_adult$Sex == 2))

train_data <- df_adult_sub[-guess_index,]

# mice 
imp_train = mice(train_data, seed = 123, print = T, m=35)

# fit model 
fit_1 <- with(imp_train, glm(sex_genetics ~ season + Age
                             + Wing + P8 + Tarsus + weight + 
                               Muscle + Bill_length,
                             family = binomial))
pooled <- pool(fit_1)

# hack for predict 
pooled_lm = fit_1$analyses[[1]]
pooled_lm$coefficients = summary(pooled)$estimate

size = nrow(imp_train[[1]][guess_index,-(10)]) # remove sex

# loop over imputed data 
dat = matrix(nrow = size, ncol = 35)
for (k in 1:35) {
  predicted_values <- predict(pooled_lm, newdata = imp[[k]][guess_index,-(10)],
                             type="response")
  binary_predictions <- ifelse(predicted_values > 0.5, 1, 0)
  dat[,k] = binary_predictions
}

#majority vote
test_predictions <- apply(dat,1,Mode)

mean(test_predictions == df_adult_sub[guess_index,]$sex_genetics)



tree_model <- rpart(sex_genetics ~ ., data = df_adult_sub, method = "class")
test_predictions <- predict(tree_model, df_adult_sub[guess_index,], type = "class")
mean(test_predictions == df_adult_sub[guess_index,]$sex_genetics)
