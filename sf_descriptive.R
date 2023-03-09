library(readr)
library(stringr)
require(grDevices)
library(dplyr)

# data load
d.bird <- read_delim("C:/Users/joela/OneDrive - ETH Zurich/FS23/Snow-finches/Data/SF_ringlist.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
d.bird <- read_delim("~/GitHub/data_sf_ringlist/SF_ringlist.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)


str(d.bird)
summary(d.bird)

# Response variable transformation
d.bird$sex_genetics[d.bird$sex_genetics == "M"] <- "m"
d.bird$sex_genetics[d.bird$sex_genetics == "F"] <- "f"

table(d.bird$sex_genetics)
table(d.bird$sex_genetics[which(d.bird$Age > 1)])
barplot(table(d.bird$sex_genetics[which(d.bird$Age > 1)]))
2267-sum(table(d.bird$sex_genetics[which(d.bird$Age > 1)]))


# Wing Length
hist(d.bird$Wing[which(d.bird$Age > 1)], main = "Wing Length in mm", ylab = "#birds", xlab = "length [mm]")
boxplot(d.bird$Wing[which(d.bird$Age > 1)])
# Leg Length
hist(d.bird$Tarsus[which(d.bird$Age > 1)], main = "leg length", ylab = "#birds", xlab = "leg length")
boxplot(d.bird$Tarsus[which(d.bird$Age > 1)])
# Weight
hist(d.bird$weight[which(d.bird$Age > 1)], main = "Weight in g", ylab = "#birds", xlab = "leg length")
boxplot(d.bird$weight[which(d.bird$Age > 1)])
# Bill length
hist(d.bird$Bill_length[which(d.bird$Age > 1)], main = "Bill length", ylab = "#birds", xlab = "leg length")
boxplot(d.bird$Bill_length[which(d.bird$Age > 1)])
# Feather length
hist(d.bird$P8[which(d.bird$Age > 1)], main = "feather length", ylab = "#birds", xlab = "leg length")
boxplot(d.bird$P8[which(d.bird$Age > 1)])

# Fat
barplot(table(d.bird$Fat[which(d.bird$Age > 1)]))
# Muscle
barplot(table(d.bird$Muscle[which(d.bird$Age > 1)]))

# plot
par(mfrow = c(2, 4))
hist(d.bird$Wing[which(d.bird$Age > 1)], main = "wing Length", ylab = "#birds", xlab = "length")
hist(d.bird$Tarsus[which(d.bird$Age > 1)], main = "leg length", ylab = "#birds", xlab = "length")
hist(d.bird$weight[which(d.bird$Age > 1)], main = "beight [in g]", ylab = "#birds", xlab = "length")
hist(d.bird$Bill_length[which(d.bird$Age > 1)], main = "bill length", ylab = "#birds", xlab = "length")
hist(d.bird$P8[which(d.bird$Age > 1)], main = "feather length", ylab = "#birds", xlab = "length")
#par(mfrow = c(2, 2))
barplot(table(d.bird$Fat[which(d.bird$Age > 1)]), main="fat")
barplot(table(d.bird$Muscle[which(d.bird$Age > 1)]), main="muscle")
barplot(table(d.bird$sex_genetics[which(d.bird$Age > 1)]), main="sex")


# adult data 
df_adult =d.bird[d.bird$Age>1,]
cols = c("Age","Wing","Tarsus","Bill_length","P8","weight","Fat","Muscle","sex_genetics")
df_adult_sub = data.frame(df_adult[cols])

# nan 
df_adult_sub %>% 
  summarise_all(~sum(is.na(.)))/NROW(df_adult)*100





