library(readr)
library(stringr)
require(grDevices)
library(dplyr)
library(data.table)
library(ggplot2)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

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
barplot(table(d.bird$Fat[which(d.bird$Age > 1)]), main="fat")
barplot(table(d.bird$Muscle[which(d.bird$Age > 1)]), main="muscle")
barplot(table(d.bird$sex_genetics[which(d.bird$Age > 1)]), main="sex")


### ###  ###  analysis of adult data 
df_adult = d.bird[d.bird$Age>1,]
cols = c("Age","Wing","Tarsus","Bill_length","P8","weight","Fat","Muscle","sex_genetics")
df_adult_sub = data.frame(df_adult[cols])

### nan count
df_adult_sub %>% 
  summarise_all(~sum(is.na(.)))/NROW(df_adult)*100
df_adult_sub

### geo info
# data prep
cols = c("Age","Wing","Tarsus","Bill_length","P8","weight","Fat","Muscle","sex_genetics","location")
bird_dt <- as.data.table(df_adult[cols])

NROW(bird_dt[, .N, by=.(location)][ N > 0 ])

(locations  <- bird_dt[, .N, by=.(location)][ N > 50 ])
relevant_locations_dt = setDT(bird_dt, key = 'location')[J(c(locations[,location]))]  

summary(relevant_locations_dt)
sum(is.na(relevant_locations_dt$sex_genetics)==T)

data_plot = relevant_locations_dt[!is.na(relevant_locations_dt$sex_genetics)]

x = as.data.frame(data_plot)
df =copy(x)
df$sex_genetics[df$sex_genetics == "m"] <- "Male"
df$sex_genetics[df$sex_genetics == "f"] <- "Female"

df
# plots by hand
crosstab(df, row.vars = "Age", col.vars = "sex_genetics", type = "f")
crosstab(df, row.vars = "Age", col.vars = c("sex_genetics","location"), type = "r")
crosstab(df, row.vars = c("location", "sex_genetics"), col.vars = "Age", type = "j")

wing= ggplot(df, aes(x=location, y=Wing, fill=sex_genetics)) + 
  geom_boxplot(notch=T) +
  ggtitle("Wing length") +
  theme_bw() +
  facet_wrap(~sex_genetics)
Tarsus = ggplot(df, aes(x=location, y=Tarsus, fill=sex_genetics)) + 
  geom_boxplot(notch=T) +
  ggtitle("Tarsus") +
  theme_bw() +
  facet_wrap(~sex_genetics)
Bill_length = ggplot(df, aes(x=location, y=Bill_length, fill=sex_genetics)) + 
  geom_boxplot(notch=T) +
  ggtitle("Bill length") +
  theme_bw() +
  facet_wrap(~sex_genetics)
P8 = ggplot(df, aes(x=location, y=P8, fill=sex_genetics)) + 
  geom_boxplot(notch=T) +
  ggtitle("feather length") +
  theme_bw() +
  facet_wrap(~sex_genetics)
weight = ggplot(df, aes(x=location, y=weight, fill=sex_genetics)) + 
  geom_boxplot(notch=T) +
  ggtitle("weight") +
  theme_bw() +
  facet_wrap(~sex_genetics)


### ###  ###  sex guess
unique(df_guess$Sex)

df_adult$Sex[df_adult$Sex == "1"] <- "m"
df_adult$Sex[df_adult$Sex == "2"] <- "f"
df_guess = df_adult[!is.na(df_adult$sex_genetics),]
df_guess = df_guess[!is.na(df_guess$Sex),]
df_guess = df_guess[df_guess$Sex != "0",]
df_guess = df_guess[df_guess$Sex != "2?",]
df_guess = df_guess[df_guess$Sex != "1?",]

(n = NROW(df_guess))
correct = df_guess$Sex == df_guess$sex_genetics
wrong = df_guess$Sex != df_guess$sex_genetics

sum(correct)/n


### ###  ###  analysis of nestling data 
par(mfrow = c(2, 4))
hist(d.bird$Wing[which(d.bird$Age == 1)], main = "wing Length", ylab = "#birds", xlab = "length")
hist(d.bird$Tarsus[which(d.bird$Age == 1)], main = "leg length", ylab = "#birds", xlab = "length")
hist(d.bird$weight[which(d.bird$Age == 1)], main = "beight [in g]", ylab = "#birds", xlab = "length")
hist(d.bird$Bill_length[which(d.bird$Age == 1)], main = "bill length", ylab = "#birds", xlab = "length")
hist(d.bird$P8[which(d.bird$Age == 1)], main = "feather length", ylab = "#birds", xlab = "length")
barplot(table(d.bird$Fat[which(d.bird$Age == 1)]), main="fat")
barplot(table(d.bird$Muscle[which(d.bird$Age == 1)]), main="muscle")
barplot(table(d.bird$sex_genetics[which(d.bird$Age == 1)]), main="sex")



df_nestling = d.bird[d.bird$Age == 1,]
cols = c("Age","Wing","Tarsus","Bill_length","P8","weight","Fat","Muscle","sex_genetics")
df_nestling_sub = data.frame(df_nestling[cols])

### nan count
df_nestling_sub %>% 
  summarise_all(~sum(is.na(.)))/NROW(df_nestling)*100
df_nestling_sub

### geo info
# data prep
cols = c("Age","Wing","Tarsus","Bill_length","P8","weight","Fat","Muscle","sex_genetics","location")
bird_dt <- as.data.table(df_nestling[cols])

NROW(bird_dt[, .N, by=.(location)][ N > 0 ])

(locations  <- bird_dt[, .N, by=.(location)][ N > 50 ])
relevant_locations_dt = setDT(bird_dt, key = 'location')[J(c(locations[,location]))]  

summary(relevant_locations_dt)
sum(is.na(relevant_locations_dt$sex_genetics)==T)

data_plot = relevant_locations_dt[!is.na(relevant_locations_dt$sex_genetics)]

x = as.data.frame(data_plot)
df =copy(x)
df$sex_genetics[df$sex_genetics == "m"] <- "Male"
df$sex_genetics[df$sex_genetics == "f"] <- "Female"

df
# plots by hand
crosstab(df, row.vars = "Age", col.vars = "sex_genetics", type = "f")
crosstab(df, row.vars = "Age", col.vars = c("sex_genetics","location"), type = "r")
crosstab(df, row.vars = c("location", "sex_genetics"), col.vars = "Age", type = "j")

wing= ggplot(df, aes(x=location, y=Wing, fill=sex_genetics)) + 
  geom_boxplot(notch=T) +
  ggtitle("Wing length") +
  theme_bw() +
  facet_wrap(~sex_genetics)
Tarsus = ggplot(df, aes(x=location, y=Tarsus, fill=sex_genetics)) + 
  geom_boxplot(notch=T) +
  ggtitle("Tarsus") +
  theme_bw() +
  facet_wrap(~sex_genetics)
Bill_length = ggplot(df, aes(x=location, y=Bill_length, fill=sex_genetics)) + 
  geom_boxplot(notch=T) +
  ggtitle("Bill length") +
  theme_bw() +
  facet_wrap(~sex_genetics)
P8 = ggplot(df, aes(x=location, y=P8, fill=sex_genetics)) + 
  geom_boxplot(notch=T) +
  ggtitle("feather length") +
  theme_bw() +
  facet_wrap(~sex_genetics)
weight = ggplot(df, aes(x=location, y=weight, fill=sex_genetics)) + 
  geom_boxplot(notch=T) +
  ggtitle("weight") +
  theme_bw() +
  facet_wrap(~sex_genetics)


### ###  ###  sex guess
unique(df_guess$Sex)

df_nestling$Sex[df_nestling$Sex == "1"] <- "m"
df_nestling$Sex[df_nestling$Sex == "2"] <- "f"
df_guess = df_nestling[!is.na(df_nestling$sex_genetics),]
df_guess = df_guess[!is.na(df_guess$Sex),]
df_guess = df_guess[df_guess$Sex != "0",]
df_guess = df_guess[df_guess$Sex != "2?",]
df_guess = df_guess[df_guess$Sex != "1?",]

(n = NROW(df_guess))
correct = df_guess$Sex == df_guess$sex_genetics
wrong = df_guess$Sex != df_guess$sex_genetics

sum(correct)/n
