# to avoid the scientific e notation in the results
options(scipen=999)

# Exploratory Data Analysis 

# getting summary of dataset
# min max median mean quartiles length - for numerical variables 
# mode class length - for categorical variables
summary(df)

# Look at the first few rows of the data
head(df)
head(df[, c('State', 'PovertyEst', 'medIncome', 'Name', 'popEst2015', 'incidenceRate', 'deathRate', 'Region')])

# Look at the data types of each column
str(df)

# Check for missing values
sum(is.na(df))

# drop rows with *
df[df == "*"] <- NA
df[df == "**"] <- NA
df <- na.omit(df)

#number of rows after removing rows with *
n_rows <- nrow(df)
n_rows

#number of states
unique_count <- length(unique(df$State))
unique_count

#1a)
#Which locations (e.g., counties, states, regions, etc.) of the country are most prone to cancer?
install.packages("dplyr")
library(dplyr)
top_counties <- df %>% arrange(desc(incidenceRate)) %>% select(Name, State, incidenceRate, Region) %>% head(n = 10)
top_counties

#1b)
#Create a 4-level indicator variable for Median Income 
#analysis comparing the incidence/death rate of these levels. 
# Four levels could be Very Low, Low, High, Very High, or similar.
df$income_level <- cut(df$medIncome, breaks=c(22640, 38448, 44710, 52244, 125635), labels=c("Very Low", "Low", "High", "Very High"))
head(df[, c('State', 'PovertyEst', 'medIncome', 'Name', 'popEst2015', 'incidenceRate', 'deathRate', 'Region', 'income_level')])

#Comparing incidence and death rates of these levels
library(ggplot2)
ggplot(df, aes(x=income_level, y=incidenceRate)) + geom_boxplot() + labs(x = "4 - level median income", y = "Cancer incidence Rate")

ggplot(df, aes(x=income_level, y=deathRate)) + geom_boxplot() + labs(x = "4 - level median income", y = "Cancer death Rate")

#1c) done on excel

#1d)
# Do a correlation analysis
df$fiveYearTrend <- as.numeric(df$fiveYearTrend)
cor(df[, c("PovertyEst", "medIncome", "popEst2015", "incidenceRate", "avgAnnCount", "fiveYearTrend", "deathRate", "avgDeathsPerYear")])


###########################################################################################################################

# Regression Analysis
#2a)
# Create regression models for the incidence rate
fit = lm(incidenceRate ~ PovertyEst + medIncome + popEst2015 + Region, data = df)
summary (fit)

fit1 = lm(incidenceRate ~ PovertyEst + medIncome + popEst2015 + Region  + medIncome*PovertyEst, data = df)
summary (fit1)

fit2 = lm(incidenceRate ~ PovertyEst + medIncome + popEst2015 + Region  + medIncome*PovertyEst + log(medIncome), data = df)
summary (fit2)

fit3 = lm(incidenceRate ~ PovertyEst + medIncome + popEst2015 + Region  +  medIncome*PovertyEst + log(medIncome) + log(PovertyEst), data = df)
summary (fit3)

#2b)
# Create regression models for the death rate
fit4 = lm(deathRate ~ PovertyEst + medIncome + popEst2015 + Region + incidenceRate, data = df)
summary (fit4)

fit5 = lm(deathRate ~ PovertyEst + medIncome + popEst2015 + Region + incidenceRate + medIncome*PovertyEst, data = df)
summary (fit5)

fit6 = lm(deathRate ~ PovertyEst + medIncome + popEst2015 + Region + incidenceRate + medIncome*PovertyEst + log(medIncome), data = df)
summary (fit6)

fit7 = lm(deathRate ~ PovertyEst + medIncome + popEst2015 + Region + incidenceRate + medIncome*PovertyEst + log(medIncome) + log(PovertyEst), data = df)
summary (fit7)

#2c)
# Checking the assumptions
# 1: linear relationships/ heteroskedasticity
#incience rate model 
plot(df$PovertyEst[complete.cases(df)], fit3$residuals[complete.cases(df)])
abline(0,0)

plot(df$medIncome[complete.cases(df)], fit3$residuals[complete.cases(df)])
abline(0,0)

plot(df$popEst2015[complete.cases(df)], fit3$residuals[complete.cases(df)])
abline(0,0)

#death rate model
plot(df$PovertyEst[complete.cases(df)], fit7$residuals[complete.cases(df)])
abline(0,0)

plot(df$medIncome[complete.cases(df)], fit7$residuals[complete.cases(df)])
abline(0,0)

plot(df$popEst2015[complete.cases(df)], fit7$residuals[complete.cases(df)])
abline(0,0)

plot(df$incidenceRate[complete.cases(df)], fit7$residuals[complete.cases(df)])
abline(0,0)

# 2: multicollinearity
cor(df[, c("PovertyEst", "medIncome", "popEst2015", "avgAnnCount", "incidenceRate", "fiveYearTrend", "deathRate", "avgDeathsPerYear")])
fit8 = lm(incidenceRate ~ PovertyEst + medIncome + Region  + medIncome*PovertyEst + log(medIncome) + log(PovertyEst), data = df)
summary (fit8)

cor(df[, c("PovertyEst", "medIncome", "popEst2015", "incidenceRate", "avgAnnCount", "deathRate", "avgDeathsPerYear")])
fit9 = lm(deathRate ~ PovertyEst + medIncome + Region + incidenceRate + medIncome*PovertyEst + log(medIncome) + log(PovertyEst), data = df)
summary (fit9)

# 3: independent error terms
acf(fit8$residuals)
acf(fit9$residuals)

# 4: errors follow normal distribution
hist(fit8$residuals)
hist(fit9$residuals)




