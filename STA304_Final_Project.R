library(MASS)
library(car)
library(tidyverse)
library(dplyr)
library(survey)
library(nlme)
library(knitr)
library(ggpubr)

setwd(dir = "C:/Users/Tony/Desktop/Final Report STA304")

dataset <- read.csv("Life Expectancy Data.csv", header = TRUE)
colnames(dataset)<-c("country", "year", "status", "life_expectancy", "adult_mortality", "infant_deaths", 
                     "alcohol", "percentage_expenditure", "hepatitis_B", "measeles", "bmi", "under_five_deaths", 
                     "polio", "total_expenditure", "diphtheria", "hiv/aids", "gdp", "population", 
                     "thinness_1-19_years", "thinness_5-9_years", "income_composition", "schooling")

# Construct a dataframe only containing data from the desired year
# Change the year and run all of the code again to analyze data from another year
df <- subset(dataset, year == 2014)

# Construct a new dataframe with only the variables that we are analyzing
data_econ <- cbind(df["year"], df["life_expectancy"],
                   df["percentage_expenditure"], df["total_expenditure"],
                   df["gdp"], df["income_composition"])

data_select <-cbind(df["year"], df["life_expectancy"],
                    df["percentage_expenditure"], df["total_expenditure"],
                    df["gdp"], df["income_composition"], df["status"])

# Exclude the rows containing NA values from the data
data_select <- na.exclude(data_select)
data <- na.exclude(data_econ)
attach(data)

# Create the table of first six lines of data set
table <- head(data_select)
kable(table, col.names = gsub("[.]", " ", names(data_select)))

# Create Boxplot for 5 numerical variable 
par(mfrow=c(1,3))

boxplot(data$life_expectancy, main='Boxplot for Life Expectancy')
boxplot(data$gdp, main ='Boxplot for GDP')
boxplot(data$percentage_expenditure, main ='Boxplot for Percentage Expenditure')
boxplot(data$total_expenditure	, main = 'Boxplot for Total Expenditure	')
boxplot(data$income_composition	, main = 'Boxplot for Income Composition	')

# the cumulative frequency graphs for status 
ggplot(df,aes(status)) + geom_bar(fill = "grey", colour = "black")


# Initial linear model
model1 <- lm(life_expectancy ~ percentage_expenditure + total_expenditure + 
               gdp + income_composition)
summary(model1)

summary(model1)$r.squared

# Remove GDP
model2 <- lm(life_expectancy ~ percentage_expenditure + total_expenditure + income_composition)
summary(model2)

summary(model2)$r.squared

# Remove percentage expenditure
model3 <- lm(life_expectancy ~ total_expenditure + gdp + income_composition)
summary(model3)

summary(model3)$r.squared

# Remove Remove percentage expenditure & GDP
model4 <- lm(life_expectancy ~ total_expenditure + income_composition)
summary(model4)
summary(model4)$r.squared

# AIC with every model 
AIC(model1, model2, model3, model4)

# scatter plots of the response variable life expectancy against each variable

ggplot(data_select,aes(x = total_expenditure, y =life_expectancy , color = status))+
  geom_point()+labs(x = "Total Expenditure", y = "Life Expectancy")

ggplot(data_select,aes(x = income_composition, y =life_expectancy , color = status))+
  geom_point()+labs(x = "Income Composition of Resources", y = "Life Expectancy")

ggplot(data_select,aes(x = percentage_expenditure, y =life_expectancy, color = status ))+
  geom_point()+labs(x = "Percentage Expenditure", y = "Life Expectancy")

ggplot(data_select,aes(x = gdp, y =life_expectancy , color = status))+
  geom_point()+labs(x = "GDP", y = "Life Expectancy")




