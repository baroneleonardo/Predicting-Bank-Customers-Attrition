library(car)
library(dplyr)
library(MASS)
library(mvtnorm)
library(mvnormtest)
library(corrplot)

data <- read.csv('Data_senza_unknown.csv')
attach(data)

# map target variable to numeric

data[which(Attrition_Flag == 'Attrited Customer'), 2] <- 1
data[which(Attrition_Flag == 'Existing Customer'), 2] <- 0

detach(data)
attach(data)

# groups

attrited <- data[which(Attrition_Flag == 1), ]
existing <- data[which(Attrition_Flag == 0), ]

# gender

x11()
barplot(table(Attrition_Flag, Gender), beside = T, ylim = c(0,4000),
        xlab = 'Gender', ylab = 'Count', legend = c('Existing', 'Attrited'))

x11()
barplot(prop.table(table(Attrition_Flag, Gender), margin = 2), beside = F,
        ylim = c(0,1), xlab = 'Gender', ylab = 'Proportion',
        legend = c('Existing', 'Attrited'))

# income

x11()
barplot(table(Attrition_Flag, Income_Category), beside = T, ylim = c(0,3000),
        xlab = 'Income Category', ylab = 'Count',
        legend = c('Existing', 'Attrited'))

x11()
barplot(prop.table(table(Attrition_Flag, Income_Category), margin = 2),
        beside = F, ylim = c(0,1), xlab = 'Income Category',
        ylab = 'Proportion', legend = c('Existing', 'Attrited'))

# marital status

x11()
barplot(table(Attrition_Flag, Marital_Status), beside = T, ylim = c(0,4000),
        xlab = 'Marital Status', ylab = 'Count',
        legend = c('Existing', 'Attrited'))

x11()
barplot(prop.table(table(Attrition_Flag, Marital_Status), margin = 2),
        beside = F, ylim = c(0,1), xlab = 'Marital_Status',
        ylab = 'Proportion', legend = c('Existing', 'Attrited'))

# card category

x11()
barplot(table(Attrition_Flag, Card_Category), beside = T, ylim = c(0,6000),
        xlab = 'Card Category', ylab = 'Count',
        legend = c('Existing', 'Attrited'))

x11()
barplot(prop.table(table(Attrition_Flag, Card_Category), margin = 2),
        beside = F, ylim = c(0,1), xlab = 'Card_Category',
        ylab = 'Proportion', legend = c('Existing', 'Attrited'))

# education

x11()
barplot(table(Attrition_Flag, Education_Level), beside = T, ylim = c(0,2500),
        xlab = 'Educational Level', ylab = 'Count',
        legend = c('Existing', 'Attrited'))

x11()
barplot(prop.table(table(Attrition_Flag, Education_Level), margin = 2),
        beside = F, ylim = c(0,1), xlab = 'Educational Level',
        ylab = 'Proportion', legend = c('Existing', 'Attrited'))

# numerical variables

numerical <- data[, c(1,2,3,5,10,11,12,13,14,15,16,17,18,19,20,21)]
detach(data)
attach(numerical)

for(i in 1:16){
  numerical[,i] <- as.numeric(numerical[,i])
}

for(i in 3:16){
  x11()
  hist(numerical[, i], main = NULL, xlab = names(numerical)[i],
       ylab = 'Frequency')
}

C <- cor(numerical[,2:16])
x11()
corrplot(C, method = 'number', tl.cex = 0.7, cl.cex = 0.7, number.cex = 0.7)

for(i in 3:16){
  x11()
  boxplot(numerical[,i] ~ numerical[,2], xlab = names(numerical)[2],
          ylab = names(numerical)[i])
}





















