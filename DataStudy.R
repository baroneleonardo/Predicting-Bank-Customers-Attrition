
# Set Working Directory
setwd("C:\\Users\\Leonardo\\Documents\\POLIMI\\Applied-Statistics-Project")

# Read Data

data <- read.csv('dataset.csv', header=TRUE)

head(data)                                                           
dim(data)
dimnames(data)

# Find if there are some Null Values
is.null(data)
sum(is.na(data))

# Category type of the DataSet
sapply(data, class)

# Look at the labels of the data
category = labels(data)[[2]]
length(category)

# Computing mean and variance of all categories (the cat are none of course)
sapply(data, mean)
sapply(data, sd)
sapply(data, var)

# With attach command i can accsess only from the variable name
attach(data)
data <- subset(data, Education_Level != "Unknown" || Income_Category != "Unknown")

# Select Categorical and numerical columns
categ_var = colnames(data)[grepl('factor|logical|character',sapply(data,class))]
num_var = colnames(data)[!grepl('factor|logical|character',sapply(data,class))]
boxplot(Customer_Age)
boxplot(Dependent_count)

library(ggplot2)
x11()
ggplot(data, aes(x = Customer_Age, fill = Attrition_Flag)) +
  geom_density(alpha = 0.8, color = NA) +
  scale_fill_discrete(name = "Attrition_Flag") +
  xlab("Age") + ylab("Density") +
  ggtitle("Distribution of Age by Attrition Flag")