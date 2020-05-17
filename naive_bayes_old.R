# Libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Data

data <- read.csv(file.choose(), header = T)

# Study data structure
str(data)

# Cross tabulation

xtabs(~admit+rank, data = data)

# Convert the admit and rank values from integer to factors

data$rank <- as.factor(data$rank)

data$admit <- as.factor(data$admit)

# Visualization
pairs.panels(data[-1])

# Data Partition

# Set a random seed
set.seed(1234)

# Sample the avialble data in two sets for training and testing
# 2 -> split into 2 parts
# number of rows = nrow(data) -> whatever we have in data
# With replacement, replace = T
# prob = c(0.8, 0.2), 80% for training data and 20% for testing
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))

train <- data[ind==1,]
test <- data[ind==2,]




# Naive Bayes Model

# Train model using training data set
model <- naive_bayes(admit ~ ., data = train)
model










