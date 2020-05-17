# Read dataset 
empdata<-data.csv("Employee.csv") 
colnames(empdata) # Print col names in data set 
# Label 
# Run over the data frame if a value in the "Emp_Sal" column in the data  
# frame >50k it returns "High" else "Low, store these labels in Sal 
Sal<-ifelse(empdata$Emp_Sal==">50K","High","low") 
# Add the "Sal"column in the data frame 
empdata<-data.frame(empdata,Sal) 
# Visualize 
View(empdata) 
# Take 1000 rows and remove 15 col (15th is the EMp_sal col, 16th row is the Sal column we added) 
empdata<-empdata(1:1000,-15) 
# Split Train and Test 
# Seed 
set.seed(2) 
# Sample the data in 2 parts, with these probability we say 
# 70% of the time we produce 1 and 30% time we produce 2  
# The number of ones and twos will be the number of rows 
# that is nrow(empdata) in our data set. 
# Thus 70% of our data will be sampled as 1 and 30% as 2 
id<-sample(2,nrow(empdata),prob = c(0.7,0.3),replace = T) 
# Pick the training data (Data sample as 1) 
emptrain<-empdata[id==1,] 
# Pick the testing data (Data sample as 2) 
emptest<-empdata[id==2,] 
library(e1071) # To get the package for naive bayes 
library(caret) # To draw the evaluation matrix 
# Train our model using specific featuresin our taining set 
# we can train using all features by "naiveBayes(Sal~.emptrain)" 
emp_nb<-naiveBayes(Sal~ Age_Of_emp + Emp_Stat_type + Edu_of_Emp + Edu_Cat + Occ_Of_Emp + Work_hour_in_week + country_of_res,data = emptrain) 
emp 
#Use our model "emp_nb" over our test data set "emptest" to check prediction 
preee3<-predict(emp_nb,emptest) 
# Get a confussion matrix to understand the accurancy, specificity, etc of our predictions and our model. 
confusionMatrix(table(preee3,emptest$Sal))