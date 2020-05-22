data(ChickWeight)

chickdata <-ChickWeight

colnames(chickdata) 

Wt<-ifelse(chickdata$weight>150,"High","low") 


empdata<-data.frame(chickdata,Wt) 

# Visualize 
#View(chickdata) 

set.seed(2) 

id<-sample(2,nrow(chickdata),prob = c(0.7,0.3),replace = T) 

train<-chickdata[id==1,] 

test<-chickdata[id==2,] 
library(e1071) # To get the package for naive bayes 
library(caret) # To draw the evaluation matrix 

model<-naiveBayes(Wt~ weight,data = train) 
model

preee3<-predict(model,test) 

confusionMatrix(table(preee3,test$Wt))

