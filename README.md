# Stevens_CS513
Data Mining


rm(list=ls())

library(rpart)
library(rpart.plot)
library(RColorBrewer)

bc_data=read.csv("I://Stevens/cs-513/HW/raw data/breast-cancer-wisconsin.data.csv")

#sampling data
indexx<-sort(sample(nrow(bc_data),as.integer((.70*nrow(bc_data)))))

#creating training data set i.e 70 % of the new_bc_data as training data
train_data <- bc_data[indexx,]

#creating test data set i.e 30 % of the new_bc_data as test data
test_data =bc_data[-indexx,]

#Growing the tree
Cart_class<- rpart( factor(Class)~., data =train_data )

summary(Cart_class)

#Plotting the graphs
rpart.plot(Cart_class)
fancyRpartPlot(Cart_class)

#scoring
c_prediction<-predict( Cart_class ,test_data , type="class" )

#frequency table
table(actual=test_data[,11],c_prediction)

#accuracy in percentage
match<- (test_data[,11]==c_prediction)*100
accuracy<-sum(match)/length(match)
accuracy


#error rate calculation
err<- (test_data[,11]!=c_prediction)
error_rate<-sum(err)/length(err)
error_rate

















