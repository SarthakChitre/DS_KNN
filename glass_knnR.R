library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)

#DATA(GLASS)

glass <- read.csv(file.choose())
View(glass)
colnames(glass)
standard <- scale(glass[,1:9])

#Join the standardized data with the target column
data <- cbind(standard,glass[10])

#check if here are any missing values
is.na(data)

#looks like the data is free from na's

head(data)


####visulization

corPlot(cor(data))

#TEST AND TRAIN DATA SPLIT

set.seed(123)

sample <- sample.split(data$Type, SplitRatio = 0.80)

train <- subset(data,sample==TRUE)
test <- subset(data,sample==FALSE)

# KNN MODEL

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)

#error is prediction

error <- mean(predicted.type!=test$Type)


#Confusion Matrix

#model is not predict a certain factor. so i used table()

confusionMatrix(table(predicted.type,test$Type))  #Accuracy : 0.6279 #P-Value : NA 

predicted.type <- NULL
error.rate <- NULL


for (i in 1:10) {
  predicted.type <- knn(train[1:9],test[1:9],train$Type, k=i)
  error.rate[i] <- mean(predicted.type!=test$Type)
  
}
knn.error <- as.data.frame(cbind(k=1:10, error.type = error.rate))

dev.off()
#Choosing K Value by Visualization
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')

#Result
predicted.type <- knn(train[1:9],test[1:9],train$Type,k=3)

#error prediction
error <- mean(predicted.type!=test$Type)

#Confusion Matrix
confusionMatrix(table(predicted.type,test$Type)) #Accuracy : 0.6744 # P-Value : NA 

#####################################################ACCURACY= 67%
