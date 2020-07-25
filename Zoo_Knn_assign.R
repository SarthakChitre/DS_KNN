library(class)
library(caret)
library(ggplot2)
zoo=read.csv(file.choose())
View(zoo)
prop.table(table(zoo$type))*100
table(zoo$type)
set.seed(123)
zoo=zoo[,-1]
View(zoo)

#Data partition
inTraininglocal=createDataPartition(zoo$type,p=0.75,list=F)
zoo_train=zoo[inTraininglocal,]
zoo_test=zoo[-inTraininglocal,]

#create label for training and test data
zoo_train_labels=zoo[inTraininglocal,17]
View(zoo_train_labels)
zoo_test_labels=zoo[-inTraininglocal,17]

#KNN model
model1=knn(train=zoo_train[,-17],
           test=zoo_test[,-17],
           cl=zoo_train_labels,k=3)

## Now evualuation the model performance
library("gmodels")
CrossTable(x=zoo_test_labels,y=model1,
           prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
a= table(model1,zoo_test_labels)  
a
sum(diag(a)/sum(a)) #0.833


#
trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
set.seed(222)
fit <- train(type ~., data = zoo, method = 'knn', tuneLength = 20,
             trControl = trcontrol, preProc = c("center","scale"),
             tuneGrid=expand.grid(k=c(1,2,3,4,5,10)))

fit
plot(fit)

