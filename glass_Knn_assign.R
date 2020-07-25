library(class)
library(caret)
library(ggplot2)
glass=read.csv(file.choose())
View(glass)
prop.table(table(glass$Type))*100
table(glass$Type)
set.seed(123)

#Creating Training and test dataset
inTraininglocal=createDataPartition(glass$Type,p=0.75,list=F)
glass_train=glass[inTraininglocal,]
glass_test=glass[-inTraininglocal,]

#create label for training and test data
glass_train_labels=glass[inTraininglocal,10]
View(glass_train_labels)
glass_test_labels=glass[-inTraininglocal,10]

#KNN model
model1=knn(train=glass_train[,-10],
           test=glass_test[,-10],
           cl=glass_train_labels,k=2)

## Now evualuation the model performance

# install package gmodels
install.packages("gmodels")
library("gmodels")

CrossTable(x=glass_test_labels,y=model1,
           prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
confusionMatrix(table(model1,glass_test_labels))  #0.5338  
#For k=10 accuracy= 0.5577
#k=15 0.5
#k=5  0.634
#k=3  0.73 #Best.

##By normalizing the data##


glass_z=as.data.frame(scale(glass[,-10]))
View(glass_z)
summary(glass_z)
set.seed(123)
#Creating Training and test dataset
inTraininglocal=createDataPartition(glass$Type,p=0.75,list=F)
glass_train_z=glass_z[inTraininglocal,]
glass_test_z=glass_z[-inTraininglocal,]

#create label for training and test data ..Already created
glass_train_labels=glass[inTraininglocal,10]
glass_test_labels=glass[-inTraininglocal,10]


#KNN model
model2=knn(train=glass_train_z,
           test=glass_test_z,
           cl=glass_train_labels,k=1)

## Now evualuation the model performance
CrossTable(x=glass_test_labels,y=model2,
           prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
confusionMatrix(table(model2,glass_test_labels))  #0.5962
#k=10 Acc=0.61
#k=5  Acc=0.65
#k=3  Acc=0.69
#k=1  0.71

#
trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
set.seed(222)
fit <- train(Type ~., data = glass, method = 'knn', tuneLength = 20,
             trControl = trcontrol, preProc = c("center","scale"),
             tuneGrid=expand.grid(k=c(1,2,3,5,10,15,20)))
fit
plot(fit)
