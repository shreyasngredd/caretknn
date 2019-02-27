#####KNN USING CARET PACKAGE#####

#Implement a KNN model to classify the animals in to categories using 
#Caret package (Alternate method)

#Load the dataset
zoo<- read.csv(file.choose())
View(zoo)
attach(zoo)

#Standardizing the data
library(DataExplorer)
plot_str(zoo)
str(zoo)
plot_missing(zoo)
head(zoo)

#First column in dataset removed as it's not required 
zoo <- zoo[-1]
View(zoo)

#Type of animal table
table(type)

#Factorizing type column
zoo$type[zoo$type==1] <- 'Type1'
zoo$type[zoo$type==2] <- 'Type2'
zoo$type[zoo$type==3] <- 'Type3'
zoo$type[zoo$type==4] <- 'Type4'
zoo$type[zoo$type==5] <- 'Type5'
zoo$type[zoo$type==6] <- 'Type6'
zoo$type[zoo$type==7] <- 'Type7'

str(zoo)

zoo$type <- as.factor(zoo$type)
View(zoo)

#Splitting data
library(caTools)

set.seed(101)
zoo_sample <- sample.split(zoo$type,SplitRatio = 0.70)
zoo_train <- subset(zoo, zoo_sample == TRUE)
zoo_test <- subset(zoo, zoo_sample == FALSE)

#KNN
library(caret)
trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
set.seed(222)
zoo_fit <- train(type ~., data = zoo_train, method = 'knn', tuneLength = 20,
                 trControl = trcontrol, preProc = c("center","scale"))

#Model Performance
zoo_fit
plot(zoo_fit)

varImp(zoo_fit)

pred <- predict(zoo_fit, newdata = zoo_test)
confusionMatrix(pred, zoo_test$type)
