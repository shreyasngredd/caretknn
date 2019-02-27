#####KNN USING CARET PACKAGE#####

#Prepare a model for glass classification using KNN using 
#Caret package (Alternate method)

#Load the dataset
glass<- read.csv(file.choose())
View(glass)
attach(glass)

#Standardizing the data
library(DataExplorer)
plot_str(glass)
str(glass)
plot_missing(glass)
head(glass)

#Factorizing Type column
glass$Type[glass$Type==1] <- 'Type1'
glass$Type[glass$Type==2] <- 'Type2'
glass$Type[glass$Type==3] <- 'Type3'
glass$Type[glass$Type==4] <- 'Type4'
glass$Type[glass$Type==5] <- 'Type5'
glass$Type[glass$Type==6] <- 'Type6'
glass$Type[glass$Type==7] <- 'Type7'

str(glass)

glass$Type <- as.factor(glass$Type)
View(glass)

#Splitting data
library(caTools)
library(class)

set.seed(101)
glass_sample <- sample.split(glass$Type,SplitRatio = 0.70)
glass_train <- subset(glass, glass_sample == TRUE)
glass_test <- subset(glass, glass_sample == FALSE)

#KNN
library(caret)
trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
set.seed(222)
glass_fit <- train(Type ~., data = glass_train, method = 'knn', tuneLength = 20,
                   trControl = trcontrol, preProc = c("center","scale"))

#Model Performance
glass_fit
plot(glass_fit)

varImp(glass_fit)

pred <- predict(glass_fit, newdata = glass_test)
confusionMatrix(pred, glass_test$Type)
