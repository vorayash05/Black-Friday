#Importing libraries

library(tidyverse)
library(caTools)
library(randomForest)

#Importing dataset
training_set <- read.csv('black_friday_train.csv')
test_set <- read.csv('black_friday_test.csv')
test_set$Purchase <- NA
sam <- test_set[,c(1,2)]
dataset <- rbind(training_set, test_set)
dataset <- dataset[-1]

# Data Cleaning
dataset$Gender <- factor(dataset$Gender,
                         levels = c('F','M'),
                         labels = c(0,1))
dataset$Age <- factor(dataset$Age,
                      levels = c('0-17','18-25','26-35','36-45','46-50','51-55','55+'),
                      labels = c(17,25,35,45,50,55,60))
dataset$Occupation <- as.factor(dataset$Occupation)
dataset$City_Category <- factor(dataset$City_Category,
                                levels = c('A','B','C'),
                                labels = c(1,2,3))
dataset$Stay_In_Current_City_Years<- factor(dataset$Stay_In_Current_City_Years,
                                levels = c('0','1','2','3','4+'),
                                labels = c(0,1,2,3,5))
dataset$Marital_Status <- as.factor(dataset$Marital_Status)

dataset$Product_Category_1 <- ifelse(is.na(dataset$Product_Category_1),
                                     -1,
                                     dataset$Product_Category_1)
dataset$Product_Category_2 <- ifelse(is.na(dataset$Product_Category_2),
                                     -1,
                                     dataset$Product_Category_2)
dataset$Product_Category_3 <- ifelse(is.na(dataset$Product_Category_3),
                                     -1,
                                     dataset$Product_Category_3)
training_set <- dataset[1:550068,]
test_set<- dataset[550069:783667 ,]



#train test split
index <- sample(1:nrow(training_set),100000)
data<- training_set[index,]
set.seed(123)
split = sample.split(data$Purchase, SplitRatio = 0.8)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
train <- train[-1]
test <- test[-1]
set.seed(123)
regressor = randomForest(x = train[-10],
                          y = train$Purchase,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set[-10])
sam <- cbind(sam, y_pred)
ss <- write.csv(sam, "ss.csv")