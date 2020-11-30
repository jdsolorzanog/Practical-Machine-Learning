# Practical machine learning Project
# Author: Juan Diego Solórzano Gómez

# Loading required packages
library(caret)
library(rattle)

# Downloading data
TrainDownload<-read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),header=TRUE)
TestDownload<-read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),header=TRUE)

# Exploring data
str(TrainDownload)

# Cleaning data
indexTr<-which(colSums(is.na(TrainDownload)|TrainDownload=="")>0.9*dim(TrainDownload)[1])
indexTe<- which(colSums(is.na(TestDownload) |TestDownload=="")>0.9*dim(TestDownload)[1])

TrainData <- TrainDownload[,-indexTr]
TestData <- TestDownload[,-indexTe]

TrainData <- TrainData[, -c(1:7)]
TestData <- TestData[,-1]

# Dividing the training cleaned dataset
set.seed(3535)
indexDivTr<-createDataPartition(TrainData$classe,p=0.75,list = FALSE)
TrainPartition<-TrainData[indexDivTr,]
TestPartition<-TrainData[-indexDivTr,]

# Training the data

# Method 1: Classification tree
controlTr <- trainControl(method = "cv",number = 5)
CT <- train(classe~.,data=TrainPartition,method="rpart",trControl=controlTr)
fancyRpartPlot(CT$finalModel)
predictionCT <- predict(CT, newdata=TestPartition)
confMatCT <- confusionMatrix(TestPartition$classe,predictionCT)
confMatCT$table
confMatCT$overall[1]

# Method 2: Random Forests
RF <- train(classe~., data=TrainPartition, method="rf", trControl=controlTr, verbose=FALSE)
predictionRT <- predict(RF,newdata=TestPartition)
confMatRF <- confusionMatrix(TestPartition$classe,predictionRT)
confMatRF$table
confMatRF$overall[1]
plot(RF)

# Method 3: Boosting
GBM <- train(classe~., data=TrainPartition, method="gbm", trControl=controlTr, verbose=FALSE)
print(GBM)
predictionGBM <- predict(GBM,newdata=TestPartition)
confMatGBM <- confusionMatrix(TestPartition$classe,predictionGBM)
confMatGBM$table
confMatGBM$overall[1]
plot(GBM)
