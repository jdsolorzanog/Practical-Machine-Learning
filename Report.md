# Project report

<strong> Getting data </strong>

I started loading the required packages and downloading the data 

<em>
library(caret) <br>
library(rattle) <br>
TrainDownload<-read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),header=TRUE) <br>
TestDownload<-read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),header=TRUE) <br>
</em>
<br>

The train dataset consists is a csv file with 19622 rows and 160 columns, while the test dataset has 20 rows and 160 columns. 

<br>

<strong> Cleaning data </strong>

Let's take a look at the data

<em> str(TrainDownload) </em>

We can observe, first, that the first seven observations correspond to data from the test subjects, which have no objective relevance in the prediction. In addition, many missing values and NA are observed in some of the variables, which will not be useful to make predictions either. <br>
For this reason, it is decided to eliminate the columns that contain missing values or NA

<em>
indexTr<-which(colSums(is.na(TrainDownload)|TrainDownload=="")>0.9*dim(TrainDownload)[1]) <br>
indexTe<- which(colSums(is.na(TestDownload) |TestDownload=="")>0.9*dim(TestDownload)[1]) <br>
TrainData <- TrainDownload[,-indexTr] <br>
TestData <- TestDownload[,-indexTe] <br>
TrainData <- TrainData[, -c(1:7)] <br>
TestData <- TestData[,-1] <br>
</em>

Now, we partition the data. Note that the seed is set to a random value <br>

<em>
set.seed(3535) <br>
indexDivTr<-createDataPartition(TrainData$classe,p=0.75,list = FALSE) <br>
TrainPartition<-TrainData[indexDivTr,] <br>
TestPartition<-TrainData[-indexDivTr,] <br>
</em>
<br>
It is time to start with the predictions. The methods of classification trees, random forests and the gradient boosting method are used. First, we apply cross validation with n=5, this will limit the effects of overfitting, and improve the efficicency of the models <br>

<em> controlTr <- trainControl(method = "cv",number = 5) </em>

<br>
<strong> Classification trees </strong> <br>

<em>
CT <- train(classe~.,data=TrainPartition,method="rpart",trControl=controlTr)<br>
fancyRpartPlot(CT$finalModel)<br>
</em>
<br>

![Classification tree](arbol.png)<br>

<em>
predictionCT <- predict(CT, newdata=TestPartition) <br>
confMatCT <- confusionMatrix(TestPartition$classe,predictionCT) <br>
confMatCT$table <br>
</em>

![Confusion matrix](ConfMatCT.PNG)<br>

<em>confMatCT$overall[1] </em><br>

![Accuracy](AccCT.PNG)<br>

Very low accuracy was obtained with this method, so it is not recommended for prediction <br>

 <br>
<strong> Random forests </strong> <br>

<em>
RF <- train(classe~., data=TrainPartition, method="rf", trControl=controlTr, verbose=FALSE) <br>
predictionRF <- predict(RF,newdata=TestPartition) <br>
confMatRF <- confusionMatrix(TestPartition$classe,predictionRF) <br>
confMatRF$table <br>
</em>
 <br>

![Confusion matrix](ConfMatRF.PNG)<br>

<em> confMatRF$overall[1] </em>

![Accuracy](AccRF.PNG)<br>

In contrast, this method gives a very high accuracy, very close to 1, which means that it will allow accurate predictions <br>

If we graphically analyze the precision of the model according to the number of predictors, the following is observed <br>

<em> plot(RF) </em> <br>

![Graphic](PlotRF.PNG)<br>

Although the maximum precision is accuracy with 27 predictors, from 2 it is obtained almost as high accuracy <br>

 <br>
<strong> Gradient Boosting Method </strong> <br>

<em>
GBM <- train(classe~., data=TrainPartition, method="gbm", trControl=controlTr, verbose=FALSE) <br>
predictionGBM <- predict(GBM,newdata=TestPartition) <br>
confMatGBM <- confusionMatrix(TestPartition$classe,predictionGBM)<br>
confMatGBM$table <br>
</em>
 <br>
 
![Confusion matrix](ConfMatGBM.PNG)<br>

<em> confMatGBM$overall[1] </em>

![Accuracy](AccGBM.PNG)<br>

Although with this method a high accuracy is also obtained, it can be concluded that the optimal method for prediction is random forests, using 27 predictors, which will give a margin of error of less than 1%
