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
<strong> Classification trees </strong>

<em>
CT <- train(classe~.,data=TrainPartition,method="rpart",trControl=controlTr) <br>
fancyRpartPlot(CT$finalModel) <br>
</em>

