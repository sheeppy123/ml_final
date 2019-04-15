---
title: "Practical Machine Learning Final Project"
author: "X.L"
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---


###Background  
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har> (see the section on the Weight Lifting Exercise Dataset).

###Data

The training data for this project are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

The data for this project come from this source: <http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har>. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.


### Goal

The goal of this project is to predict the manne (the "classe" variable in training set) r in which subjects did  he exercise. 

### Load the libraries and data, and set seed number



```r
library(rattle)
library(e1071)
library(randomForest)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

set.seed(111)


train.data <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
test.data <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
```


### Clean the data
 

```r
# Remove the first 7 columns data since they are not related in predicting.
train.data.clean <- train.data[,8:length(colnames(train.data))]
test.data.clean <- test.data[,8:length(colnames(test.data))]
# Remove colums with NAs
train.data.clean <- train.data.clean[, colSums(is.na(train.data.clean)) == 0] 
test.data.clean <- test.data.clean[, colSums(is.na(test.data.clean)) == 0] 
```

### Slice the original training data into training dataset and validation dataset



```r
training.index <- createDataPartition(train.data.clean$classe, p=0.8, list=F)
train.data.model <- train.data.clean[training.index, ]
validate.data <- train.data.clean[-training.index, ]
```

  
### Train the model  
We will train a ramdom forest model for this project, and a 5-fold cross validation will be used during the training process.

```r
rf.model <- train(classe ~ ., data=train.data.model, method="rf",ntree=20)
rf.model
```

```
## Random Forest 
## 
## 15699 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 15699, 15699, 15699, 15699, 15699, 15699, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9827184  0.9781289
##   27    0.9873317  0.9839699
##   52    0.9783557  0.9726106
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 27.
```

### Evaluate performance  
.  


```r
rf.predict <- predict(rf.model, validate.data)
confusionMatrix(validate.data$classe, rf.predict)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1115    0    0    0    1
##          B    7  751    1    0    0
##          C    0    5  675    4    0
##          D    0    1    3  639    0
##          E    0    2    3    3  713
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9924          
##                  95% CI : (0.9891, 0.9948)
##     No Information Rate : 0.286           
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9903          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9938   0.9895   0.9897   0.9892   0.9986
## Specificity            0.9996   0.9975   0.9972   0.9988   0.9975
## Pos Pred Value         0.9991   0.9895   0.9868   0.9938   0.9889
## Neg Pred Value         0.9975   0.9975   0.9978   0.9979   0.9997
## Prevalence             0.2860   0.1935   0.1738   0.1647   0.1820
## Detection Rate         0.2842   0.1914   0.1721   0.1629   0.1817
## Detection Prevalence   0.2845   0.1935   0.1744   0.1639   0.1838
## Balanced Accuracy      0.9967   0.9935   0.9935   0.9940   0.9981
```

```r
accuracy <- postResample(rf.predict, validate.data$classe)
acc.out <- accuracy[1]
overall.ose <- 
        1 - as.numeric(confusionMatrix(validate.data$classe, rf.predict)
                       $overall[1])
```



### Run the model with the testing dataset



```r
results <- predict(rf.model, 
                   test.data.clean[, -length(names(test.data.clean))])
results
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

