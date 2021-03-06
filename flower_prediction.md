---
title: 'Flower organ identity using machine learning in R'
date: 2018-12-10
tags:
  - Machine Learning
  - Data analysis
  - caret package 
---


This is the example of using machine learning in predicting the specific genes in flower developments. 
======

* Project 1: using caret in R for machine learning and apply into "Modeling Transcription binding contribution and gene expression". I use the random forest algorithm to build the model to predict potential regulators affect domain specificity based on the expression changes between 2 floral specific organs (classify genes in different flower organs including petals, sepals, carpels and stamens). Code can be [found here](https://github.com/donalbonny/MachineLearning_projects/blob/master/flower_model.Rmd)

This is for practicing machine learning model, not for research purpose. 

Please follow the reference paper for the scientific resource

[Paper 1](https://github.com/donalbonny/donalbonny.github.io/blob/master/assets/s41467-018-06772-3.pdf)

[Paper 2](https://github.com/donalbonny/donalbonny.github.io/blob/master/assets/419.full.pdf)
   
   
![alt text](https://github.com/donalbonny/donalbonny.github.io/blob/master/figures/2012-12-10-floral-development-2/flower%20prediction.png)





Modeling Transcription binding contribution and gene expression
---------------------------------------------------------------

Domain-specific genes targeted by floral quartets that determine different floral organ identities.

-   Objective: to build the model to predict or to identify potential regulators affect domain specificity based on the expression changes between 2 domains.

-   Dataset: the expression changes between 2 different domains in different organ specific genes (i.e., sepals, petals, stamens, and carpels). Dataset from this analysis was obtained from the published data from Chen et al., 2018 and JiaoY et al.,2010. Based on domain-specific mRNA translatome data (JiaoY et al., 2010), Chen et al., 2018 identified 1013 organ-specific genes that were highly expressed either in AP1-specific (sepal), AG-specific (carpel), AP1/AP3-common (petal), or AP3/AG-common (stamen) domains


``` r
set.seed(2018)
data <-  read.csv("flower.csv", header = T)
head(data)
```

    ##        Cluster AP1_Stage4 AP1_Stage6 AP3_Stage4 AP3_Stage6 AG_Stage4
    ## 1 AP1-specific    217.030    177.405    110.450    144.255    64.190
    ## 2 AP1-specific     49.095     38.205     23.695     30.435    13.355
    ## 3 AP1-specific     32.965     28.935     14.635     17.735    15.520
    ## 4 AP1-specific    150.445    127.280     58.755    114.165    53.435
    ## 5 AP1-specific    188.905    235.005    107.440    106.305    90.205
    ## 6 AP1-specific     14.135     16.700      5.700      6.550     9.325
    ##   AG_Stage6
    ## 1    79.340
    ## 2    16.030
    ## 3    13.955
    ## 4    60.040
    ## 5    93.330
    ## 6    11.935

``` r
dim(data)
```

    ## [1] 678   7

``` r
sapply(data, class)
```

    ##    Cluster AP1_Stage4 AP1_Stage6 AP3_Stage4 AP3_Stage6  AG_Stage4 
    ##   "factor"  "numeric"  "numeric"  "numeric"  "numeric"  "numeric" 
    ##  AG_Stage6 
    ##  "numeric"

We will split dataset into train dataset (80% data) and validation dataset

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.5.2

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
validation_index <- createDataPartition(data$Cluster, p = 0.8, list = FALSE)
validation <- data[-validation_index,]
data <- data[validation_index,]
dim(data)
```

    ## [1] 544   7

You now have training data in the dataset variable and a validation set we will use later in the validation variable.

``` r
#Look at the cluster distribution
library(caret)
percentage <- prop.table(table(data$Cluster)) * 100
cbind(freq= table(data$Cluster), percentage = percentage)
```

    ##              freq percentage
    ## AG-specific   142  26.102941
    ## AP1-specific  134  24.632353
    ## AP1_AP3       161  29.595588
    ## AP3-specific   54   9.926471
    ## AP3_AG         53   9.742647

So there are 5 different clusters: AG-Specific, AP1 specific, AP1-AP3, AP3 specific and AP3-AG that organ specific genes can be classified into

There are 6 features can be used to build the model: AP1-Stage 4, AP1-Stage6, AP3-Stage 4, AP3-Stage 6, AG-Stage 4, AG-Stage 6

``` r
#statistical summary of data 
summary(data)
```

    ##          Cluster      AP1_Stage4          AP1_Stage6       
    ##  AG-specific :142   Min.   :    0.000   Min.   :    0.000  
    ##  AP1-specific:134   1st Qu.:    3.594   1st Qu.:    3.471  
    ##  AP1_AP3     :161   Median :   14.210   Median :   14.207  
    ##  AP3-specific: 54   Mean   :  117.794   Mean   :  102.833  
    ##  AP3_AG      : 53   3rd Qu.:   54.330   3rd Qu.:   47.571  
    ##                     Max.   :14948.460   Max.   :11070.680  
    ##    AP3_Stage4         AP3_Stage6          AG_Stage4       
    ##  Min.   :   0.000   Min.   :    0.000   Min.   :   0.000  
    ##  1st Qu.:   2.522   1st Qu.:    2.946   1st Qu.:   2.467  
    ##  Median :  11.773   Median :   12.715   Median :   9.107  
    ##  Mean   :  76.514   Mean   :   99.020   Mean   :  48.222  
    ##  3rd Qu.:  41.072   3rd Qu.:   47.440   3rd Qu.:  28.045  
    ##  Max.   :4591.915   Max.   :10642.870   Max.   :2163.015  
    ##    AG_Stage6       
    ##  Min.   :   0.000  
    ##  1st Qu.:   2.829  
    ##  Median :   9.440  
    ##  Mean   :  49.843  
    ##  3rd Qu.:  29.034  
    ##  Max.   :2333.475

``` r
#split the data input and output 
x = data[,2:7]
head(x)
```

    ##   AP1_Stage4 AP1_Stage6 AP3_Stage4 AP3_Stage6 AG_Stage4 AG_Stage6
    ## 1    217.030    177.405    110.450    144.255    64.190    79.340
    ## 2     49.095     38.205     23.695     30.435    13.355    16.030
    ## 3     32.965     28.935     14.635     17.735    15.520    13.955
    ## 4    150.445    127.280     58.755    114.165    53.435    60.040
    ## 5    188.905    235.005    107.440    106.305    90.205    93.330
    ## 7    124.925     72.550     37.515     61.455    21.615    34.300

``` r
y = data[,1]
head(y)
```

    ## [1] AP1-specific AP1-specific AP1-specific AP1-specific AP1-specific
    ## [6] AP1-specific
    ## Levels: AG-specific AP1-specific AP1_AP3 AP3-specific AP3_AG

Let's check the distribution of the train data by plotting boxplot for x including the distribution of all 6 features.

``` r
par(mfrow=c(1,6))
  for(i in 1:6) {
  boxplot(x[,i], main=names(x)[i])
}
```

![alt text](https://github.com/donalbonny/donalbonny.github.io/blob/master/figures/2012-12-10-floral-development-2/unnamed-chunk-7-1.png)



And the distribution of clusters

``` r
plot(y)
```

![alt text](https://github.com/donalbonny/donalbonny.github.io/blob/master/figures/2012-12-10-floral-development-2/unnamed-chunk-8-1.png)



Looks like this is imbalance data since we have AP1-AP3 is the highest number among 6 different clusters. AP3-specific and AP3-AG has only 50, which is 1/3 of the number of AP1-AP3 cluster.

This is called "Accuracy Paradox". It is the case where your accuracy measures tell the story that you have excellent accuracy (such as 90%), but the accuracy is only reflecting the underlying class distribution.

Next part, we should look at the following performance measures that can give more insight into the accuracy of the model than traditional classification accuracy:

-   Confusion Matrix: A breakdown of predictions into a table showing correct predictions (the diagonal) and the types of incorrect predictions made (what classes incorrect predictions were assigned).
-   Precision: A measure of a classifiers exactness.
-   Recall: A measure of a classifiers completeness
-   F1 Score (or F-score): A weighted average of precision and recall.

Also, take a look at the following:

-   Kappa (or Cohen's kappa): Classification accuracy normalized by the imbalance of the classes in the data.
-   ROC Curves: Like precision and recall, accuracy is divided into sensitivity and specificity and models can be chosen based on the balance thresholds of these values.

Now, we can also plot t-SNE (t-distributed stochastic neighbor embedding) plot showing expression profiles of organ-specific genes

``` r
#tSNE
library(Rtsne)
```

    ## Warning: package 'Rtsne' was built under R version 3.5.2

``` r
set.seed(2018)
tsne <- Rtsne(x, dims = 3, perplexity=30, verbose=TRUE, max_iter = 500)
```

    ## Performing PCA
    ## Read the 544 x 6 data matrix successfully!
    ## OpenMP is working. 1 threads.
    ## Using no_dims = 3, perplexity = 30.000000, and theta = 0.500000
    ## Computing input similarities...
    ## Building tree...
    ## Done in 0.11 seconds (sparsity = 0.239444)!
    ## Learning embedding...
    ## Iteration 50: error is 56.523471 (50 iterations in 0.15 seconds)
    ## Iteration 100: error is 52.865649 (50 iterations in 0.09 seconds)
    ## Iteration 150: error is 52.680297 (50 iterations in 0.09 seconds)
    ## Iteration 200: error is 52.690323 (50 iterations in 0.09 seconds)
    ## Iteration 250: error is 52.691137 (50 iterations in 0.09 seconds)
    ## Iteration 300: error is 0.376420 (50 iterations in 0.09 seconds)
    ## Iteration 350: error is 0.319900 (50 iterations in 0.09 seconds)
    ## Iteration 400: error is 0.306396 (50 iterations in 0.10 seconds)
    ## Iteration 450: error is 0.302445 (50 iterations in 0.08 seconds)
    ## Iteration 500: error is 0.298800 (50 iterations in 0.09 seconds)
    ## Fitting performed in 0.99 seconds.

``` r
colors = rainbow(length(unique(data$Cluster)))
names(colors) = unique(data$Cluster)
plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2",  "cex.main"=2, "cex.lab"=1.5)
text(tsne$Y,col=colors[data$Cluster])
```

![alt text](https://github.com/donalbonny/donalbonny.github.io/blob/master/figures/2012-12-10-floral-development-2/unnamed-chunk-10-1.png)



If you want to build the 3d tSNE plot, use can use plot3d function:

``` r
#require(rgl)
#plot3d(tsne$Y, col=colors[data$Cluster])
#legend3d("topright", legend = '0':'5', pch = 16, col = rainbow(5))
```

``` r
library("ellipse")
```

    ## Warning: package 'ellipse' was built under R version 3.5.2

    ## 
    ## Attaching package: 'ellipse'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     pairs

``` r
featurePlot(x=x, y=y, plot="ellipse", auto.key = list(columns = 2))
```

![alt text](https://github.com/donalbonny/donalbonny.github.io/blob/master/figures/2012-12-10-floral-development-2/unnamed-chunk-12-1.png)


``` r
featurePlot(x = x, y = y, plot = "box", auto.key = list(columns = 3))
```

![alt text](https://github.com/donalbonny/donalbonny.github.io/blob/master/figures/2012-12-10-floral-development-2/unnamed-chunk-13-1.png)

``` r
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
```

![alt text](https://github.com/donalbonny/donalbonny.github.io/blob/master/figures/2012-12-10-floral-development-2/unnamed-chunk-14-1.png)

We will 10-fold crossvalidation to estimate accuracy and We are using the metric of "Accuracy" to evaluate models.

``` r
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
```

Classfication problem, given 6 different attributes
===================================================

Objective:predict the class (cluster) of any instance from the value of attributes.
-----------------------------------------------------------------------------------

Let's evaluate different algorithms:

Classification and Regression Trees (CART).

k-Nearest Neighbors (kNN).

Random Forest (RF)

``` r
# classification and decision tree 
set.seed(2018)
fit.cart <- train(Cluster~., data = data, method = "rpart", metric = metric, trControl = control)

#kNN k- Nearest Neighbors
set.seed(2018)
fit.knn <- train(Cluster~., data = data, method = "knn", metric = metric, trControl = control)

#random forest 
set.seed(2018)
fit.rf <- train(Cluster~., data= data, method="rf", metric=metric, trControl=control)
```

We can also plot the decission tree using rpart library

``` r
library(rpart.plot)
```

    ## Warning: package 'rpart.plot' was built under R version 3.5.2

    ## Loading required package: rpart

``` r
par(mar=c(0.5,2, 0.5, 0.5))
tree <- rpart(Cluster~., data=data)

rpart.plot(tree,box.palette="RdBu", shadow.col="gray", cex = 0.75, main = "Classification Tree")
```

![alt text](https://github.com/donalbonny/donalbonny.github.io/blob/master/figures/2012-12-10-floral-development-2/unnamed-chunk-17-1.png)

We can see the accuracy of each classifier and also other metrics like Kappa:

``` r
result <-  resamples(list( cart = fit.cart, knn = fit.knn, rf = fit.rf))
summary(result)
```

    ## 
    ## Call:
    ## summary.resamples(object = result)
    ## 
    ## Models: cart, knn, rf 
    ## Number of resamples: 10 
    ## 
    ## Accuracy 
    ##           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## cart 0.3773585 0.4018519 0.4309764 0.4592173 0.5092593 0.6000000    0
    ## knn  0.7169811 0.7685185 0.8148148 0.8029439 0.8318182 0.8909091    0
    ## rf   0.7818182 0.8472222 0.9065339 0.8933943 0.9405724 0.9636364    0
    ## 
    ## Kappa 
    ##           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## cart 0.1241863 0.1803997 0.2289681 0.2582818 0.3281200 0.4444444    0
    ## knn  0.6223278 0.6919604 0.7575703 0.7398256 0.7779241 0.8561465    0
    ## rf   0.7115385 0.7995922 0.8757615 0.8595107 0.9227128 0.9520488    0

Plot of the model evaluation results and compare the spread and the mean accuracy of each model.

``` r
dotplot(result)
```

![alt text](https://github.com/donalbonny/donalbonny.github.io/blob/master/figures/2012-12-10-floral-development-2/unnamed-chunk-19-1.png)

Random forest was the most accurate model. The result for random forest model can be summarised:

``` r
print(fit.rf)
```

    ## Random Forest 
    ## 
    ## 544 samples
    ##   6 predictor
    ##   5 classes: 'AG-specific', 'AP1-specific', 'AP1_AP3', 'AP3-specific', 'AP3_AG' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 489, 489, 489, 490, 491, 490, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##   2     0.8933943  0.8595107
    ##   4     0.8861889  0.8501204
    ##   6     0.8660180  0.8230971
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 2.

Now run the Random forest model directly on the validation set and summarize the results in a confusion matrix.

``` r
prediction <- predict(fit.rf, validation)
confusionMatrix(prediction, validation$Cluster)
```

    ## Confusion Matrix and Statistics
    ## 
    ##               Reference
    ## Prediction     AG-specific AP1-specific AP1_AP3 AP3-specific AP3_AG
    ##   AG-specific           35            3       0            0      3
    ##   AP1-specific           0           27       1            0      0
    ##   AP1_AP3                0            3      37            1      0
    ##   AP3-specific           0            0       2           11      2
    ##   AP3_AG                 0            0       0            1      8
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.8806          
    ##                  95% CI : (0.8133, 0.9302)
    ##     No Information Rate : 0.2985          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.8429          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: AG-specific Class: AP1-specific Class: AP1_AP3
    ## Sensitivity                      1.0000              0.8182         0.9250
    ## Specificity                      0.9394              0.9901         0.9574
    ## Pos Pred Value                   0.8537              0.9643         0.9024
    ## Neg Pred Value                   1.0000              0.9434         0.9677
    ## Prevalence                       0.2612              0.2463         0.2985
    ## Detection Rate                   0.2612              0.2015         0.2761
    ## Detection Prevalence             0.3060              0.2090         0.3060
    ## Balanced Accuracy                0.9697              0.9041         0.9412
    ##                      Class: AP3-specific Class: AP3_AG
    ## Sensitivity                      0.84615       0.61538
    ## Specificity                      0.96694       0.99174
    ## Pos Pred Value                   0.73333       0.88889
    ## Neg Pred Value                   0.98319       0.96000
    ## Prevalence                       0.09701       0.09701
    ## Detection Rate                   0.08209       0.05970
    ## Detection Prevalence             0.11194       0.06716
    ## Balanced Accuracy                0.90655       0.80356
