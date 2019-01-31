``` r
data <- read.csv("breast_cancer.csv", header = TRUE)
colnames(data)
```

    ##  [1] "id"                      "diagnosis"              
    ##  [3] "radius_mean"             "texture_mean"           
    ##  [5] "perimeter_mean"          "area_mean"              
    ##  [7] "smoothness_mean"         "compactness_mean"       
    ##  [9] "concavity_mean"          "concave.points_mean"    
    ## [11] "symmetry_mean"           "fractal_dimension_mean" 
    ## [13] "radius_se"               "texture_se"             
    ## [15] "perimeter_se"            "area_se"                
    ## [17] "smoothness_se"           "compactness_se"         
    ## [19] "concavity_se"            "concave.points_se"      
    ## [21] "symmetry_se"             "fractal_dimension_se"   
    ## [23] "radius_worst"            "texture_worst"          
    ## [25] "perimeter_worst"         "area_worst"             
    ## [27] "smoothness_worst"        "compactness_worst"      
    ## [29] "concavity_worst"         "concave.points_worst"   
    ## [31] "symmetry_worst"          "fractal_dimension_worst"
    ## [33] "X"

``` r
# remove the last column with only NA value 

data <- data[,-33] 
dim(data)
```

    ## [1] 569  32

``` r
str(data)
```

    ## 'data.frame':    569 obs. of  32 variables:
    ##  $ id                     : int  842302 842517 84300903 84348301 84358402 843786 844359 84458202 844981 84501001 ...
    ##  $ diagnosis              : Factor w/ 2 levels "B","M": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ radius_mean            : num  18 20.6 19.7 11.4 20.3 ...
    ##  $ texture_mean           : num  10.4 17.8 21.2 20.4 14.3 ...
    ##  $ perimeter_mean         : num  122.8 132.9 130 77.6 135.1 ...
    ##  $ area_mean              : num  1001 1326 1203 386 1297 ...
    ##  $ smoothness_mean        : num  0.1184 0.0847 0.1096 0.1425 0.1003 ...
    ##  $ compactness_mean       : num  0.2776 0.0786 0.1599 0.2839 0.1328 ...
    ##  $ concavity_mean         : num  0.3001 0.0869 0.1974 0.2414 0.198 ...
    ##  $ concave.points_mean    : num  0.1471 0.0702 0.1279 0.1052 0.1043 ...
    ##  $ symmetry_mean          : num  0.242 0.181 0.207 0.26 0.181 ...
    ##  $ fractal_dimension_mean : num  0.0787 0.0567 0.06 0.0974 0.0588 ...
    ##  $ radius_se              : num  1.095 0.543 0.746 0.496 0.757 ...
    ##  $ texture_se             : num  0.905 0.734 0.787 1.156 0.781 ...
    ##  $ perimeter_se           : num  8.59 3.4 4.58 3.44 5.44 ...
    ##  $ area_se                : num  153.4 74.1 94 27.2 94.4 ...
    ##  $ smoothness_se          : num  0.0064 0.00522 0.00615 0.00911 0.01149 ...
    ##  $ compactness_se         : num  0.049 0.0131 0.0401 0.0746 0.0246 ...
    ##  $ concavity_se           : num  0.0537 0.0186 0.0383 0.0566 0.0569 ...
    ##  $ concave.points_se      : num  0.0159 0.0134 0.0206 0.0187 0.0188 ...
    ##  $ symmetry_se            : num  0.03 0.0139 0.0225 0.0596 0.0176 ...
    ##  $ fractal_dimension_se   : num  0.00619 0.00353 0.00457 0.00921 0.00511 ...
    ##  $ radius_worst           : num  25.4 25 23.6 14.9 22.5 ...
    ##  $ texture_worst          : num  17.3 23.4 25.5 26.5 16.7 ...
    ##  $ perimeter_worst        : num  184.6 158.8 152.5 98.9 152.2 ...
    ##  $ area_worst             : num  2019 1956 1709 568 1575 ...
    ##  $ smoothness_worst       : num  0.162 0.124 0.144 0.21 0.137 ...
    ##  $ compactness_worst      : num  0.666 0.187 0.424 0.866 0.205 ...
    ##  $ concavity_worst        : num  0.712 0.242 0.45 0.687 0.4 ...
    ##  $ concave.points_worst   : num  0.265 0.186 0.243 0.258 0.163 ...
    ##  $ symmetry_worst         : num  0.46 0.275 0.361 0.664 0.236 ...
    ##  $ fractal_dimension_worst: num  0.1189 0.089 0.0876 0.173 0.0768 ...

``` r
data$diagnosis = as.factor(data$diagnosis)
sapply(data, class)
```

    ##                      id               diagnosis             radius_mean 
    ##               "integer"                "factor"               "numeric" 
    ##            texture_mean          perimeter_mean               area_mean 
    ##               "numeric"               "numeric"               "numeric" 
    ##         smoothness_mean        compactness_mean          concavity_mean 
    ##               "numeric"               "numeric"               "numeric" 
    ##     concave.points_mean           symmetry_mean  fractal_dimension_mean 
    ##               "numeric"               "numeric"               "numeric" 
    ##               radius_se              texture_se            perimeter_se 
    ##               "numeric"               "numeric"               "numeric" 
    ##                 area_se           smoothness_se          compactness_se 
    ##               "numeric"               "numeric"               "numeric" 
    ##            concavity_se       concave.points_se             symmetry_se 
    ##               "numeric"               "numeric"               "numeric" 
    ##    fractal_dimension_se            radius_worst           texture_worst 
    ##               "numeric"               "numeric"               "numeric" 
    ##         perimeter_worst              area_worst        smoothness_worst 
    ##               "numeric"               "numeric"               "numeric" 
    ##       compactness_worst         concavity_worst    concave.points_worst 
    ##               "numeric"               "numeric"               "numeric" 
    ##          symmetry_worst fractal_dimension_worst 
    ##               "numeric"               "numeric"

``` r
#check missing value 

cat("Number of missing value:", sum(is.na(data)), "\n")
```

    ## Number of missing value: 0

``` r
summary(data)
```

    ##        id            diagnosis  radius_mean      texture_mean  
    ##  Min.   :     8670   B:357     Min.   : 6.981   Min.   : 9.71  
    ##  1st Qu.:   869218   M:212     1st Qu.:11.700   1st Qu.:16.17  
    ##  Median :   906024             Median :13.370   Median :18.84  
    ##  Mean   : 30371831             Mean   :14.127   Mean   :19.29  
    ##  3rd Qu.:  8813129             3rd Qu.:15.780   3rd Qu.:21.80  
    ##  Max.   :911320502             Max.   :28.110   Max.   :39.28  
    ##  perimeter_mean     area_mean      smoothness_mean   compactness_mean 
    ##  Min.   : 43.79   Min.   : 143.5   Min.   :0.05263   Min.   :0.01938  
    ##  1st Qu.: 75.17   1st Qu.: 420.3   1st Qu.:0.08637   1st Qu.:0.06492  
    ##  Median : 86.24   Median : 551.1   Median :0.09587   Median :0.09263  
    ##  Mean   : 91.97   Mean   : 654.9   Mean   :0.09636   Mean   :0.10434  
    ##  3rd Qu.:104.10   3rd Qu.: 782.7   3rd Qu.:0.10530   3rd Qu.:0.13040  
    ##  Max.   :188.50   Max.   :2501.0   Max.   :0.16340   Max.   :0.34540  
    ##  concavity_mean    concave.points_mean symmetry_mean   
    ##  Min.   :0.00000   Min.   :0.00000     Min.   :0.1060  
    ##  1st Qu.:0.02956   1st Qu.:0.02031     1st Qu.:0.1619  
    ##  Median :0.06154   Median :0.03350     Median :0.1792  
    ##  Mean   :0.08880   Mean   :0.04892     Mean   :0.1812  
    ##  3rd Qu.:0.13070   3rd Qu.:0.07400     3rd Qu.:0.1957  
    ##  Max.   :0.42680   Max.   :0.20120     Max.   :0.3040  
    ##  fractal_dimension_mean   radius_se        texture_se      perimeter_se   
    ##  Min.   :0.04996        Min.   :0.1115   Min.   :0.3602   Min.   : 0.757  
    ##  1st Qu.:0.05770        1st Qu.:0.2324   1st Qu.:0.8339   1st Qu.: 1.606  
    ##  Median :0.06154        Median :0.3242   Median :1.1080   Median : 2.287  
    ##  Mean   :0.06280        Mean   :0.4052   Mean   :1.2169   Mean   : 2.866  
    ##  3rd Qu.:0.06612        3rd Qu.:0.4789   3rd Qu.:1.4740   3rd Qu.: 3.357  
    ##  Max.   :0.09744        Max.   :2.8730   Max.   :4.8850   Max.   :21.980  
    ##     area_se        smoothness_se      compactness_se      concavity_se    
    ##  Min.   :  6.802   Min.   :0.001713   Min.   :0.002252   Min.   :0.00000  
    ##  1st Qu.: 17.850   1st Qu.:0.005169   1st Qu.:0.013080   1st Qu.:0.01509  
    ##  Median : 24.530   Median :0.006380   Median :0.020450   Median :0.02589  
    ##  Mean   : 40.337   Mean   :0.007041   Mean   :0.025478   Mean   :0.03189  
    ##  3rd Qu.: 45.190   3rd Qu.:0.008146   3rd Qu.:0.032450   3rd Qu.:0.04205  
    ##  Max.   :542.200   Max.   :0.031130   Max.   :0.135400   Max.   :0.39600  
    ##  concave.points_se   symmetry_se       fractal_dimension_se
    ##  Min.   :0.000000   Min.   :0.007882   Min.   :0.0008948   
    ##  1st Qu.:0.007638   1st Qu.:0.015160   1st Qu.:0.0022480   
    ##  Median :0.010930   Median :0.018730   Median :0.0031870   
    ##  Mean   :0.011796   Mean   :0.020542   Mean   :0.0037949   
    ##  3rd Qu.:0.014710   3rd Qu.:0.023480   3rd Qu.:0.0045580   
    ##  Max.   :0.052790   Max.   :0.078950   Max.   :0.0298400   
    ##   radius_worst   texture_worst   perimeter_worst    area_worst    
    ##  Min.   : 7.93   Min.   :12.02   Min.   : 50.41   Min.   : 185.2  
    ##  1st Qu.:13.01   1st Qu.:21.08   1st Qu.: 84.11   1st Qu.: 515.3  
    ##  Median :14.97   Median :25.41   Median : 97.66   Median : 686.5  
    ##  Mean   :16.27   Mean   :25.68   Mean   :107.26   Mean   : 880.6  
    ##  3rd Qu.:18.79   3rd Qu.:29.72   3rd Qu.:125.40   3rd Qu.:1084.0  
    ##  Max.   :36.04   Max.   :49.54   Max.   :251.20   Max.   :4254.0  
    ##  smoothness_worst  compactness_worst concavity_worst  concave.points_worst
    ##  Min.   :0.07117   Min.   :0.02729   Min.   :0.0000   Min.   :0.00000     
    ##  1st Qu.:0.11660   1st Qu.:0.14720   1st Qu.:0.1145   1st Qu.:0.06493     
    ##  Median :0.13130   Median :0.21190   Median :0.2267   Median :0.09993     
    ##  Mean   :0.13237   Mean   :0.25427   Mean   :0.2722   Mean   :0.11461     
    ##  3rd Qu.:0.14600   3rd Qu.:0.33910   3rd Qu.:0.3829   3rd Qu.:0.16140     
    ##  Max.   :0.22260   Max.   :1.05800   Max.   :1.2520   Max.   :0.29100     
    ##  symmetry_worst   fractal_dimension_worst
    ##  Min.   :0.1565   Min.   :0.05504        
    ##  1st Qu.:0.2504   1st Qu.:0.07146        
    ##  Median :0.2822   Median :0.08004        
    ##  Mean   :0.2901   Mean   :0.08395        
    ##  3rd Qu.:0.3179   3rd Qu.:0.09208        
    ##  Max.   :0.6638   Max.   :0.20750

B: 357 and M: 212

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 3.5.2

    ## Loading required package: lattice

``` r
validation_index <- createDataPartition(data$diagnosis, p = 0.8, list = FALSE)
validation <- data[-validation_index,]
data <- data[validation_index,]
dim(data)
```

    ## [1] 456  32

``` r
percentage <- prop.table(table(data$diagnosis)) * 100
cbind(freq= table(data$diagnosis), percentage = percentage)
```

    ##   freq percentage
    ## B  286    62.7193
    ## M  170    37.2807

``` r
x = data[,3:32]
y = data[,2]
```

``` r
par(mfrow=c(2,3))
  for(i in 1:6) {
  boxplot(x[,i], main=names(x)[i])
  }
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
par(mfrow=c(2,3))
  for(i in 7:12) {
  boxplot(x[,i], main=names(x)[i])
  }
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-11-2.png)

``` r
par(mfrow=c(2,3))
  for(i in 13:18) {
  boxplot(x[,i], main=names(x)[i])
  }
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-11-3.png)

``` r
par(mfrow=c(2,3))
  for(i in 19:24) {
  boxplot(x[,i], main=names(x)[i])
}
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-11-4.png)

``` r
par(mfrow=c(2,3))
  for(i in 25:30) {
  boxplot(x[,i], main=names(x)[i])
  }
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-11-5.png)

``` r
p1 <- ggplot(data, aes(x=concavity_se)) + ggtitle("Concavity SE") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 0.1, colour="black", fill="white") + ylab("Percentage")
p2 <- ggplot(data, aes(x= texture_se)) + ggtitle("Texture") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="white") + ylab("Percentage")
p3 <- ggplot(data, aes(x= radius_se)) + ggtitle("Radius") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="white") + ylab("Percentage")
grid.arrange(p1, p2, p3, ncol=3)
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-12-1.png)

Let's check the correlation between different features

``` r
features <- data[, 3:ncol(data)]
correlations <- cor(features,method="pearson")
corrplot(correlations, number.cex = .9, method = "square", 
         hclust.method = "ward", order = "FPC",
         type = "full", tl.cex=0.8,tl.col = "black")
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
attach(data)
#Correlation bewteen numeric variables and diagnosis
# to check if all the variables have significant correlation with diagnosis 


par(mfrow=c(1,3))

boxplot(data$concavity_mean~ data$diagnosis, main=" concave.point_mean vs Breast Cancer", xlab="Diagnosis", ylab=" Concave Point Mean")


boxplot(data$radius_mean~ data$diagnosis, main=" Radius Mean vs Breast Cancer", xlab="Diagnosis", ylab=" Radius Mean")


boxplot(data$texture_mean~ data$diagnosis, main=" Texture Mean vs Breast Cancer", xlab="Diagnosis", ylab=" Texture Mean")
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-14-1.png)

PCA analysis
============

``` r
ggplot(data, aes(x=concavity_mean, y= texture_mean, color=diagnosis)) + geom_point()
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
data %>% select(-diagnosis) %>% # remove diagnosis column
  scale() %>%                 # scale to 0 mean and unit variance
  prcomp() ->                 # do PCA
  pca 
```

``` r
pca_data <- data.frame(pca$x, Diagnosis = data$diagnosis)
head(pca_data)
```

    ##        PC1       PC2        PC3       PC4        PC5        PC6        PC7
    ## 1 9.115839  2.064962 -1.1782097 3.5380257 -1.1214021  1.3048575  0.2033826
    ## 2 2.341406 -3.726077 -1.0681076 0.9712571  0.5309161  0.2687526 -0.3097704
    ## 3 5.714380 -1.065654 -0.6845960 0.7934706 -0.2313693  0.3774271  0.5164584
    ## 4 7.179322 10.621996 -1.8933315 0.2622966 -3.2497068  1.8030487  1.8911524
    ## 5 4.004403 -2.088725  0.9861685 2.8917343  1.2162045 -0.9192811 -0.2480596
    ## 6 2.390861  4.189152 -2.4363876 0.9325637 -1.1695552 -0.7052096 -0.1737447
    ##          PC8        PC9       PC10        PC11        PC12        PC13
    ## 1  2.1139561  0.7199923  0.2263821  0.59422621 -0.62604678 -0.12315093
    ## 2  0.1641172 -0.7326402 -1.1230228 -0.59101251 -0.58934672 -0.49827865
    ## 3 -0.7286687 -0.0952603 -0.3230065 -0.50379467  0.43706319 -0.60582481
    ## 4  1.4238883 -0.7987169 -0.5575961  1.39639993  1.25041618 -1.48039938
    ## 5 -0.8796049 -0.6601505 -0.4327235 -0.05032569 -0.38291190  0.57054389
    ## 6  0.4651375  0.2331364  0.1121080  0.51239571 -0.01798278 -0.03988549
    ##           PC14        PC15        PC16        PC17        PC18        PC19
    ## 1  0.525208102  0.85668414  0.59171724 -0.64513100 -0.43037715 -0.51041697
    ## 2 -0.937862650  0.54509340  0.04810661  0.60468815  0.05935079  0.36306127
    ## 3  0.003050953 -0.03215007 -0.46590215 -0.33232380  0.17581082 -0.03248821
    ## 4 -0.109254591  0.37774084  0.20901099  0.04609371  0.48251228 -0.13765711
    ## 5 -0.183980608  0.59434366 -0.33236834  0.15566635  0.11592559 -0.20275479
    ## 6 -0.081189990 -0.01912489 -0.18782495  0.03052365  0.13488076 -0.32513778
    ##          PC20        PC21        PC22         PC23        PC24        PC25
    ## 1 -0.02994153 -0.15179584 -0.16314992 -0.246643745  0.07596138  0.21121935
    ## 2 -0.11592847  0.03304937  0.12718217  0.151226843  0.20504321  0.03654961
    ## 3 -0.37300830  0.39875373  0.01647927 -0.044180415  0.06019891 -0.21236017
    ## 4 -0.06720459  0.68328268  0.17862247 -0.007350404 -0.01190415 -0.14719448
    ## 5  0.41123609 -0.12406087  0.05628252 -0.048736753 -0.11543121  0.06654796
    ## 6 -0.25113580  0.06665393  0.15990460  0.024777202 -0.03807289 -0.02503467
    ##          PC26         PC27        PC28        PC29         PC30
    ## 1 -0.06254843  0.156275210  0.23828543  0.01925028  0.049691882
    ## 2 -0.11983515  0.045342125 -0.18136546 -0.03638247 -0.007331333
    ## 3  0.13858994  0.006604708 -0.03895420 -0.03780720 -0.001123020
    ## 4 -0.08785067  0.231722992 -0.17308447 -0.03604625 -0.081201574
    ## 5  0.06107367 -0.023019098 -0.02511523  0.03035024  0.012946576
    ## 6  0.05103204  0.048107722  0.07456597  0.00796659 -0.020146166
    ##           PC31 Diagnosis
    ## 1 -0.040658610         M
    ## 2 -0.003908805         M
    ## 3  0.001202539         M
    ## 4 -0.024559169         M
    ## 5  0.019107941         M
    ## 6  0.003960868         M

``` r
ggplot(pca_data, aes(x=PC1, y=PC2, color=Diagnosis)) + geom_point()
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-18-1.png)

look at the rotation data

``` r
pc1 <- ggplot(pca_data, aes(x=PC1, fill=data$diagnosis)) + geom_density(alpha=0.25)  
pc2 <- ggplot(pca_data, aes(x=PC2, fill=data$diagnosis)) + geom_density(alpha=0.25)  
grid.arrange(pc1, pc2, ncol=2)
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
rotation_data <- data.frame(pca$rotation, variable=row.names(pca$rotation))
```

``` r
# capture the rotation matrix in a data frame
rotation_data <- data.frame(pca$rotation, variable=row.names(pca$rotation))
# define a pleasing arrow style
arrow_style <- arrow(length = unit(0.05, "inches"),
                     type = "closed")
# now plot, using geom_segment() for arrows and geom_text for labels
ggplot(rotation_data) + 
  geom_segment(aes(xend=PC1, yend=PC2), x=0, y=0, arrow=arrow_style) + 
  geom_text(aes(x=PC1, y=PC2, label=variable), hjust=0, size=3, color='red') + 
  xlim(-0.5, 0.5) + 
  ylim(-0.5, 0.5) +
  coord_fixed() # fix aspect ratio to 1:1
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-21-1.png)

Model selection
===============

Since the outcome varianle is diagnosis including Benign or Malign so We will use classification algorithm of supervised learning for this dataset

Different types of classification algorithms in Machine Learning :

1.  Logistic Regression

2.  Nearest Neighbor

3.  Support Vector Machines

4.  Kernel SVM

5.  Naïve Bayes

6.  Decision Tree Algorithm

7.  Random Forest Classification

We will use Classification Accuracy method to find the accuracy of our models. Classification Accuracy is what we usually mean, when we use the term accuracy. It is the ratio of number of correct predictions to the total number of input samples.

``` r
control <- trainControl(method="cv", number=10, classProbs = TRUE) #10-fold crossvalidation to estimate accuracy
metric <- "Accuracy"
```

``` r
#random forest
set.seed(2018)
fit.rf <- train(diagnosis~., data= data, method="rf", metric=metric, trControl=control)

#kNN
set.seed(2018)
fit.knn <- train(diagnosis~., data = data, method = "knn", metric = metric, trControl = control)

#SVM 
set.seed(2018)
fit.svm <- train(diagnosis~., data = data, method = "svmRadial", metric = metric, trControl = control)

# Decision Tree
set.seed(2018)
fit.rpart <- train(diagnosis~., data = data, method = "rpart", metric = metric, trControl = control)
```

``` r
result <-  resamples(list(rpart = fit.rpart, svm = fit.svm, knn = fit.knn, rf = fit.rf))
summary(result)
```

    ## 
    ## Call:
    ## summary.resamples(object = result)
    ## 
    ## Models: rpart, svm, knn, rf 
    ## Number of resamples: 10 
    ## 
    ## Accuracy 
    ##            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## rpart 0.8478261 0.8962560 0.9239130 0.9189372 0.9503623 0.9777778    0
    ## svm   0.8888889 0.9619565 0.9782609 0.9713527 1.0000000 1.0000000    0
    ## knn   0.6222222 0.6887681 0.7500000 0.7390338 0.7814010 0.8260870    0
    ## rf    0.8913043 0.9184783 0.9666667 0.9518357 0.9782609 1.0000000    0
    ## 
    ## Kappa 
    ##            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## rpart 0.6694045 0.7745082 0.8375554 0.8241201 0.8937053 0.9521785    0
    ## svm   0.7608927 0.9182133 0.9533399 0.9386851 1.0000000 1.0000000    0
    ## knn   0.1257143 0.2712188 0.4063902 0.3965470 0.4923300 0.6076759    0
    ## rf    0.7638604 0.8279454 0.9288204 0.8958853 0.9527721 1.0000000    0

``` r
prediction <- predict(fit.rf, validation)
confusionMatrix(prediction, validation$diagnosis)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  B  M
    ##          B 71  1
    ##          M  0 41
    ##                                           
    ##                Accuracy : 0.9912          
    ##                  95% CI : (0.9517, 0.9998)
    ##     No Information Rate : 0.6283          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.981           
    ##  Mcnemar's Test P-Value : 1               
    ##                                           
    ##             Sensitivity : 1.0000          
    ##             Specificity : 0.9762          
    ##          Pos Pred Value : 0.9861          
    ##          Neg Pred Value : 1.0000          
    ##              Prevalence : 0.6283          
    ##          Detection Rate : 0.6283          
    ##    Detection Prevalence : 0.6372          
    ##       Balanced Accuracy : 0.9881          
    ##                                           
    ##        'Positive' Class : B               
    ## 

``` r
dotplot(result)
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
bwplot(result, metric = "Accuracy")
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-26-2.png) The results show that Random forest gives the best result for our dataset

Logistic regression using glm function
======================================

``` r
glm <-  glm(diagnosis ~.,
               data = data,
               family = binomial)
```

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
lr_data <- data.frame(predictor=glm$linear.predictors, prob=glm$fitted.values, Diagnosis = data$diagnosis)
```

To check how the two diagnosis outcomes are separated by the linear predictor.

``` r
ggplot(lr_data, aes(x=predictor, fill=Diagnosis)) + 
  geom_density(alpha=.5) +
  scale_fill_colorblind()
```

![](/donalbonny.github.io/figures/cancer_files/figure-markdown_github/unnamed-chunk-28-1.png)
