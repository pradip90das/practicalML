Predict Five Types of Classes
========================================================

## Summary
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 
In this analysis, we choose 18 variables to predict which exercise they do, 
and train using random forest method.
We find that the expected error rate is about 0.3% in test sample.
Therefore, we conclude that we can predict which exercise they do.


## Analysis

Settings for this analysis. Loading libraries & setting my functions.

checkClasification() takes two arguments, one is the label which outcome is, and 
the other is the label predicted by some models.
```{r fig.width=7, fig.height=6}
library(caret)

checkClasification <- function(values, prediction){
  sum( prediction != values )/length(values)
}
```

Loading raw data. 
We prepare 3 data: One is training dataset used for creating models.
Next we set cross-validation dataset to estimate error rate 
produced by our models.
The ratio between training & cross-validation dataset is 6:4.
And we also prepate testing dataset to predict which outcome is.
```{r,echo=TRUE}
raw_training <- read.csv("pml-training.csv");
testing  <- read.csv("pml-testing.csv");

set.seed(1)
inTrain <- createDataPartition(y=raw_training$classe,p=0.6, list=F)
training <- raw_training[inTrain, ]
cv       <- raw_training[-inTrain, ]

```

From the analysis, we adopt the following variables as explanatory variables:

```{r,echo=TRUE}
"new_window", "num_window", "roll_belt", "pitch_belt", "yaw_belt","total_accel_belt", "roll_arm", "pitch_arm", "yaw_arm","total_accel_arm","roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell","total_accel_dumbbell","roll_forearm", "pitch_forearm", "yaw_forearm","total_accel_forearm"
```


Using the explanatory variables, we create models by random forest.
```{r,echo=TRUE}

set.seed(1);

model <- train( classe ~ new_window+num_window+roll_belt+pitch_belt+yaw_belt+total_accel_belt+roll_arm+pitch_arm+yaw_arm++total_accel_arm+roll_dumbbell+pitch_dumbbell
+yaw_dumbbell+total_accel_dumbbell+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm,data=training, method="rf")

```

## Results
After creating our model, we check error rate both training and 
cross-validation dataset.
The error rate is approximately 0.3% in cross-validation dataset. 
```{r,echo=TRUE}

# error rates
# -- training dataset
checkClasification(training$classe, predict(model, training))
# -- cross-validation dataset
checkClasification(cv$classe, predict(model, cv))
```

Using our model, we predict labels in test samples.
The outputs are written in each files (total 20 files are created).
```{r}
# test sample
result <- predict(model, testing)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(result)
```



