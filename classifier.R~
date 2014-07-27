# load carret library
library(caret)

raw_training <- read.csv("pml-training.csv");
testing  <- read.csv("pml-testing.csv");

checkClasification <- function(values, prediction){
  sum( prediction != values )/length(values)
}


# set the seed to 1
set.seed(1)
# devide the training samples into training and CV.
Train <- createDataPartition(y=raw_training$classe,p=0.6, list=F)
training <- raw_training[Train, ]
cv <- raw_training[-Train, ]


set.seed(1);
#traing the model with relevent feature set
model <- train( classe ~ new_window+num_window+roll_belt+pitch_belt+yaw_belt+total_accel_belt+roll_arm+pitch_arm+yaw_arm++total_accel_arm+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+total_accel_dumbbell+roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm,data=training, method="rf")

checkClasification(training$classe, predict(modFit, training))
checkClasification(cv$classe, predict(modFit, cv))


# run the model on the test sample
result <- predict(model, testing)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
# call the function to save the classification result in to file.
pml_write_files(result)

