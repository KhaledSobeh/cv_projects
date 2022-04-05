################################################
########## Final Project Data Science ##########
################################################
library(dplyr)
library(caret)
weather <- read.csv('train.csv')
head(weather)
str(weather)
##############################
weather <- select(weather,-SampleId)
change_stn <- function(x){
  if (x == 5){
    x <- 3
  }
  else if(x == 4){
    x <- 2
  }
  else{
    x <- 1
  }
}
weather$clc_bare_water <- as.factor(ifelse(weather$clc_water>0 & weather$clc_bare>0,1,0))
weather$stn_type <- as.factor(weather$stn_type)
weather$stn_type <- as.factor(sapply(weather$stn_type,change_stn))
weather$stn_elev_2000 <- as.factor(ifelse(weather$stn_elev>2000,1,0))
weather$climate_type <- as.factor(weather$climate_type)
wcss <- vector()
for(k in 1:15) wcss[k] <- sum(kmeans(select(weather,c(lat,lon)),k)$withinss)
plot(1:15,wcss,type = 'b', main = paste('Cluster clients'),
     xlab = 'Number of Clusters', ylab = 'WCSS')
km <- kmeans(select(weather,c(lat,lon)),centers = 6)
df <- data.frame(weather$lat,weather$lon,km$cluster)
weather$district <- as.factor(df$km.cluster)
weather$lg_aqua_night_lst <- log(weather$aqua_night_lst)
weather$lg_aqua_night_lst_binary <- as.factor(ifelse(weather$lg_aqua_night_lst == 'NaN',
                                                     0,1))
weather$lg_aqua_night_lst <- ifelse(weather$lg_aqua_night_lst == 'NaN',
                                    -10,weather$lg_aqua_night_lst)
newlab <- function(x){#for aqua night lst above
  if(x>10){
    x <- 2
  }
  else if(x>5){
    x <- 1
  }
  else{
    x <- 0
  }
}
weather$new1 <- as.factor(sapply(weather$aqua_night_lst,newlab))
weather$new2 <- as.factor(ifelse(weather$stn_elev>1200,1,0))
weather$new3 <- as.factor(ifelse(weather$clc_bare>0.05 & weather$clc_water>0.05,1,0))
weather$new4 <- as.factor(ifelse(weather$aqua_night_lst^3<0,1,0))
weather$new4 <- as.factor(ifelse(log(weather$aqua_night_lst)/weather$aqua_night_lst >0,1,0))
##########################################
## Removing Variables
##########################################
weather <- select(weather,-c(nearZeroVar(weather, names = TRUE)))
rm(km)
rm(df)
weather$district_climate <- as.factor((as.integer(weather$district) * as.integer(weather$climate_type)))
weather <- select(weather,-c(lat,lon))
weather <- select(weather,-stn_id)
weather <- select(weather,-elev)
##############################
##############################

##########################
### Splitting the Data ###
##########################
library(caTools)
set.seed(123)
split <- sample.split(weather$TempLabel, SplitRatio = 0.9)
training_set <- subset(weather, split == TRUE)
test_set <- subset(weather, split == FALSE)



###################
##### XGBoost #####
###################



# Fitting XGBoost to the Training set
# install.packages('xgboost')
library(xgboost)
classifier <- xgboost(data = as.matrix(training_set[-16]),
                     label = training_set$TempLabel, nrounds = 300)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-16]))
lastfun <- function(x){
  if(x<1){
    x <- 1
  }
  else if(x<2){
    x <- 2
  }
  else if(x<3){
    x <- 3
  }
  else if(x<4){
    x <- 4
  }
  else{
    x <- 5
  }
}
y_pred <- as.numeric(sapply(y_pred,lastfun))
# Making the Confusion Matrix
cm = table(test_set[, 16], y_pred)
cat('test Accuracy =',sum(diag(cm)) / sum(cm))

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$TempLabel, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_fold[-17]),
                       label = training_fold$TempLabel, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-17]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 17], y_pred)
  accuracy = (sum(diag(cm)) / sum(cm))
  return(accuracy)
})
(accuracy = mean(as.numeric(cv)))



##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
train <- weather

# Create a training and validation sets
trainObs <- sample(nrow(train), .8 * nrow(train), replace = FALSE)
valObs <- sample(nrow(train), .2 * nrow(train), replace = FALSE)

train_dat <- train[trainObs,]
val_dat <- train[valObs,]

# Create numeric labels with one-hot encoding
train_labs <- as.numeric(train_dat$TempLabel) - 1
val_labs <- as.numeric(val_dat$TempLabel) - 1

new_train <- model.matrix(~ . + 0, data = train_dat[, -16])
new_val <- model.matrix(~ . + 0, data = train[valObs, -16])

# Prepare matrices
xgb_train <- xgb.DMatrix(data = new_train, label = train_labs)
xgb_val <- xgb.DMatrix(data = new_val, label = val_labs)

##########

# Set parameters(default)
params <- list(booster = "gbtree", objective = "multi:softprob",
               num_class = 3, eval_metric = "mlogloss")

# Calculate # of folds for cross-validation
xgbcv <- xgb.cv(params = params, data = xgb_train, nrounds = 100, nfold = 5,
                showsd = TRUE, stratified = TRUE, print_every_n = 10,
                early_stop_round = 20, maximize = FALSE, prediction = TRUE)



#######################################################
################## Random Forest ######################
#######################################################
ranForest <- randomForest(TempLabel ~. ,data = training_set) # train
predictions <- predict(ranForest, newdata=test_set,type = 'class')
(cm <- table(predictions,test_set$TempLabel))
(accuracy_test <- (sum(diag(cm)) / sum(cm)))

for(i in seq(500,700,50)){
  for(j in seq(3,9,3)){
    ranForest <- randomForest(TempLabel ~. ,data = training_set) # train
    predictions <- predict(ranForest, newdata=test_set,type = 'class')
    (cm <- table(predictions,test_set$TempLabel))
    (accuracy_test <- (sum(diag(cm)) / sum(cm)))
    cat(j,i,accuracy_test)
    cat('\n')
    beepr::beep(10)
  }
}


#####################################################
################## Neural Nets ######################
#####################################################
weather <- read.csv('train.csv')
head(weather)
str(weather)
##############################

weather <- select(weather,-SampleId)
#weather <- select(weather,-date)
weather[2:16] <- data.frame(sapply(weather[2:16], function(x) as.numeric(as.character(x))))
target <- function(targ){
  if(targ=="A"){
    tres <- 1
  }
  else if(targ=="B"){
    tres <- 2
  }
  else if(targ=="C"){
    tres <- 3
  }
  else if(targ=="D"){
    tres <- 4
  }
  else{
    tres <- 5
  }
}
weather$TempLabel <- as.factor(sapply(weather$TempLabel,target))
##############################

###Normalize the data
normalize <- function(x) {
  return ((x - mean(x)) / (sd(x)))
}
weather$stn_elev <- normalize(weather$stn_elev)
weather$aqua_night_lst <- normalize(weather$aqua_night_lst)
weather$aqua_emis <- normalize(weather$aqua_emis)
weather$aqua_ndvi <- normalize(weather$aqua_ndvi)
weather$elev <- normalize(weather$elev)
weather$pop <- normalize(weather$pop)
weather$clc_artificial <- normalize(weather$clc_artificial)
weather$clc_vegetation <- normalize(weather$clc_vegetation)




weather <- select(weather,-stn_id)

library(caTools)
set.seed(99)
split <- sample.split(weather$TempLabel ,SplitRatio = 0.7)
weather_train <- subset(weather,split == T)
weather_test <- subset(weather,split == F)
###########################
library(nnet)
library(NeuralNetTools)
library(dplyr)

##
# ,MaxNWts = 30000
nn <- nnet(TempLabel~.,data = weather_train,
           size = 15,MaxNWts = 3500,maxit = 150)
library(NeuralNetTools)
plotnet(nn)
#
#library(gamlss.add)
#plot(nn)

# Train confusion matrix & accuracy
pred_train <- predict(nn, type='class') 
(cm_train <- table(prediction=pred_train, truth=weather_train$TempLabel))
(accuracy_train <- (sum(diag(cm_train)) / sum(cm_train)))

# Test confusion matrix & accuracy
pred_test <- predict(nn, newdata = select(weather_test,-TempLabel), type='class') 
(cm_test <- table(prediction=pred_test, truth=weather_test$TempLabel))
(accuracy_test <- (sum(diag(cm_test)) / sum(cm_test)))

#######################
##### cv & tuning #####
#######################
library(caret)
folds <- createFolds(weather_train$TempLabel, k = 10)
# size
final_pred_test <- c()
final_pred_train <- c()
Size <-  c(1:15)
for(i in Size){
  test_pred <- c()
  train_pred <- c()
  for(x in folds){
    training_fold <- weather_train[-x, ]
    test_fold <- weather_train[x, ]
    classifier <- nnet(Y~.,data = weather_train, size = i)
    y_pred_test <- predict(classifier, newdata = test_fold[,-14], type = 'class')
    cm_test <- table(test_fold[,14], y_pred_test)
    accuracy_test <- sum(diag(cm_test)) / sum(cm_test)
    y_pred_train <- predict(classifier, newdata = training_fold[,-14], type = 'class')
    cm_train <- table(training_fold[,14], y_pred_train)
    accuracy_train <- sum(diag(cm_train)) / sum(cm_train)
    test_pred <- c(test_pred,accuracy_test)
    train_pred <- c(train_pred,accuracy_train)
  }
  tst <- mean(test_pred)
  trn <- mean(train_pred)
  final_pred_train <- c(final_pred_train,trn)
  final_pred_test <- c(final_pred_test,tst)
}
(df_nn_size <- data.frame(Size,final_pred_train,final_pred_test))













proximity=T

ranForest <- randomForest(TempLabel ~. ,data = weather_train,ntree = 500,mtry = 15) # train
predictions <- predict(ranForest, newdata=weather_test)
cm <- table(predictions,weather_test$TempLabel)
(accuracy_test <- (sum(diag(cm)) / sum(cm)))

for (x in folds){
  cross.train <- t[-x,] # train subset
  cross.validation <-  dtset[x,] # test subset
  ranForest <- randomForest(price ~. ,data = cross.train) # train
  predictions <- predict(ranForest, newdata=cross.validation)
  rmsecol <- c(rmsecol,RMSE(predictions, cross.validation$price))
}







######################################
#G
######################################
tunegrid <- expand.grid(nrounds = 50,
                        max_depth = 2:4,
                        eta = seq(0.05,0.6,length.out = 20),
                        gamma = 0,
                        colsample_bytree = 1,
                        min_child_weight = 1,
                        subsample = 1)
trcontrol <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 3, 
                          search = "grid") 



xgb_train = caret::train(TempLabel ~ ., 
                         data=weather_train,
                         trControl = trcontrol,
                         tuneGrid = tunegrid,
                         method = "xgbTree")
plot(xgb_train)
pred <- predict(xgb_train, weather_test)
cm <- table(pred,weather_test$TempLabel)
(accuracy_test <- (sum(diag(cm)) / sum(cm)))


#########
## SVM ##
#########
library(e1071)
svm_model <- svm(TempLabel~.,data = training_set,kernel = 'radial')
predictions <- predict(svm_model, newdata=test_set)
(cm <- table(predictions,test_set$TempLabel))
(accuracy_test <- (sum(diag(cm)) / sum(cm)))

predictions <- predict(svm_model, newdata=training_set)
(cm <- table(predictions,training_set$TempLabel))
(accuracy_test <- (sum(diag(cm)) / sum(cm)))






nb.1 <- naiveBayes(TempLabel~., data = training_set)
predictions <- predict(nb.1, newdata=test_set)
(cm <- table(predictions,test_set$TempLabel))
(accuracy_test <- (sum(diag(cm)) / sum(cm)))

predictions <- predict(nb.1, newdata=training_set)
(cm <- table(predictions,training_set$TempLabel))
(accuracy_test <- (sum(diag(cm)) / sum(cm)))



