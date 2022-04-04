#######################################################
########## Machine Learning Project - Part 2 ##########
#######################################################
library(ggplot2)
library(dplyr)

## Please put all the datasets in the same folder(Set As Working Directory) ##
#Uploading the dataset:

getSet <- function(Heart){
  ##################################
  ##### Working with NA Values #####
  ##################################
  
  #make missing values as NA:
  Heart$age <- ifelse(Heart$age > 100,NA,Heart$age) #age column:
  Heart$ca <- ifelse(Heart$ca == 4,NA,Heart$ca) #ca column:
  Heart$thal <- ifelse(Heart$thal == 0,NA,Heart$thal) #thal column:
  
  #Duplicated rows:
  which(duplicated(Heart) | duplicated(Heart[nrow(Heart):1, ])[nrow(Heart):1])
  
  #Missing data in specific row:
  row.missing <- function(Column){
    MissingValues <- c()
    for(i in 1:length(Column)){
      if(is.na(Column[i])){
        MissingValues <- c(MissingValues,i)
      }
    }
    print(MissingValues)
  }
  
  Heart$gender = as.factor(Heart$gender)
  Heart$fbs = as.factor(Heart$fbs)
  Heart$exang = as.factor(Heart$exang)
  Heart$cp = as.factor(Heart$cp)
  Heart$slope = as.factor(Heart$slope)
  Heart$ca = as.factor(Heart$ca)
  Heart$thal = as.factor(Heart$thal)
  Heart$restecg = as.factor(Heart$restecg)

  #Heart <- Heart %>% rename(Y = y) #change target name(To make it easy:
  #y is y axis,Y is the Target variable)
  #Heart$Y <- as.factor(Heart$Y) #change Y to factor 
  
  #####################################
  ##### Working with missing data #####
  #####################################
  
  #Dropping id column:
  Heart <- select(Heart,-id)
  
  #Dropping rows with NA columns:
  if(length(Heart) > 13){
    Heart <- Heart %>% filter(!is.na(ca))
    Heart <- Heart %>% filter(!is.na(thal))
  }
  
  # Fill NA values in age column:
  ageMed <- as.data.frame(Heart %>% group_by(ca) %>% summarise(MED = median(age,na.rm = T)))
  for(i in 1:length(Heart$age)){
    if(is.na(Heart[i,1])){#just for missing values
      Heart[i,1] <- ageMed[as.integer(Heart[i,12])+1,2] #change missing value by age-ca median.
    }
  }
  
  
  ##########################
  ##### Discretization #####
  ##########################
  
  # BloodPressureRange
  BloodPressure <- function(tres){
    if(tres<120){
      tres <- 1
    }
    else if(tres<129){
      tres <- 2
    }
    else if(tres<139){
      tres <- 3
    }
    else{
      tres <- 4
    }
  }
  Heart$BloodPressureRange <- as.factor(sapply(Heart$trestbps,BloodPressure))
  
  ###############################
  ##### Feature engineering #####
  ###############################
  
  # GoodMaxHeart
  Heart$new <- rep(220,length(Heart$age)) - Heart$age
  Heart$GoodMaxHeart <- as.factor(ifelse((Heart$new*0.76 > Heart$thalach) &
                                           (Heart$new*0.64 < Heart$thalach),1,0))
  Heart <- select(Heart,-new)
  return(Heart)
}

Heart <- getSet(read.csv('Xy_train.csv'))
#############################################################
# Heart_test_final <- getSet(read.csv('X_test.csv'))
###########################################################################
###########################################################################
###########################################################################
###########################################################################
## Split the Dataset to training & test sets:
library(caTools)
set.seed(29)
split <- sample.split(Heart$Y ,SplitRatio = 0.7)
Heart_train <- subset(Heart,split == T)
Heart_test <- subset(Heart,split == F)


#########################
##### Decision Tree #####
#########################
library(rpart)
library(rpart.plot)

### 1
tree <- rpart(Y ~ . ,method = 'class',data = Heart_train)
#prp(tree)
rpart.plot(tree)

#Predict on training set:
preds.tree.train <- predict(tree, newdata = select(Heart_train,-Y), type = 'class')
(CM_train <- table(preds.tree.train,Heart_train$Y))
cat('train Accuracy =',sum(diag(CM_train)) / sum(CM_train))

#Predict on test set:
preds.tree.test <- predict(tree, newdata = select(Heart_test,-Y), type = 'class')
(CM_test <- table(preds.tree.test,Heart_test$Y))
cat('test Accuracy =',sum(diag(CM_test)) / sum(CM_test))

### 2
library(caret)
set.seed(29)
folds <- createFolds(Heart_train$Y, k = 10)

final_pred_test <- c()
final_pred_train <- c()
Minsplit <- seq(1,101,10)
for(i in Minsplit){
  test_pred <- c()
  train_pred <- c()
  for(x in folds){
    training_fold <- Heart_train[-x, ]
    test_fold <- Heart_train[x, ]
    classifier <- rpart(formula = Y ~ .,
                        data = training_fold,
                        method = 'class', minsplit = i)
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
(df_minsplit <- data.frame(Minsplit,final_pred_train,final_pred_test))
ggplot(df_minsplit, aes(Minsplit)) + 
  geom_line(aes(y=final_pred_test, color = "test")) + 
  geom_line(aes(y=final_pred_train, color = "train")) + 
  scale_color_manual(values = c( "steelblue","darkred"))+
  scale_y_continuous(name="Accuracy", limits=c(0, 1)) + 
  xlab('min split') + theme(legend.position = c(0.23, 0.1),
                            legend.direction = "horizontal")

###
final_pred_test <- c()
final_pred_train <- c()
cp <- c(0,0.002,0.005,0.01,0.015,0.02,0.03,0.5,1,2)
for(i in cp){
  test_pred <- c()
  train_pred <- c()
  for(x in folds){
    training_fold <- Heart_train[-x, ]
    test_fold <- Heart_train[x, ]
    classifier <- rpart(formula = Y ~ .,
                        data = training_fold,
                        method = 'class', cp = i)
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
(df_cp <- data.frame(cp,final_pred_train,final_pred_test))
ggplot(df_cp, aes(cp)) + 
  geom_line(aes(y=final_pred_test, color = "test")) + 
  geom_line(aes(y=final_pred_train, color = "train")) + 
  scale_color_manual(values = c( "steelblue","darkred"))+
  scale_y_continuous(name="Accuracy", limits=c(0, 1)) + 
  xlab('cp') + theme(legend.position = c(0.23, 0.1),
                            legend.direction = "horizontal")
###
final_pred_test <- c()
final_pred_train <- c()
maxdepth <-  c(1:10)
for(i in maxdepth){
  test_pred <- c()
  train_pred <- c()
  for(x in folds){
    training_fold <- Heart_train[-x, ]
    test_fold <- Heart_train[x, ]
    classifier <- rpart(formula = Y ~ .,
                        data = training_fold,
                        method = 'class', maxdepth = i)
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
(df_maxdepth <- data.frame(maxdepth,final_pred_train,final_pred_test))
ggplot(df_maxdepth,aes(x=maxdepth)) +
  geom_line(aes(y = final_pred_train), color = "darkred") + 
  geom_line(aes(y = final_pred_test), color="steelblue", linetype="twodash")+
  ggtitle('blue: validation, brown: train')+
  ylab('accuracy') + xlab('max depth') #+
  #geom_vline(xintercept = max(df_maxdepth$final_pred_test), color = "yellow")
ggplot(df_maxdepth, aes(maxdepth)) + 
  geom_line(aes(y=final_pred_test, color = "test")) + 
  geom_line(aes(y=final_pred_train, color = "train")) + 
  scale_color_manual(values = c( "steelblue","darkred"))+
  scale_y_continuous(name="Accuracy", limits=c(0, 1)) + 
  xlab('max depth') + theme(legend.position = c(0.23, 0.1),
                     legend.direction = "horizontal")

####
library(e1071)
audit.rpart <- tune.rpart(Y ~ ., data=Heart_train, minsplit=seq(1,16,2),
                          cp = c(0,0.0005,0.001,0.002,0.005,0.01,0.02,0.03),
                          maxdepth = 1:6)
audit.rpart
### 3
##
final_classifier <- rpart(formula = Y ~ .,data = Heart_train,method = 'class',
                          minsplit = 9, cp = 0.02, maxdepth = 4)
rpart.plot(final_classifier)
y_pred <- predict(final_classifier, newdata = Heart_test[,-14], type = 'class')
cm <- table(Heart_test[,14], y_pred)
(accuracy <- (sum(diag(cm)) / sum(cm)))
cat('tuning test Accuracy =',sum(diag(cm)) / sum(cm))
y_pred <- predict(final_classifier, newdata = Heart_train[,-14], type = 'class')
cm <- table(Heart_train[,14], y_pred)
(accuracy <- (sum(diag(cm)) / sum(cm)))
cat('tuning train Accuracy =',sum(diag(cm)) / sum(cm))


##
rpart.plot(final_classifier)
##
var.important <-  final_classifier$variable.importance
df_var_imp <- data.frame('var.name' = names(var.important),var.important)

ggplot(df_var_imp,aes(y=reorder(as.factor(var.name),var.important),x=var.important)) + 
  geom_bar(stat = "identity",aes(fill = var.important)) + xlab('variable') +
  ylab('important') +theme_classic()

###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###Normalize the data
normalize <- function(x) {
  return ((x - mean(x)) / (sd(x)))
}
HearT <- Heart

HearT$age <- normalize(HearT$age)
HearT$trestbps <- normalize(HearT$trestbps)
HearT$chol <- normalize(HearT$chol)
HearT$thalach <- normalize(HearT$thalach)
HearT$oldpeak <- normalize(HearT$oldpeak)
#####################
## Factors########
################
library(nnet)
HearT$gender <- class.ind(HearT$gender)
HearT$cp <- class.ind(HearT$cp)
HearT$fbs <- class.ind(HearT$fbs)
HearT$restecg <- class.ind(HearT$restecg)
HearT$exang <- class.ind(HearT$exang)
HearT$slope <- class.ind(HearT$slope)
HearT$ca <- class.ind(HearT$ca)
HearT$thal <- class.ind(HearT$thal)
HearT$BloodPressureRange <- class.ind(HearT$BloodPressureRange)
HearT$GoodMaxHeart <- class.ind(HearT$GoodMaxHeart)

#Splitting:
library(caTools)
set.seed(29)
split <- sample.split(HearT$Y ,SplitRatio = 0.7)
Heart_train <- subset(HearT,split == T)
Heart_test <- subset(HearT,split == F)
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################


#######################
##### Neural Nets #####
#######################
library(nnet)
library(NeuralNetTools)
library(dplyr)

##
nn <- nnet(Y~.,data = Heart_train, size = 1)
plotnet(nn)
#plot(nn)

# Train confusion matrix & accuracy
pred_train <- predict(nn, type='class') 
(cm_train <- table(prediction=pred_train, truth=Heart_train$Y))
(accuracy_train <- (sum(diag(cm_train)) / sum(cm_train)))
cat('train accuracy:',accuracy_train)

# Test confusion matrix & accuracy
pred_test <- predict(nn, newdata = select(Heart_test,-Y), type='class') 
(cm_test <- table(prediction=pred_test, truth=Heart_test$Y))
(accuracy_test <- (sum(diag(cm_test)) / sum(cm_test)))
cat('test accuracy:',accuracy_test)

#########
### 2 ###
#########
library(caret)
set.seed(29)
folds <- createFolds(Heart_train$Y, k = 10)

# size
final_pred_test <- c()
final_pred_train <- c()
Size <-  c(1:27)
for(i in Size){
  test_pred <- c()
  train_pred <- c()
  for(x in folds){
    training_fold <- Heart_train[-x, ]
    test_fold <- Heart_train[x, ]
    classifier <- nnet(Y~.,data = Heart_train, size = i)
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
ggplot(df_nn_size, aes(Size)) + 
  geom_line(aes(y=final_pred_test, color = "test")) + 
  geom_line(aes(y=final_pred_train, color = "train")) + 
  scale_color_manual(values = c( "steelblue","darkred"))+
  scale_y_continuous(name="Accuracy", limits=c(0.925, 1)) + 
  xlab('size') + theme(legend.position = c(0.23, 0.1),
                            legend.direction = "horizontal")


#maxit
final_pred_test <- c()
final_pred_train <- c()
Maxit <-  seq(50,500,10)
for(i in Maxit){
  test_pred <- c()
  train_pred <- c()
  for(x in folds){
    training_fold <- Heart_train[-x, ]
    test_fold <- Heart_train[x, ]
    classifier <- nnet(Y~.,data = Heart_train, Maxit = i,size = 1)
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
(df_nn_maxit <- data.frame(Maxit,final_pred_train,final_pred_test))
ggplot(df_nn_maxit, aes(Maxit)) + 
  geom_line(aes(y=final_pred_test, color = "test")) + 
  geom_line(aes(y=final_pred_train, color = "train")) + 
  scale_color_manual(values = c( "steelblue","darkred"))+
  scale_y_continuous(name="Accuracy", limits=c(0.9, 1)) + 
  xlab('maxit') + theme(legend.position = c(0.23, 0.1),
                       legend.direction = "horizontal")

#dacay
final_pred_test <- c()
final_pred_train <- c()
Decay <-  c(0,0.000001,0.00001,0.0001,0.001,0.01,0.2,0.3,0.4,0.5,1,2,3)
for(i in Decay){
  test_pred <- c()
  train_pred <- c()
  for(x in folds){
    training_fold <- Heart_train[-x, ]
    test_fold <- Heart_train[x, ]
    classifier <- nnet(Y~.,data = Heart_train, Decay = i,size = 1)
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
(df_nn_decay <- data.frame(Decay,final_pred_train,final_pred_test))
ggplot(df_nn_decay, aes(Decay)) + 
  geom_line(aes(y=final_pred_test, color = "test")) + 
  geom_line(aes(y=final_pred_train, color = "train")) + 
  scale_color_manual(values = c( "steelblue","darkred"))+
  scale_y_continuous(name="Accuracy", limits=c(0.91, 1)) + 
  xlab('decay') + theme(legend.position = c(0.23, 0.1),
                        legend.direction = "horizontal")

#rang
final_pred_test <- c()
final_pred_train <- c()
Rang <-  c(0,0.000001,0.00001,0.0001,0.001)
for(i in Rang){
  test_pred <- c()
  train_pred <- c()
  for(x in folds){
    training_fold <- Heart_train[-x, ]
    test_fold <- Heart_train[x, ]
    classifier <- nnet(Y~.,data = Heart_train, Rang = i,size = 1,trace=F)
    y_pred_test <- predict(classifier, newdata = test_fold[,-14], type = 'class')
    cm_test <- table(test_fold[,14], y_pred_test)
    accuracy_test <- sum(diag(cm_test)) / sum(cm_test)
    print(accuracy_test)
    y_pred_train <- predict(classifier, newdata = training_fold[,-14], type = 'class')
    cm_train <- table(training_fold[,14], y_pred_train)
    accuracy_train <- sum(diag(cm_train)) / sum(cm_train)
    #print(accuracy_train)
    test_pred <- c(test_pred,accuracy_test)
    train_pred <- c(train_pred,accuracy_train)
  }
  y_pred_tst <- predict(classifier, newdata = Heart_test, type = 'class')
  cm_tst <- table(Heart_test$Y, y_pred_tst)
  accuracy_tst <- sum(diag(cm_tst)) / sum(cm_tst)
  print(accuracy_tst)
  tst <- mean(test_pred)
  trn <- mean(train_pred)
  final_pred_train <- c(final_pred_train,trn)
  final_pred_test <- c(final_pred_test,tst)
}
(df_nn_rang <- data.frame(Rang,final_pred_train,final_pred_test))
ggplot(df_nn_rang, aes(Rang)) + 
  geom_line(aes(y=final_pred_test, color = "test")) + 
  geom_line(aes(y=final_pred_train, color = "train")) + 
  scale_color_manual(values = c( "steelblue","darkred"))+
  scale_y_continuous(name="Accuracy", limits=c(0.93, 1)) + 
  xlab('rang') + theme(legend.position = c(0.3, 0.1),
                        legend.direction = "horizontal")

#############################################################
library(caret)
nnGrid <-  expand.grid(size = c(21:27), 
                        maxit = seq(100,300,20), 
                        decay = c(0,0.00001,0.0001,0.001,0.01))
                        #rang =c(0,0.000001,0.00001)
                       
final_pred_test <- c()
final_pred_train <- c()
for(i in 1:nrow(nnGrid)){
  for(x in folds){
    training_fold <- Heart_train[-x, ]
    test_fold <- Heart_train[x, ]
    classifier <- nnet(Y~.,data = Heart_train, size = nnGrid[i,1],maxit = nnGrid[i,2],
                       dacay = nnGrid[i,3])#,rang = nnGrid[i,4])
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
df_tune_nn <- nnGrid
df_tune_nn$train <- final_pred_train
df_tune_nn$test <- final_pred_test
head(arrange(df_tune_nn,desc(test)))
ggplot(df_tune_nn)

###Finally
tune_nn <- nnet(Y~.,data = Heart_train, size = 24, maxit = 100,
           decay = 0)
plotnet(tune_nn)
#plot(nn)
# Train confusion matrix & accuracy
pred_train <- predict(tune_nn, type='class') 
(cm_train <- table(prediction=pred_train, truth=Heart_train$Y))
(accuracy_train <- (sum(diag(cm_train)) / sum(cm_train)))
cat('tune train accuracy:',accuracy_train)
# Test confusion matrix & accuracy
pred_test <- predict(tune_nn, newdata = select(Heart_test,-Y), type='class') 
(cm_test <- table(prediction=pred_test, truth=Heart_test$Y))
(accuracy_test <- (sum(diag(cm_test)) / sum(cm_test)))
cat('tune test accuracy:',accuracy_test)

#############################################################
set.seed(29)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

nnGrid <-  expand.grid(size=c(21:27), 
                        decay=c(0,0.00001,0.0001,0.001,0.01),
                       bag=c('TRUE','FALSE'))

nrow(nnGrid)

nnFit2 <- train(Y ~ ., data = Heart_train, 
                 method = "avNNet", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = nnGrid)
gbmFit2
gbmFit2$bestTune

beep(3)
### The optimal hyper parameter combination:

classifier <- train(form = Y ~ ., data = Heart_train, method = 'avNNet')
classifier
classifier$bestTune

# Train confusion matrix & accuracy
pred_train <- predict(classifier, type='raw') 
(cm_train <- table(prediction=pred_train, truth=Heart_train$Y))
(accuracy_train <- (sum(diag(cm_train)) / sum(cm_train)))
cat('train accuracy:',accuracy_train)

# Test confusion matrix & accuracy
pred_test <- predict(nn, newdata = select(Heart_test,-Y), type='class') 
(cm_test <- table(prediction=pred_test, truth=Heart_test$Y))
(accuracy_test <- (sum(diag(cm_test)) / sum(cm_test)))
cat('test accuracy:',accuracy_test)


#####################
##### K - Means #####
#####################
normalize <- function(x) {
  return ((x - mean(x)) / (sd(x)))
}
HearT <- Heart

HearT$age <- normalize(HearT$age)
HearT$trestbps <- normalize(HearT$trestbps)
HearT$chol <- normalize(HearT$chol)
HearT$thalach <- normalize(HearT$thalach)
HearT$oldpeak <- normalize(HearT$oldpeak)
#####################
## Factors########
################
library(nnet)
HearT$gender <- class.ind(HearT$gender)
HearT$cp <- class.ind(HearT$cp)
HearT$fbs <- class.ind(HearT$fbs)
HearT$restecg <- class.ind(HearT$restecg)
HearT$exang <- class.ind(HearT$exang)
HearT$slope <- class.ind(HearT$slope)
HearT$ca <- class.ind(HearT$ca)
HearT$thal <- class.ind(HearT$thal)
HearT$BloodPressureRange <- class.ind(HearT$BloodPressureRange)
HearT$GoodMaxHeart <- class.ind(HearT$GoodMaxHeart)

##1
k.means.model <- kmeans(as.matrix(select(HearT,-Y)),centers = 2)
cm_kmeans <- table(k.means.model$cluster-1,HearT$Y)
acc_kmeans <- (sum(diag(cm_kmeans)) / sum(cm_kmeans))
cat('Accuracy = ',acc_kmeans)


library(cluster)
clusplot(as.matrix(select(HearT,-Y)),k.means.model$cluster,color = T,shade = T,
         labels = 0, lines = 0, main="K-Means")



## 2,3
wcss <- vector()
for(k in 2:10) wcss[k-1] <- sum(kmeans(select(HearT,-Y),k, nstart = 25)$withinss)
plot(2:10,wcss,type = 'b', main = paste('Cluster clients'),
     xlab = 'Number of Clusters', ylab = 'WCSS')

library(clv)
dunn <- c(); DB <- c(); K <- 8
trn.kmeans <- select(HearT,-Y)
for(k in 2:K){
  clust_data <- kmeans(trn.kmeans, centers=k)
  scatt_data <- cls.scatt.data(trn.kmeans, clust=clust_data$cluster,
                               dist='euclidean')
  dunn <- c(dunn, clv.Dunn(scatt_data, 'centroid', 'centroid'))
  DB <- c(DB,   clv.Davies.Bouldin(scatt_data, 'centroid', 'centroid'))
}

clust_metrics <- data.frame(K = rep(seq(2,K,1),2), value = c(dunn, DB),
                            metric = c(rep('Dunn',K-1), rep('DB',K-1)))
ggplot(clust_metrics, aes(x=K, y=value, color=factor(metric))) +
  geom_point() + geom_line()+
  theme_gray() + theme(legend.position = c(0.2, 0.4))

## 4
#############################
## Hierarchical Clustering ##
#############################

#Finding Optimal K Clusters:
dandrogram <- hclust(dist(select(HearT,-Y),method = 'euclidean'),
                     method = 'ward.D')
#minimise the variance for each cluster(ward.D)
plot(dandrogram,
     main = paste('Dendogram'),
     xlab = 'Pations',
     ylab = 'Euclidean Distances')
######################################################
######################################################
######################################################

#######################
##### More Models #####
#######################

#########
## XGB ##
#########
Heart <- getSet(read.csv('Xy_train.csv'))

library(caret)
inTraining <- createDataPartition(Heart$Y, p = .7, list = FALSE)
# create a stratified random sample of the data into training and test sets
training <- Heart[ inTraining,]
testing  <- Heart[-inTraining,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

library(gbm)
gbmFit1 <- train(Y ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1


## Tuning Grids.
gbmGrid <-  expand.grid(interaction.depth = c(3, 5, 7, 9), 
                        n.trees = (1:10)*10, 
                        shrinkage = 0.1,
                        n.minobsinnode = c(10,15,20,25))

nrow(gbmGrid)

set.seed(29)
gbmFit2 <- train(Y ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2
gbmFit2$bestTune


## Plotting the Resampling Profile:

#The plot function can be used to examine the relationship between
#the estimates of performance and the tuning parameters.
trellis.par.set(caretTheme())
plot(gbmFit2)

# a heatmap of the results:
trellis.par.set(caretTheme())
plot(gbmFit2, metric = "Accuracy", plotType = "level",
     scales = list(x = list(rot = 90)))

# try this:
ggplot(gbmFit2)

############
#Predicting#
############
predict(gbmFit2, newdata = head(testing))
predY <- predict(gbmFit2, newdata = testing, type = "raw")
cm <- table(predY,testing$Y) 
(Accuracy_gbm <- sum(diag(cm))/sum(cm))




###################
## Random Forest ##
###################
Heart <- getSet(read.csv('Xy_train.csv'))
## Split the Dataset to training & test sets:
library(caTools)
set.seed(29)
split <- sample.split(Heart$Y ,SplitRatio = 0.7)
Heart_train <- subset(Heart,split == T)
Heart_test <- subset(Heart,split == F)

library(randomForest)
rf <- randomForest(Y~.,Heart_train)
rf_pr <- predict(rf,newdata = Heart_test,type = 'class')
(cm <- table(rf_pr,Heart_test$Y))
cat('Accuracy =',sum(diag(cm)) / sum(cm))

rf_pr <- predict(rf,newdata = Heart_train,type = 'class')
(cm <- table(rf_pr,Heart_train$Y))
cat('Accuracy =',sum(diag(cm)) / sum(cm))

library(caret)
set.seed(29)
folds <- createFolds(Heart_train$Y, k = 30)

## Tune mtry, ntree:
Mtry <- c(1:4)
Ntree <- c(350,400,450,500,525,550,600)
ntree_col <- c()
mtry_col <- c()
final_pred_train <- c()
final_pred_test <- c()
for(i in Mtry){
  for(j in Ntree){
    test_pred <- c()
    train_pred <- c()
    for(x in folds){
      training_fold <- Heart_train[-x, ]
      test_fold <- Heart_train[x, ]
      classifier <- randomForest(Y~.,data = training_fold, mtry=i,ntree=j)
      y_pred_test <- predict(classifier, newdata = test_fold[,-14], type = 'class')
      cm_test <- table(test_fold[,14], y_pred_test)
      accuracy_test <- sum(diag(cm_test)) / sum(cm_test)
      #print(accuracy_test)
      y_pred_train <- predict(classifier, newdata = training_fold[,-14], type = 'class')
      cm_train <- table(training_fold[,14], y_pred_train)
      accuracy_train <- sum(diag(cm_train)) / sum(cm_train)
      #print(accuracy_train)
      test_pred <- c(test_pred,accuracy_test)
      train_pred <- c(train_pred,accuracy_train)
    }
    tst <- mean(test_pred)
    trn <- mean(train_pred)
    final_pred_train <- c(final_pred_train,trn)
    final_pred_test <- c(final_pred_test,tst)
    ntree_col <- c(ntree_col,j)
    mtry_col <- c(mtry_col,i)
  }
}
(df <- data.frame(ntree_col,mtry_col,final_pred_train,final_pred_test))

ggplot(df, aes(ntree_col)) + 
  geom_line(aes(y=final_pred_test,color = 'red'))+
  #geom_line(aes(y=final_pred_train,color='blue'))+
  facet_wrap(~mtry_col)+
  #theme(legend.position = c(0.9, 0.1))+
  ylab('Accuracy') + xlab('ntree')

rf <- randomForest(Y~.,Heart_train,ntree = 550,mtry=2)
rf_pr <- predict(rf,newdata = Heart_test,type = 'class')
(cm <- table(rf_pr,Heart_test$Y))
cat('test Accuracy =',sum(diag(cm)) / sum(cm))


#########
## SVM ##
#########
svm_model_train <- svm(Y ~. ,data = Heart_train)#, kernel = 'linear')

pred_svm_model <- predict(svm_model_train, newdata = Heart_train)
CM <- table(pred_svm_model,Heart_train$Y)
cat(' Accuracy <-' ,sum(diag(CM)) / sum(CM))

pred_svm_model <- predict(svm_model_train, newdata = Heart_test)
CM <- table(pred_svm_model,Heart_test$Y)
cat(' Accuracy <-' ,sum(diag(CM)) / sum(CM))

#####tune
obj <- tune.svm(Species~., data = iris, gamma = 2^(-4:1), cost = seq(2,20,1),
                kernel=c('linear','polynomial','radial','sigmoid'))
obj$best.parameters

svm_model_train <- svm(Y ~. ,data = Heart_train,
                       gamma=0.125,cost=2,kernel='radial')#, kernel = 'linear')
pred_svm_model <- predict(svm_model_train, newdata = Heart_train,
                          type='class')
CM <- table(pred_svm_model,Heart_train$Y)
cat(' Accuracy <-' ,sum(diag(CM)) / sum(CM))

pred_svm_model <- predict(svm_model_train, newdata = Heart_test,type='class')
CM <- table(pred_svm_model,Heart_test$Y)
cat(' Accuracy <-' ,sum(diag(CM)) / sum(CM))


