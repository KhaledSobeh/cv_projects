library(ggplot2)
library(dplyr)
library(beepr)
weather <- read.csv('train.csv')
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
weather$stn_type <- as.factor(weather$stn_type)
weather$stn_type <- as.factor(sapply(weather$stn_type,change_stn))
weather$stn_elev_2000 <- as.factor(ifelse(weather$stn_elev>2000,1,0))
weather$climate_type <- as.factor(weather$climate_type)
wcss <- vector()
for(k in 1:15) wcss[k] <- sum(kmeans(select(weather,c(lat,lon)),k)$withinss)
plot(1:15,wcss,type = 'b', main = paste('Cluster clients'),
     xlab = 'Number of Clusters', ylab = 'WCSS')
km <- kmeans(select(weather,c(lat,lon)),centers = 5)
df <- data.frame(weather$lat,weather$lon,km$cluster)
weather$district <- as.factor(df$km.cluster)
weather$lg_aqua_night_lst <- log(weather$aqua_night_lst)
weather$lg_aqua_night_lst_binary <- as.factor(ifelse(weather$lg_aqua_night_lst == 'NaN',
                                                     0,1))
weather$lg_aqua_night_lst <- ifelse(weather$lg_aqua_night_lst == 'NaN',
                                    -10,weather$lg_aqua_night_lst)

#################
library(caret)
Weather <- weather[1:5000,]
inTraining <- createDataPartition(Weather$TempLabel, p = .7, list = FALSE)
# create a stratified random sample of the data into training and test sets
training <- Weather[ inTraining,]
testing  <- Weather[-inTraining,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 5)

library(gbm)
#gbmFit1 <- train(TempLabel ~ ., data = training, 
#                 method = "gbm", 
#                 trControl = fitControl,
#                 ## This last option is actually one
#                 ## for gbm() that passes through
#                 verbose = FALSE)
#gbmFit1

## Tuning Grids.
gbmGrid <-  expand.grid(interaction.depth = c(2,3,4), 
                        n.trees = (4:15)*10, 
                        shrinkage = c(0.05,0.1,0.15),
                        n.minobsinnode = c(5,10,15))

nrow(gbmGrid)


gbmFit2 <- train(TempLabel ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2
gbmFit2$bestTune

beep(3)
##################################################################
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
cm <- table(predY,testing$TempLabel) 
(Accuracy_gbm <- sum(diag(cm))/sum(cm))





##################################################################3




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

library(rpart)
gbmFit1 <- train(Y ~ ., data = training, 
                 method = "rpart", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1


## Tuning Grids.
gbmGrid <-  expand.grid(minsplit=seq(1,41,2),
                         cp = c(0,0.0005,0.001,0.002,0.005,0.01,0.02),
                         maxdepth = 1:6)

nrow(gbmGrid)

gbmFit2 <- train(Y ~ ., data = training, 
                 method = "rpart", 
                 trControl = fitControl, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2
gbmFit2$bestTune

audit.rpart <- tune.rpart(Y ~ ., data=training, minsplit=seq(1,41,2),
                          cp = c(0,0.0005,0.001,0.002,0.005,0.01,0.02),
                          maxdepth = 1:6)


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

predY <- predict(gbmFit2, newdata = testing, type = "prob")
y_pred <- c()
for(i in 1:length(predY[,1])){
  if(predY[i,1] >= predY[,2]){
    y_pred[i] <- 0
  }
  else{
    y_pred[i] <- 1
  }
}
y_pred
cm <- table(y_pred,testing$Y) 
(Accuracy_gbm <- sum(diag(cm))/sum(cm))







