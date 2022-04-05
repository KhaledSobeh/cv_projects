ff <- weather[1:10000,]

library(caret)
folds <- createFolds(ff$TempLabel, k = 5)
Mtry <- c(4:7)
Ntree <- c(500,550,600,650)
ntree_col <- c()
mtry_col <- c()
mean_rmse_col <- c()
for(i in Mtry){
  for(j in Ntree){
    rmsecol <- c()
    for (x in folds){
      #print('Hi')
      cross.train <- ff[-x,] # train subset
      cross.validation <-  ff[x,] # test subset
      ranForest <- randomForest(TempLabel ~. ,data = cross.train,mtry = i, ntree = j) # train
      predictions <- predict(ranForest, newdata=cross.validation,type = 'class')
      cm <- table(predictions,cross.validation$TempLabel)
      rmsecol <- c(rmsecol,(sum(diag(cm)) / sum(cm)))
    }
    mn <- mean(rmsecol)
    mean_rmse_col <- c(mean_rmse_col,mn)
    mtry_col <- c(mtry_col,i)
    ntree_col <- c(ntree_col,j)
  }
}
beep(6)
(df <- data.frame(ntree_col,mtry_col,mean_rmse_col))
beep(3)

arrange(df,mean_rmse_col)


install.packages("beepr")
library(beepr)
beep(6)
beep(3) 












