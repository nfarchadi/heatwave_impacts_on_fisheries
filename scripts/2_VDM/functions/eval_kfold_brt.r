#k fold cross validation function

eval_kfold_brt <- function(dataInput, gbm.x, gbm.y, learning.rate = 0.05, k_folds = 5, tree.complexity = 3, bag.fraction = 0.6){
  dataInput$Kset <- dismo::kfold(dataInput, k_folds) #randomly allocate k groups
  Evaluations_kfold_BRT <- as.data.frame(matrix(data=0,nrow=5,ncol=4)) 
  colnames(Evaluations_kfold_BRT) <- c("k","Deviance","AUC","TSS")
  counter=1
  for (k in 1:k_folds){
    print(k)
    train <- dataInput[dataInput$Kset!=k,]
    test <- dataInput[dataInput$Kset==k,]
    brt.k <- dismo::gbm.step(data=train, gbm.x= gbm.x, 
                             gbm.y = gbm.y, 
                             family="bernoulli", 
                             tree.complexity = tree.complexity,
                             learning.rate = learning.rate, 
                             bag.fraction = bag.fraction 
    )
    preds <- gbm::predict.gbm(brt.k, test,
                              n.trees=brt.k$gbm.call$best.trees, 
                              type="response")
    
    dev_eval3<-function(x){
      #null <- x$self.statistics$null.deviance #use with gbm.fixed
      #res <- x$self.statistics$resid.deviance
      null <- x$self.statistics$mean.null
      res <- x$self.statistics$mean.resid
      dev=((null - res)/null)*100
      return(dev)
    }
    dev<-dev_eval3(brt.k)
    #dev <- dismo::calc.deviance(obs=test[,gbm.y], pred=preds, calc.mean=TRUE)
    d <- cbind(test[gbm.y], preds)
    pres <- as.numeric(d[d[,1]==1,2])
    abs <- as.numeric(d[d[,1]==0,2])
    e <- dismo::evaluate(p=pres, a=abs)
    Evaluations_kfold_BRT[counter,1] <- k
    Evaluations_kfold_BRT[counter,2] <- dev
    Evaluations_kfold_BRT[counter,3] <- e@auc
    Evaluations_kfold_BRT[counter,4] <- max(e@TPR + e@TNR-1)
    counter=counter+1 
  }
  return(Evaluations_kfold_BRT)
}