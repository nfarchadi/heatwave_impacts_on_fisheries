#leave one year out function

eval_loo_brt <- function(pres, abs, gbm.x, gbm.y, learning.rate = 0.05, tree.complexity = 3, bag.fraction = 0.6){
  
  pres$year <- lubridate::year(pres$date)
  abs$year <- lubridate::year(abs$date)
  
  ## setup output df
  Evaluations_LOO_BRT <- as.data.frame(matrix(data = 0, nrow = 1, ncol = 5))
  colnames(Evaluations_LOO_BRT) <- c("k","Deviance","AUC","TSS","n_pres")
  counter=1
  
  u_years <- unique(pres$year)
  for (y in u_years){
    
    print(paste('Running ', which(u_years == y), ' of ', length(u_years), sep=''), sep='')
    
    pres_train <- pres[which(pres$year != y),]
    ## absences are sampled from the full input absence df
    abs_train <- abs[sample(which(abs$year != y), size = nrow(pres_train), replace=F),]
    train <- rbind(pres_train, abs_train)
    
    pres_test <- pres[which(pres$year == y),]
    abs_test <- abs[sample(which(abs$year == y), size = nrow(pres_test), replace=F),]
    test <- rbind(pres_test, abs_test)
    
    brt.loo <- dismo::gbm.step(data=train, gbm.x= gbm.x, 
                               gbm.y = gbm.y, 
                               family="bernoulli", 
                               tree.complexity = tree.complexity,
                               learning.rate = learning.rate, 
                               bag.fraction = bag.fraction)
    
    ## make predictions for eval
    preds <- gbm::predict.gbm(brt.loo, test,
                              n.trees=brt.loo$gbm.call$best.trees, 
                              type="response")
    
    dev_eval3<-function(x){
      null <- x$self.statistics$mean.null
      res <- x$self.statistics$mean.resid
      dev=((null - res)/null)*100
      return(dev)
    }
    dev<-dev_eval3(brt.loo)
    #dev <- dismo::calc.deviance(obs=test[,gbm.y], pred=preds, calc.mean=TRUE)
    
    d <- cbind(test[,gbm.y], preds)
    pres_y <- as.numeric(d[d[,1] == 1,2])
    abs_y <- as.numeric(d[d[,1] == 0,2])
    e <- dismo::evaluate(p = pres_y, a = abs_y)
    
    Evaluations_LOO_BRT[counter,1] <- y
    Evaluations_LOO_BRT[counter,2] <- dev
    Evaluations_LOO_BRT[counter,3] <- e@auc
    Evaluations_LOO_BRT[counter,4] <- max(e@TPR + e@TNR-1)
    Evaluations_LOO_BRT[counter,5] <- length(which(train[,gbm.y] == 1))
    
    counter=counter+1 
  }
  return(Evaluations_LOO_BRT)
}