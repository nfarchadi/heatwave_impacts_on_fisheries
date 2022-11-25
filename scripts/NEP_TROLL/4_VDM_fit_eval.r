#load in packages
library(tidyverse)
library(dismo)
library(lubridate)
library(here)
library(caret)

####------load in data------####
NEP_TROLL<-here("data","AIS_processed","NEP_TROLL","Pres_Abs_2013to2020_NWA_USA_TROLL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull_v2_enhanced.rds") %>% readRDS() %>% as.data.frame() %>% na.omit()


NEP_TROLL$Pres_abs<-as.integer(as.character(NEP_TROLL$Pres_abs))
NEP_TROLL$Pres_abs %>% unique()


#adding lunar phases
NEP_TROLL$lunar <- lunar::lunar.illumination(NEP_TROLL$date)
NEP_TROLL <-NEP_TROLL %>% dplyr::select(-geometry)

# #histogram to look at the data
# NEP_TROLL %>% dplyr::select(c(12:16,19:23)) %>% gather() %>% ggplot(aes(value))+geom_histogram(bins = 10)+facet_wrap(~key, scales = "free_x")


####----Exploring different learning rates & interaction depth----####

#need to change response for caret package
NEP_TROLL <- NEP_TROLL %>% 
  mutate(Pres_abs = if_else(Pres_abs == 1, "Pres", "Abs"),
         Pres_abs = as.factor(Pres_abs))

NEP_TROLL$Pres_abs %>% unique()

#make grid for testing different learning rates
gbmGrid <-  expand.grid(interaction.depth = c(3:8), 
                        n.trees = 2000, 
                        shrinkage = seq(0.01,0.2,by=0.02),
                        n.minobsinnode = 10)

fitControl <- trainControl(method = "repeatedcv",
                           number = 2,
                           repeats = 1,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(124)
gbmFit <- train(as.factor(Pres_abs) ~ n2 + sst_sd + sst + ssh_sd +
                   ssh + bathy + rugosity + dis_port + 
                   dis_seamount + lunar,
                data = NEP_TROLL, 
                method = "gbm",
                trControl = fitControl,
                verbose = FALSE, 
                ## Now specify the exact models 
                ## to evaluate:
                tuneGrid = gbmGrid,
                metric = "ROC")

ggplot(gbmFit)


####------Fit BRT------####
# lr<-((0.0000017 * nrow(NEP_TROLL)) - 0.000191) * (1:8)
# lr<-lr[1]
# lr<-round(lr,3)
set.seed(124)#for reproducibility 
start_time<-Sys.time()
brt <- dismo::gbm.step(data=NEP_TROLL, 
                        gbm.x= c(12:16,19:23),  
                        gbm.y= 7, ### response variable
                        family = "bernoulli",
                        tree.complexity = 7, ### complexity of the interactions that the model will fit
                        learning.rate = 0.03,  ### optimized to end up with >1000 trees
                        bag.fraction = 0.6### recommended by Elith, amount of input data used each time
                        )

end_time<-Sys.time()
end_time - start_time #time difference

#looking at relative influence
summary(brt)#response curves

par(mar=c(4, 4, 1, 1))

dismo::gbm.plot(brt, n.plots = 12, write.title= FALSE, rug = T, smooth = TRUE, plot.layout=c(4,3), common.scale = T)

# ggsave(path="C:/Users/nfarc/Desktop/NASA_FaCeT/Plots", 
#        filename = "relative_influence.png",
#        width=12, height=12, units='in')


#save the model
# saveRDS(brt, here("data","VDM_and_eval","NEP_TROLL","brt_hoursabove1_VDM_gbm_step.rds"))

saveRDS(brt, here("data","VDM_and_eval","NEP_TROLL","brt_hoursabove1_VDM_gbm_step CCS_filtered_w_bathyrugosity_lr0.03_treecomp7.rds"))


###################----VALIDATION----##########################

####------K-fold cross-validation------####
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
                              bag.fraction = bag.fraction)
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


#now run the kfold cross-validation
brt_VDM_k2 <- eval_kfold_brt(dataInput = NEP_TROLL, 
                            gbm.x = c(12:16,19:23), 
                            gbm.y=7, learning.rate = 0.03, 
                            bag.fraction = 0.6, tree.complexity = 7,
                            k_folds = 10)








####------Leave One Year OUT------####
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


brt_VDM_loo <- eval_loo_brt(pres = NEP_TROLL[which(NEP_TROLL$Pres_abs == 1),],
                        abs = NEP_TROLL[which(NEP_TROLL$Pres_abs == 0),], 
                        gbm.x = c(12:16,19:23), 
                        gbm.y=7, learning.rate = 0.03, 
                        bag.fraction = 0.6, tree.complexity = 7)



#combining the model output, and both vlaidation results into a list to save
eval <- list(brt = brt, brt_k = brt_VDM_k, brt_loo = brt_VDM_loo)

#now save it

saveRDS(eval, here("data","VDM_and_eval","NEP_TROLL","brt_hoursabove1_VDM_eval_gbm_step.rds"))


