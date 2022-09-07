#load in packages
library(tidyverse)
library(dismo)
library(lubridate)
library(here)

####------load in data------####
NWA_PLL<-here("data","AIS_processed","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull_v2_enhanced.rds") %>% readRDS()

#Pres_Abs_2013to2020_NWA_USA_LL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull.rds --- what I used previously in case I need to go back to it.

NWA_PLL$Pres_abs<-as.integer(as.character(NWA_PLL$Pres_abs))
NWA_PLL$Pres_abs %>% unique()

# If I want filter data to fishing_hours >= 2
#NWA_PLL_presences<-NWA_PLL %>% filter(Pres_abs == 1 & fishing_hours >=2)

#want presences to be >=2 fishing hours so we need to re-sample the absences again in order to get 1:1 ratio
# udates <-unique(NWA_PLL_presences$date)
# 
# absences<-data.frame()
# set.seed(124) #for reproducibility
# for (i in 1:length(udates)){
#   subdate_presence<-NWA_PLL_presences %>%  filter(date== udates[i] & Pres_abs == 1)
# 
#   subdate_absences= NWA_PLL %>%
#     filter((Pres_abs == 0 & date == udates[i])) %>%
#     .[sample(nrow(.),nrow(subdate_presence)),]
# 
#   absences<-rbind(absences,subdate_absences)
# }
# 
# NWA_PLL<-rbind(NWA_PLL_presences,absences)

#adding lunar phases
NWA_PLL$lunar <- lunar::lunar.illumination(NWA_PLL$date)
NWA_PLL <-NWA_PLL %>% as.data.frame() %>% dplyr::select(-geometry)

#checking for multi-collinearity
#library("PerformanceAnalytics")
#chart.Correlation(NWA_PLL[,12:23],method="pearson",histogram=TRUE)



####------Fit BRT------####
lr<-((0.0000017 * nrow(NWA_PLL)) - 0.000191) * (4:8)
lr<-lr[3]
set.seed(124)#for reproducibility 
start_time<-Sys.time()
brt <- dismo::gbm.step(data=NWA_PLL, 
                        gbm.x= c(9:13,16:20),  
                        gbm.y= 7, ### response variable
                        family = "bernoulli",
                        tree.complexity = 3, ### complexity of the interactions that the model will fit
                        learning.rate = lr,  ### optimized to end up with >1000 trees
                        bag.fraction = 0.6### recommended by Elith, amount of input data used each time
                        )

end_time<-Sys.time()
end_time - start_time #time difference

#looking at relative influence
summary(brt)

#reponse curves
par(mar=c(4, 4, 1, 1))

dismo::gbm.plot(brt, n.plots = 12, write.title= FALSE, rug = T, smooth = TRUE, plot.layout=c(4,3), common.scale = T)

ggsave(path="C:/Users/nfarc/Desktop/NASA_FaCeT/Plots", 
       filename = "relative_influence.png",
       width=12, height=12, units='in')


#save the model
saveRDS(brt, here("data","VDM_and_eval","NWA_PLL","brt_hoursabove1_VDM_gbm_step.rds"))



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


#now run the kfold cross-validation
brt_VDM_k <- eval_kfold_brt(dataInput = NWA_PLL, 
                            gbm.x = c(9:13,16:20), 
                            gbm.y=7, learning.rate = lr, 
                            bag.fraction = 0.6, tree.complexity = 3,
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


brt_VDM_loo <- eval_loo_brt(pres = NWA_PLL[which(NWA_PLL$Pres_abs == 1),],
                        abs = NWA_PLL[which(NWA_PLL$Pres_abs == 0),], 
                        gbm.x = c(9:13,16:20), 
                        gbm.y=7, learning.rate = lr, 
                        bag.fraction = 0.6, tree.complexity = 3)



#combining the model output, and both vlaidation results into a list to save
eval <- list(brt = brt, brt_k = brt_VDM_k, brt_loo = brt_VDM_loo)

#now save it

saveRDS(eval, here("data","VDM_and_eval","NWA_PLL","brt_hoursabove1_VDM_eval_gbm_step.rds"))


