#Fitting VDM & validation for U.S. Pacific Troll Fleet

library(tidyverse)
library(dismo)
library(lubridate)
library(here)

##############
# load in data
##############

NEP_TROLL<-here("data","AIS","NEP_TROLL","Pres_Abs_2012to2020_NEP_USA_TROLL_onlyfishing_1to1ratio_absenceconstrained_convexhull_enhanced.rds") %>% readRDS()

# need to make sure responses (1 = presence & 0 = absence) are as integers for the BRT
NEP_TROLL$Pres_abs<-as.integer(as.character(NEP_TROLL$Pres_abs))


# adding lunar phases
NEP_TROLL$lunar <- lunar::lunar.illumination(NEP_TROLL$date)
NEP_TROLL <-NEP_TROLL %>% as.data.frame() %>% dplyr::select(-geometry)


#########
# fit BRT
#########

set.seed(124) # for reproducibility 
start_time<-Sys.time()
brt <- dismo::gbm.step(data=NEP_TROLL, 
                        gbm.x= c(9:13,16:20),  
                        gbm.y= 7, ### response variable
                        family = "bernoulli",
                        tree.complexity = 7, ### complexity of the interactions that the model will fit
                        learning.rate = 0.03,  ### optimized to end up with >1000 trees
                        bag.fraction = 0.6### recommended by Elith, amount of input data used each time
                        )

end_time<-Sys.time()
end_time - start_time #time difference

# looking at relative influence
summary(brt)

# reponse curves
par(mar=c(4, 4, 1, 1))

dismo::gbm.plot(brt, n.plots = 12, write.title= FALSE, rug = T, smooth = TRUE, plot.layout=c(4,3), common.scale = T)


# save the model
saveRDS(brt, here("data","VDM_and_val","NEP_TROLL","brt_VDM_gbm_step.rds"))


############
# validation
############

## K-fold cross-validation
source(here("scripts","2_VDM","functions","eval_kfold_brt.r"))


brt_VDM_k <- eval_kfold_brt(dataInput = NEP_TROLL, 
                            gbm.x = c(9:13,16:20), 
                            gbm.y=7, learning.rate = lr, 
                            bag.fraction = 0.6, tree.complexity = 3,
                            k_folds = 10)









## Leave One Year Out cross-validation
source(here("scripts","2_VDM","functions","eval_loo_brt.r"))

brt_VDM_loo <- eval_loo_brt(pres = NEP_TROLL[which(NEP_TROLL$Pres_abs == 1),],
                        abs = NEP_TROLL[which(NEP_TROLL$Pres_abs == 0),], 
                        gbm.x = c(9:13,16:20), 
                        gbm.y=7, learning.rate = lr, 
                        bag.fraction = 0.6, tree.complexity = 3)



#combining the model output, and both vlaidation results into a list to save
eval <- list(brt = brt, brt_k = brt_VDM_k, brt_loo = brt_VDM_loo)

#now save it

saveRDS(eval, here("data","VDM_and_val","NEP_TROLL","brt_VDM_eval_gbm_step.rds"))


