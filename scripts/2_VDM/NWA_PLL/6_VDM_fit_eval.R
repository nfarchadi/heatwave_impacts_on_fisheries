#Fitting VDM & validation for U.S. Atlantic Pelagic Longline Fleet

library(tidyverse)
library(dismo)
library(lubridate)
library(here)

##############
# load in data
##############

NWA_PLL<-here("data","AIS","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_1to1ratio_absenceconstrained_convexhull_enhanced.rds") %>% readRDS()

# need to make sure responses (1 = presence & 0 = absence) are as integers for the BRT
NWA_PLL$Pres_abs<-as.integer(as.character(NWA_PLL$Pres_abs))


# adding lunar phases
NWA_PLL$lunar <- lunar::lunar.illumination(NWA_PLL$date)
NWA_PLL <-NWA_PLL %>% as.data.frame() %>% dplyr::select(-geometry)


#########
# fit BRT
#########
lr<-((0.0000017 * nrow(NWA_PLL)) - 0.000191) * (4:8)
lr<-lr[3]

set.seed(124) # for reproducibility 
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

# looking at relative influence
summary(brt)

# reponse curves
par(mar=c(4, 4, 1, 1))

dismo::gbm.plot(brt, n.plots = 12, write.title= FALSE, rug = T, smooth = TRUE, plot.layout=c(4,3), common.scale = T)


# save the model
saveRDS(brt, here("data","VDM_and_val","NWA_PLL","brt_VDM_gbm_step.rds"))


############
# validation
############

## K-fold cross-validation
source(here("scripts","2_VDM","functions","eval_kfold_brt.r"))


brt_VDM_k <- eval_kfold_brt(dataInput = NWA_PLL, 
                            gbm.x = c(9:13,16:20), 
                            gbm.y=7, learning.rate = lr, 
                            bag.fraction = 0.6, tree.complexity = 3,
                            k_folds = 10)









## Leave One Year Out cross-validation
source(here("scripts","2_VDM","functions","brt_VDM_loo.r"))

brt_VDM_loo <- eval_loo_brt(pres = NWA_PLL[which(NWA_PLL$Pres_abs == 1),],
                        abs = NWA_PLL[which(NWA_PLL$Pres_abs == 0),], 
                        gbm.x = c(9:13,16:20), 
                        gbm.y=7, learning.rate = lr, 
                        bag.fraction = 0.6, tree.complexity = 3)



#combining the model output, and both vlaidation results into a list to save
eval <- list(brt = brt, brt_k = brt_VDM_k, brt_loo = brt_VDM_loo)

#now save it

saveRDS(eval, here("data","VDM_and_eval","NWA_PLL","brt_VDM_eval_gbm_step.rds"))


