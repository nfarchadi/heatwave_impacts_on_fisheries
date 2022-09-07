#Calculating VDM Monthly means and monthly anomalies

library(tidyverse)
library(raster)
library(zoo)
library(glue)


#Need to bring in the spatial predictions 
VDMharddrivepath<-"E:/VDM_results/NEP_TROLL"
predir=glue("{VDMharddrivepath}/spatial_predictions/")


VDM_predictions<-list.files(predir, full.names = TRUE) %>%
  grep(pattern = ".nc", value = TRUE)

ras_stack_VDM <- VDM_predictions %>% stack()
idx<-format(as.Date(substr(VDM_predictions, 46,55)), format = "%Y-%m")
names(ras_stack_VDM)<-idx

############################################################
#Monthly means
############################################################
VDM_month<-stackApply(ras_stack_VDM,idx, fun = mean)

#adding in a time variable
tt<-as.yearmon(unique(format(as.Date(substr(VDM_predictions, 46,55)), format = "%Y-%m")))
VDM_month<-setZ(VDM_month,tt)
names(VDM_month)<-as.character(tt)

#save these monthly predictions for later use
writeRaster(VDM_month, here("data","Monthly_Spatial_Predictions","NEP_TROLL","NEP_TROLL_monthly_predictions_2012to2020"), format='CDF', overwrite=TRUE)


############################################################
#Monthly VDM anomaly (monthly mean - climatolgical monthly mean / month SD)
############################################################

## Extract month value from a Date or yearmon object
month <- function(x)format(x, '%m')
## Compute anomaly using monthly grouping with ave  
anomaly <- function(x){
  ## Monthly means
  mm <- ave(x, month(tt), FUN = mean)
  ## Monthly standard deviation
  msd <- ave(x, month(tt), FUN = sd)
  ## anomaly
  (x - mm)/msd
}

## Use anomaly with calc
VDManom <- calc(VDM_month, anomaly)
VDManom <- setZ(VDManom, tt)
writeRaster(VDManom, here("data","Monthly_Spatial_Predictions","NEP_TROLL","NEP_TROLL_monthly_anom_predictions_2012to2020"), format='CDF', overwrite=TRUE)

