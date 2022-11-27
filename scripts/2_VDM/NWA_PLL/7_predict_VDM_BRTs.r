#Daily predictions for VDMs

library(tidyverse)
library(raster)
library(lunar)
library(ncdf4)
library(here)

source(here("scripts","2_VDM","functions","predict_VDM.r"))

# days from 2012 - 2020
udates<-seq(as.Date("2012-01-01"),as.Date("2020-12-31"),by="day")

# load in the model
brt<-here("data","VDM_and_val","NWA_PLL","brt_VDM_gbm_step.rds") %>% readRDS()


# hycom directory for NWA
HYCOM_NWA_dir<-here("data","hycom","NWA_PLL","hycom_combine")

port_NWA_dir<-here("data","static","NWA_PLL","NWA_dis_port.nc") 

seamount_NWA_dir<-here("data","static","NWA_PLL","NWA_dis_seamount.nc")


#output directory
output_dir<-here("data","spatial_predictions","NWA_PLL")


#function to predict daily
predict_NWA_PLL_VDM(udates, brt, HYCOM_NWA_dir, port_NWA_dir, seamount_NWA_dir, 
                    output_dir)
