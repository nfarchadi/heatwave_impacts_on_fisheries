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
brt<-here("data","VDM_and_val","NEP_TROLL","brt_VDM_gbm_step.rds") %>% readRDS()


# hycom directory for NEP
HYCOM_NEP_dir<-here("data","hycom","NEP_TROLL","hycom_combine")

port_NEP_dir<-here("data","static","NEP_TROLL","NEP_dis_port.nc") 

seamount_NEP_dir<-here("data","static","NEP_TROLL","NEP_dis_seamount.nc")


#output directory
output_dir<-here("data","spatial_predictions","NEP_TROLL")


#function to predict daily
predict_NEP_TROLL_VDM(udates, brt, HYCOM_NEP_dir, port_NEP_dir, seamount_NEP_dir, 
                    output_dir)
