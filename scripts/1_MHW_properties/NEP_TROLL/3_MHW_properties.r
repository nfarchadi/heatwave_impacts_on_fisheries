#characterizing MHW properties: intensity, size, and duration

library(here)
library(tidyverse)
library(heatwaveR)
library(sf)
library(raster)
library(SDMTools)

sf_use_s2(FALSE)# need to do this to remove spherical geometry


source(here("scripts","1_MHW_properties","functions","MHW_metrics.r"))


###########################################
# Northern management areas - NED, NEC, MAB
###########################################

MAB_NEC_NED_MHW<-here("data","oisst", "NWA_PLL",
                      "MAB_NEC_NED_MHW.rds") %>% readRDS()

MAB_NEC_NED_MHW$yearmon<-zoo::as.yearmon(MAB_NEC_NED_MHW$yearmon)

# lets run this for just MAB NEC NED mgmt areas
NWA_MHW_monthly_metrics_MAB_NEC_NED<-MHW_metrics(MAB_NEC_NED_MHW)


NWA_MHW_monthly_metrics_MAB_NEC_NED<-bind_rows(NWA_MHW_monthly_metrics_MAB_NEC_NED) %>% 
  dplyr::select(1,2,3,39:47) %>% rename("n.cell_MHW"="V44",
                                        "n.cell_noMHW"="V45",
                                        "n.cell_totalMHW"="V46",
                                        "mgmt_area"="V47") %>% 
  mutate(total.area= total.area/1000000) %>% 
  group_by(date,mgmt_area) %>% 
  mutate(total.area.mgmt.area_MHW = sum(total.area, na.rm = TRUE)) %>% ungroup %>%
  filter(class == 1) 

rm(MAB_NEC_NED_MHW) 

NWA_MHW_monthly_metrics_northern<-NWA_MHW_monthly_metrics_MAB_NEC_NED %>%
  mutate(month = lubridate::month(date))

saveRDS(NWA_MHW_monthly_metrics_northern, here("data","mgmt_area_metrics","NWA_PLL","NWA_MHW_monthly_metrics_northern.rds"))

rm(NWA_MHW_monthly_metrics_MAB_NEC_NED,NWA_MHW_monthly_metrics_northern)

#####################################################
# Southern management areas - GOM, CAR, FEC, SAR, SAB
#####################################################

GOM_CAR_FEC_SAR_SAB_MHW<-here("data","water_temp","NWA","oisst",
                      "GOM_CAR_FEC_SAR_SAB_MHW.rds") %>% readRDS()

# lets run this for just GOM CAR FEC SAR SAB mgmt area
NWA_MHW_monthly_metrics_GOM_CAR_FEC_SAR_SAB<-MHW_metrics(GOM_CAR_FEC_SAR_SAB_MHW)


NWA_MHW_monthly_metrics_GOM_CAR_FEC_SAR_SAB<-bind_rows(NWA_MHW_monthly_metrics_GOM_CAR_FEC_SAR_SAB) %>%
  dplyr::select(1,2,3,39:47) %>% rename("n.cell_MHW"="V44",
                                        "n.cell_noMHW"="V45",
                                        "n.cell_totalMHW"="V46",
                                        "mgmt_area"="V47") %>%  
  mutate(total.area= total.area/1000000) %>% 
  group_by(date,mgmt_area) %>% 
  mutate(total.area.mgmt.area_MHW = sum(total.area, na.rm = TRUE)) %>% ungroup %>% 
  filter(class == 1)

rm(GOM_CAR_FEC_SAR_SAB_MHW)

NWA_MHW_monthly_metrics_southern<-NWA_MHW_monthly_metrics_GOM_CAR_FEC_SAR_SAB %>% 
  mutate(month = lubridate::month(date))

saveRDS(NWA_MHW_monthly_metrics_southern, here("data","mgmt_area_metrics","NWA_PLL","NWA_MHW_monthly_metrics_southern.rds"))


rm(NWA_MHW_monthly_metrics_GOM_CAR_FEC_SAR_SAB,NWA_MHW_monthly_metrics_southern)
