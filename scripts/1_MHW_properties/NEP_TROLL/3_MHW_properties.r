#characterizing MHW properties: intensity, size, and duration

library(here)
library(tidyverse)
library(heatwaveR)
library(sf)
library(raster)
library(SDMTools)

sf_use_s2(FALSE)# need to do this to remove spherical geometry


source(here("scripts","1_MHW_properties","functions","MHW_metrics.r"))


################################
# all NEP Troll management areas
################################

CP_MT_EK_CL_VN_MHW<-here("data","oisst", "NEP_TROLL",
                      "CP_MT_EK_CL_VN_MHW.rds") %>% readRDS()

CP_MT_EK_CL_VN_MHW$yearmon<-zoo::as.yearmon(CP_MT_EK_CL_VN_MHW$yearmon)

# lets run this for just MAB NEC NED mgmt areas
NEP_MHW_monthly_metrics_MAB_NEC_NED<-MHW_metrics(CP_MT_EK_CL_VN_MHW)


NEP_MHW_monthly_metrics_MAB_NEC_NED<-bind_rows(NEP_MHW_monthly_metrics_MAB_NEC_NED) %>% 
  dplyr::select(1,2,3,39:47) %>% rename("n.cell_MHW"="V44",
                                        "n.cell_noMHW"="V45",
                                        "n.cell_totalMHW"="V46",
                                        "mgmt_area"="V47") %>% 
  mutate(total.area= total.area/1000000) %>% 
  group_by(date,mgmt_area) %>% 
  mutate(total.area.mgmt.area_MHW = sum(total.area, na.rm = TRUE)) %>% ungroup %>%
  filter(class == 1) 

rm(CP_MT_EK_CL_VN_MHW) 

NEP_MHW_monthly_metrics_TROLL<-NEP_MHW_monthly_metrics_MAB_NEC_NED %>%
  mutate(month = lubridate::month(date))

saveRDS(NEP_MHW_monthly_metrics_TROLL, here("data","mgmt_area_metrics","NEP_TROLL","NEP_MHW_monthly_metrics_TROLL.rds"))

rm(NEP_MHW_monthly_metrics_MAB_NEC_NED,NEP_MHW_monthly_metrics_TROLL)
