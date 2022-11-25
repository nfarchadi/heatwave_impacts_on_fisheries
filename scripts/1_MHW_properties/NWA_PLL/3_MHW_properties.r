#characterizing MHW properties: intensity, size, and duration

library(here)
library(tidyverse)
library(pracma)
library(heatwaveR)
library(sf)
library(patchwork)
library(raster)
library(viridis)

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

saveRDS(NWA_MHW_monthly_metrics_northern, here("data","Mgmt_area","NWA_PLL","NWA_MHW_monthly_metrics_northern.rds"))

# #This is landscape metrics for the NWA PLL
# NWA_PLL_landscape_metrics<-readRDS(here("data","Mgmt_area","NWA_PLL_landscapemetrics_monthly.rds"))
# 
# #suitable fishing habitat area ~ area of anomalously warmer water
# NWAPLL_lsm_month_northern<-NWA_PLL_landscape_metrics %>% 
#   dplyr::select(date,total.area,mgmt_area,total.area.mgmt.area, 
#                 COGx, COGy, COGx.sd, COGy.sd,X25,X75,Y25,Y75,n.cell_habitat,
#                 n.cell_nohabitat,n.cell_total) %>% 
#   rename("NWA_PLL.total.area"="total.area") %>% 
#   mutate(prop_habitat_area = NWA_PLL.total.area/total.area.mgmt.area,
#          prop_habitat_cells = n.cell_habitat / n.cell_total)
# 
# NWAMHW_lsm_month_northern<-NWA_MHW_monthly_metrics_northern %>%
#   dplyr::select(date,total.area,total.area.mgmt.area_MHW, 
#                 mean_SSTa, max_SSTa, mgmt_area,n.cell_MHW,
#                 n.cell_noMHW,n.cell_totalMHW) %>%
#   rename("NWA_MHW.total.area" = "total.area") %>% 
#   mutate(prop_MHW_area = NWA_MHW.total.area/total.area.mgmt.area_MHW,
#          prop_MHW_cells = n.cell_MHW / n.cell_totalMHW)
# 
# #combine the suitability and MHW class stat dfs and classify MHW sizes
# NWAPLL_MHW_northern<-left_join(NWAPLL_lsm_month_northern,NWAMHW_lsm_month_northern,
#                           by= c("date","mgmt_area")) %>% 
#   filter(mgmt_area %in% c("MAB","NEC","NED")) %>% 
#   mutate(MHW = case_when(prop_MHW_area >= .3 ~ "Large MHW",
#                          prop_MHW_area < .3 & prop_MHW_area >= .1 ~ "Small MHW",
#                          prop_MHW_area < .1 ~ "Near-Average",
#                          TRUE ~ "Near-Average")) %>%
#   mutate(month = lubridate::month(date)) 



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

saveRDS(NWA_MHW_monthly_metrics_southern, here("data","Mgmt_area","NWA_PLL","NWA_MHW_monthly_metrics_southern.rds"))

# #This is landscape metrics for the NWA PLL
# NWA_PLL_landscape_metrics_southern<-readRDS(here("data","Mgmt_area","NWA_PLL_landscapemetrics_monthly.rds"))
# 
# 
# #suitable fishing habitat area ~ area of anomalously warmer water
# NWAPLL_lsm_month_southern<-NWA_PLL_landscape_metrics %>% 
#   dplyr::select(date,total.area,mgmt_area,total.area.mgmt.area, 
#                 COGx, COGy, COGx.sd, COGy.sd,X25,X75,Y25,Y75,n.cell_habitat,
#                 n.cell_nohabitat, n.cell_total) %>% 
#   rename("NWA_PLL.total.area"="total.area") %>% 
#   mutate(prop_habitat_area = NWA_PLL.total.area/total.area.mgmt.area,
#          prop_habitat_cells = n.cell_habitat / n.cell_total)
# 
# NWAMHW_lsm_month_southern<-NWA_MHW_monthly_metrics_southern %>% 
#   dplyr::select(date,total.area,total.area.mgmt.area_MHW, 
#                 mean_SSTa, max_SSTa, mgmt_area,n.cell_MHW,
#                 n.cell_noMHW, n.cell_totalMHW) %>%
#   rename("NWA_MHW.total.area" = "total.area") %>% 
#   mutate(prop_MHW_area = NWA_MHW.total.area/total.area.mgmt.area_MHW,
#          prop_MHW_cells = n.cell_MHW / n.cell_totalMHW)
# 
# NWAPLL_MHW_southern<-left_join(NWAPLL_lsm_month_southern,NWAMHW_lsm_month_southern,
#                           by= c("date","mgmt_area"))  %>% 
#   filter(mgmt_area %in% c("GOM", "CAR", "FEC", "SAR", "SAB")) %>% 
#   mutate(MHW = case_when(prop_MHW_area >= .3 ~ "Large MHW",
#                          prop_MHW_area < .3 & prop_MHW_area >= .1 ~ "Small MHW",
#                          prop_MHW_area < .1 ~ "Near-Average",
#                          TRUE ~ "Near-Average")) %>%
#   mutate(month = lubridate::month(date))
