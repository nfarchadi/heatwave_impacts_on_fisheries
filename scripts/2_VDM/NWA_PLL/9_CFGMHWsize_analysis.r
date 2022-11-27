# calculating proportion of MHW and habitat size and joining CFG & MHW monthly metrics 

library(here)
library(tidyverse)
library(heatwaveR)
library(sf)
library(patchwork)
library(raster)
world <- ne_countries(scale = "medium", returnclass = "sf")

sf_use_s2(FALSE)# need to do this to remove spherical geometry

#####################
# northern mgmt areas
#####################

# load in both CFG monthly metrics & northern region MHW metrics
NWA_PLL_MHWmetrics_northern<-here("data","mgmt_area_metrics","NWA_PLL","NWA_MHW_monthly_metrics_northern.rds") %>% readRDS()

NWA_PLL_CFGmetrics<-here("data","mgmt_area_metrics","NWA_PLL",
                         "NWA_PLL_CFGmetrics_monthly.rds") %>% readRDS()


# filtering down and calculating the proportion of habitat as MHW size
NWAPLL_CFGmetrics_northern<-NWA_PLL_CFGmetrics %>% 
  dplyr::select(date,total.area,mgmt_area,total.area.mgmt.area, 
                COGx, COGy, COGx.sd, COGy.sd,X25,X75,Y25,Y75,n.cell_habitat,
                n.cell_nohabitat,n.cell_total) %>% 
  rename("NWA_PLL.total.area"="total.area") %>% 
  mutate(prop_habitat_area = NWA_PLL.total.area/total.area.mgmt.area,
         prop_habitat_cells = n.cell_habitat / n.cell_total)

NWAPLL_MHWmetrics_northern<-NWA_PLL_MHWmetrics_northern %>%
  dplyr::select(date,total.area,total.area.mgmt.area_MHW, 
                mean_SSTa, max_SSTa, mgmt_area,n.cell_MHW,
                n.cell_noMHW,n.cell_totalMHW) %>%
  rename("NWA_MHW.total.area" = "total.area") %>% 
  mutate(prop_MHW_area = NWA_MHW.total.area/total.area.mgmt.area_MHW,
         prop_MHW_cells = n.cell_MHW / n.cell_totalMHW)

# joining the two data sets
NWAPLL_CFG_MHW_northern<-left_join(NWAPLL_CFGmetrics_northern,NWAPLL_MHWmetrics_northern,
                          by= c("date","mgmt_area")) %>% 
  filter(mgmt_area %in% c("MAB","NEC","NED")) %>% 
  mutate(month = lubridate::month(date)) 

saveRDS(NWAPLL_CFG_MHW_northern, here("data","mgmt_area_metrics","NWA_PLL","NWAPLL_CFG_MHW_northern.rds"))

rm(NWA_PLL_CFGmetrics,NWA_PLL_MHWmetrics_northern,NWAPLL_CFG_MHW_northern,
   NWAPLL_CFGmetrics_northern,NWAPLL_MHWmetrics_northern)

#####################
# southern mgmt areas
#####################

# load in both CFG monthly metrics & southern region MHW metrics
NWA_PLL_MHWmetrics_southern<-here("data","mgmt_area_metrics","NWA_PLL","NWA_MHW_monthly_metrics_southern.rds") %>% readRDS()

NWA_PLL_CFGmetrics<-here("data","mgmt_area_metrics","NWA_PLL",
                         "NWA_PLL_CFGmetrics_monthly.rds") %>% readRDS()


# filtering down and calculating the proportion of habitat as MHW size
NWAPLL_CFGmetrics_southern<-NWA_PLL_CFGmetrics %>% 
  dplyr::select(date,total.area,mgmt_area,total.area.mgmt.area, 
                COGx, COGy, COGx.sd, COGy.sd,X25,X75,Y25,Y75,n.cell_habitat,
                n.cell_nohabitat,n.cell_total) %>% 
  rename("NWA_PLL.total.area"="total.area") %>% 
  mutate(prop_habitat_area = NWA_PLL.total.area/total.area.mgmt.area,
         prop_habitat_cells = n.cell_habitat / n.cell_total)

NWAPLL_MHWmetrics_southern<-NWA_PLL_MHWmetrics_southern %>%
  dplyr::select(date,total.area,total.area.mgmt.area_MHW, 
                mean_SSTa, max_SSTa, mgmt_area,n.cell_MHW,
                n.cell_noMHW,n.cell_totalMHW) %>%
  rename("NWA_MHW.total.area" = "total.area") %>% 
  mutate(prop_MHW_area = NWA_MHW.total.area/total.area.mgmt.area_MHW,
         prop_MHW_cells = n.cell_MHW / n.cell_totalMHW)

# joining the two data sets
NWAPLL_CFG_MHW_southern<-left_join(NWAPLL_CFGmetrics_southern,NWAPLL_MHWmetrics_southern,
                                   by= c("date","mgmt_area")) %>% 
  filter(mgmt_area %in% c("GOM", "CAR", "FEC", "SAR","SAB")) %>% 
  mutate(month = lubridate::month(date)) 

saveRDS(NWAPLL_CFG_MHW_southern, here("data","mgmt_area_metrics","NWA_PLL","NWAPLL_CFG_MHW_southern.rds"))

rm(NWA_PLL_CFGmetrics,NWA_PLL_MHWmetrics_southern,NWAPLL_CFG_MHW_southern,
   NWAPLL_CFGmetrics_southern,NWAPLL_MHWmetrics_southern)
