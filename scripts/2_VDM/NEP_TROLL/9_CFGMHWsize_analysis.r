# calculating proportion of MHW and habitat size and joining CFG & MHW monthly metrics 

library(here)
library(tidyverse)
library(heatwaveR)
library(sf)
library(patchwork)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")

sf_use_s2(FALSE)# need to do this to remove spherical geometry

###############
# NEP mgmt area
###############

# load in both CFG monthly metrics & MHW metrics
NEP_TROLL_MHWmetrics<-here("data","mgmt_area_metrics","NEP_TROLL","NEP_MHW_monthly_metrics.rds") %>% readRDS()

NEP_TROLL_CFGmetrics<-here("data","mgmt_area_metrics","NEP_TROLL",
                         "NEP_TROLL_CFGmetrics_monthly.rds") %>% readRDS()


# filtering down and calculating the proportion of habitat as MHW size
NEPTROLL_CFGmetrics<-NEP_TROLL_CFGmetrics %>% 
  dplyr::select(date,total.area,mgmt_area,total.area.mgmt.area, 
                COGx, COGy, COGx.sd, COGy.sd,X25,X75,Y25,Y75,n.cell_habitat,
                n.cell_nohabitat,n.cell_total) %>% 
  rename("NEP_TROLL.total.area"="total.area") %>% 
  mutate(prop_habitat_area = NEP_TROLL.total.area/total.area.mgmt.area,
         prop_habitat_cells = n.cell_habitat / n.cell_total)

NEPTROLL_MHWmetrics<-NEP_TROLL_MHWmetrics %>%
  dplyr::select(date,total.area,total.area.mgmt.area_MHW, 
                mean_SSTa, max_SSTa, mgmt_area,n.cell_MHW,
                n.cell_noMHW,n.cell_totalMHW) %>%
  rename("NEP_MHW.total.area" = "total.area") %>% 
  mutate(prop_MHW_area = NEP_MHW.total.area/total.area.mgmt.area_MHW,
         prop_MHW_cells = n.cell_MHW / n.cell_totalMHW)

# joining the two data sets
NEPTROLL_CFG_MHW<-left_join(NEPTROLL_CFGmetrics,NEPTROLL_MHWmetrics,
                          by= c("date","mgmt_area")) %>% 
  mutate(month = lubridate::month(date)) 

saveRDS(NEPTROLL_CFG_MHW, here("data","mgmt_area_metrics","NEP_TROLL","NEPTROLL_CFG_MHW.rds"))

rm(NEP_TROLL_CFGmetrics,NEP_TROLL_MHWmetrics,NEPTROLL_CFG_MHW,
   NEPTROLL_CFGmetrics,NEPTROLL_MHWmetrics)
