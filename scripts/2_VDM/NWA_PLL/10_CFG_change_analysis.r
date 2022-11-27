# classifying MHW months and calculating % changein core fishing grounds

library(here)
library(sf)
library(tidyverse)
library(viridis)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(heatwaveR)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NWA_PLL_areas<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


#################################################
# Classifying months mgmt area was in a MHW state
#################################################

# loading in northern mgmt area MHW data
MAB_NEC_NED_MHW<-here("data","water_temp","NWA","oisst",
                      "MAB_NEC_NED_MHW.rds") %>% readRDS()

MAB_NEC_NED_MHW$yearmon<-zoo::as.yearmon(MAB_NEC_NED_MHW$yearmon)

# averaging over the pixels per mgmt zone and yearmon
MAB_NEC_NED_MHW<-MAB_NEC_NED_MHW %>%
  filter(yearmon >= 2012) %>%
  group_by(yearmon, ET_ID) %>%
  summarise(temp_anomaly = mean(temp_anomaly, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  mutate(MHW = ifelse(temp_anomaly >= seas, 1, 0)) 

# loading in southern region
GOM_CAR_FEC_SAR_SAB_MHW<-here("data","water_temp","NWA","oisst",
                              "GOM_CAR_FEC_SAR_SAB_MHW.rds") %>% readRDS()

# averaging over the pixels per mgmt zone and yearmon
GOM_CAR_FEC_SAR_SAB_MHW<-GOM_CAR_FEC_SAR_SAB_MHW %>%
  filter(yearmon >= 2012) %>%
  group_by(yearmon, ET_ID) %>%
  summarise(temp_anomaly = mean(temp_anomaly, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  mutate(MHW = ifelse(temp_anomaly >= seas, 1, 0))


# combine
MHW_mgmtarea_all<-rbind(MAB_NEC_NED_MHW,GOM_CAR_FEC_SAR_SAB_MHW) %>%
  rename("mgmt_area"="ET_ID",
         "date"="yearmon")

MHW_mgmtarea_all$mgmt_area<-factor(MHW_mgmtarea_all$mgmt_area,
                                   levels = c("NED","NEC","MAB",
                                              "SAB","SAR","FEC",
                                              "GOM","CAR"))

rm(MAB_NEC_NED_MHW,GOM_CAR_FEC_SAR_SAB_MHW)


# combine NWA PLL CFG MHW metric data sets for both regions 
NWAPLL_CFG_MHW_northern<-here("data","mgmt_area_metrics","NWA_PLL",
                              "NWAPLL_CFG_MHW_northern.rds") %>% readRDS()

NWAPLL_CFG_MHW_southern<-here("data","mgmt_area_metrics","NWA_PLL",
                              "NWAPLL_CFG_MHW_southern.rds") %>% readRDS()

NWAPLL_CFG_MHW_total<-rbind(NWAPLL_CFG_MHW_northern,NWAPLL_CFG_MHW_southern) %>% 
  ungroup() #in case it is grouped 


# tiding up the data
NWAPLL_CFG_MHW_total<-NWAPLL_CFG_MHW_total %>% mutate(year = lubridate::year(date))

NWAPLL_CFG_MHW_total$mgmt_area<-factor(NWAPLL_CFG_MHW_total$mgmt_area,
                                       levels = c("NED","NEC","MAB",
                                                  "SAB","SAR","FEC",
                                                  "GOM","CAR"))


# joining the two so now there is column indicating when a month is classified as MHW per mgmt area
NWAPLL_CFG_MHW_total<-NWAPLL_CFG_MHW_total %>% 
  left_join(., MHW_mgmtarea_all, by=c("mgmt_area","date"))


#########################################
# Finding the event duration for each MHW
#########################################

#indexing each event by mgmt_zone
NWAPLL_CFG_MHW_total<-NWAPLL_CFG_MHW_total %>%
  dplyr::select(date,mgmt_area,MHW) %>%
  group_by(mgmt_area) %>%
  mutate(event_index = data.table::rleid(MHW)) %>% ungroup() %>% 
  left_join(., NWAPLL_CFG_MHW_total, by=c("date", "mgmt_area","MHW"))

#counting the duration months and the sequence for each event
NWAPLL_CFG_MHW_total<-NWAPLL_CFG_MHW_total %>%
  group_by(mgmt_area, event_index, MHW) %>% 
  summarise(duration_months = n(), .groups = "drop") %>%
  ungroup() %>% 
  left_join(., NWAPLL_CFG_MHW_total, 
            by=c("mgmt_area","MHW", "event_index")) %>% 
  group_by(mgmt_area, event_index) %>%
  mutate(sequence_months = seq_along(event_index)) %>% 
  ungroup() 



############################################
# calculate % change in core fishing grounds
############################################

NWAPLL_CFG_MHW_total<-NWAPLL_CFG_MHW_total %>% 
  group_by(mgmt_area,month) %>% 
  mutate(year = lubridate::year(date),
         month_mean = mean(n.cell_habitat, na.rm=TRUE),
         NWA_PLL.CFG.change =
           ((n.cell_habitat - month_mean)/month_mean)*100) %>% 
  ungroup()


NWAPLL_CFG_MHW_total$mgmt_area<-factor(NWAPLL_CFG_MHW_total$mgmt_area,
                                       levels = c("NED","NEC","MAB",
                                                  "SAB","SAR","FEC",
                                                  "GOM","CAR"))


saveRDS(NWAPLL_CFG_MHW_total, 
        here("data","mgmt_area_metrics","NWA_PLL","NWAPLL_CFG_MHW_total.rds"))
