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
NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


#################################################
# Classifying months mgmt area was in a MHW state
#################################################

# loading in northern mgmt area MHW data
CP_MT_EK_CL_VN_MHW<-here("data","oisst", "NEP_TROLL",
                      "CP_MT_EK_CL_VN_MHW.rds") %>% readRDS()

CP_MT_EK_CL_VN_MHW$yearmon<-zoo::as.yearmon(CP_MT_EK_CL_VN_MHW$yearmon)

# averaging over the pixels per mgmt zone and yearmon
MHW_mgmtarea_all<-CP_MT_EK_CL_VN_MHW %>%
  filter(yearmon >= 2012) %>%
  group_by(yearmon, ET_ID) %>%
  summarise(temp_anomaly = mean(temp_anomaly, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  mutate(MHW = ifelse(temp_anomaly >= seas, 1, 0)) %>%
  rename("mgmt_area"="ET_ID",
         "date"="yearmon")

MHW_mgmtarea_all$mgmt_area<-factor(MHW_mgmtarea_all$mgmt_area,
                                   levels = c("VN","CL","EK",
                                              "MT","CP"))

rm(CP_MT_EK_CL_VN_MHW)


# combine NEP TROLL CFG MHW metric data sets for both regions 
NEPTROLL_CFG_MHW<-here("data","mgmt_area_metrics","NEP_TROLL",
                              "NEPTROLL_CFG_MHW.rds") %>% readRDS() %>% 
  ungroup() #in case it is grouped 


# tiding up the data
NEPTROLL_CFG_MHW<-NEPTROLL_CFG_MHW %>% mutate(year = lubridate::year(date))

NEPTROLL_CFG_MHW$mgmt_area<-factor(NEPTROLL_CFG_MHW$mgmt_area,
                                       levels = c("VN","CL","EK",
                                                  "MT","CP"))


# joining the two so now there is column indicating when a month is classified as MHW per mgmt area
NEPTROLL_CFG_MHW_total<-NEPTROLL_CFG_MHW %>% 
  left_join(., MHW_mgmtarea_all, by=c("mgmt_area","date")) 


#########################################
# Finding the event duration for each MHW
#########################################

#indexing each event by mgmt_zone
NEPTROLL_CFG_MHW_total<-NEPTROLL_CFG_MHW_total %>%
  dplyr::select(date,mgmt_area,MHW) %>%
  group_by(mgmt_area) %>%
  mutate(event_index = data.table::rleid(MHW)) %>% ungroup() %>% 
  left_join(., NEPTROLL_CFG_MHW_total, by=c("date", "mgmt_area","MHW"))

#counting the duration months and the sequence for each event
NEPTROLL_CFG_MHW_total<-NEPTROLL_CFG_MHW_total %>%
  group_by(mgmt_area, event_index, MHW) %>% 
  summarise(duration_months = n(), .groups = "drop") %>%
  ungroup() %>% 
  left_join(., NEPTROLL_CFG_MHW_total, 
            by=c("mgmt_area","MHW", "event_index")) %>% 
  group_by(mgmt_area, event_index) %>%
  mutate(sequence_months = seq_along(event_index)) %>% 
  ungroup() 



############################################
# calculate % change in core fishing grounds
############################################

NEPTROLL_CFG_MHW_total<-NEPTROLL_CFG_MHW_total %>% 
  group_by(mgmt_area,month) %>% 
  mutate(year = lubridate::year(date),
         month_mean = mean(n.cell_habitat, na.rm=TRUE),
         NEP_TROLL.CFG.change =
           ((n.cell_habitat - month_mean)/month_mean)*100) %>% 
  ungroup()


NEPTROLL_CFG_MHW_total$mgmt_area<-factor(NEPTROLL_CFG_MHW_total$mgmt_area,
                                       levels = c("VN","CL","EK",
                                                  "MT","CP"))


saveRDS(NEPTROLL_CFG_MHW_total, 
        here("data","mgmt_area_metrics","NEP_TROLL","NEPTROLL_CFG_MHW_total.rds"))
