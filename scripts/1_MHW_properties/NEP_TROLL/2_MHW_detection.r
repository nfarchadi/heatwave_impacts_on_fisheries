library(here)
library(tidyverse)
library(pracma)
library(heatwaveR)
library(sf)
library(patchwork)
library(raster)
library(viridis)

sf_use_s2(FALSE)# need to do this to remove spherical geometry

source(here("scripts","1_MHW_properties","functions","seasonal_threshold.r"))

#mangement areas shapefile
NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


#####
# NED
#####

NED_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="NED",]


NED_oisst<-here("data","water_temp","NEP","oisst",
                "NED_OISST.rds") %>% readRDS() 


#monthly means
NED_oisst_monthly<-NED_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
NED_monthy_clim<-NED_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
NED_anomalies<-NED_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,NED_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon))


#seasonal varying 90 percentile
NED_seasonal<-find_seasonal_threshold(NED_anomalies)

NED_MHW<-bind_rows(NED_seasonal) %>% 
  right_join(.,NED_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(NED_oisst,NED_oisst_monthly,NED_monthy_clim,NED_anomalies,NED_seasonal)

NED_MHW<- NED_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within NED mgmt area
NED_MHW <- st_join(NED_MHW,NED_mgmtarea) %>% as_tibble() %>% na.omit()

rm(NED_mgmtarea)

#####
# NEC
#####

NEC_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="NEC",]


NEC_oisst<-here("data","water_temp","NEP","oisst",
                "NEC_OISST.rds") %>% 
  readRDS() 


#monthly means
NEC_oisst_monthly<-NEC_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
NEC_monthy_clim<-NEC_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
NEC_anomalies<-NEC_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,NEC_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon)) 

#seasonal varying 90 percentile
NEC_seasonal<-find_seasonal_threshold(NEC_anomalies)

NEC_MHW<-bind_rows(NEC_seasonal) %>% 
  right_join(.,NEC_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(NEC_oisst,NEC_oisst_monthly,NEC_monthy_clim,NEC_anomalies,NEC_seasonal)

NEC_MHW<- NEC_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


# find points within NEC mgmt area
NEC_MHW <- st_join(NEC_MHW,NEC_mgmtarea) %>% as_tibble() %>% na.omit()

rm(NEC_mgmtarea)


#####
# MAB
#####

MAB_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="MAB",]


MAB_oisst<-here("data","water_temp","NEP","oisst",
                         "MAB_OISST.rds") %>% readRDS() 

#monthly means
MAB_oisst_monthly<-MAB_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
MAB_monthy_clim<-MAB_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
MAB_anomalies<-MAB_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,MAB_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon)) 

#seasonal varying 90 percentile
MAB_seasonal<-find_seasonal_threshold(MAB_anomalies)

#define heatwave periods
MAB_MHW<-bind_rows(MAB_seasonal) %>% 
  right_join(.,MAB_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(MAB_oisst,MAB_oisst_monthly,MAB_monthy_clim,MAB_anomalies,MAB_seasonal)

MAB_MHW<-MAB_MHW %>% 
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


# find points within MAB mgmt area
MAB_MHW <- st_join(MAB_MHW, MAB_mgmtarea) %>% as_tibble() %>% na.omit()

rm(MAB_mgmtarea)


###################################
# Combine southern management areas
###################################

#doing this separately between northern and southern management areas as it file will be too large to combine into one

MAB_NEC_NED_MHW<-rbind(MAB_MHW,NEC_MHW,NED_MHW)

saveRDS(MAB_NEC_NED_MHW, 
        here("data","oisst","NEP_TROLL","MAB_NEC_NED_MHW.rds"))

rm(MAB_MHW,NEC_MHW,NED_MHW,MAB_NEC_NED_MHW)




#####
# GOM
#####

GOM_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="GOM",]


GOM_oisst<-here("data","water_temp","NEP","oisst",
                "GOM_OISST.rds") %>% readRDS() 


#monthly means
GOM_oisst_monthly<-GOM_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
GOM_monthy_clim<-GOM_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
GOM_anomalies<-GOM_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,GOM_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon)) 


#seasonal varying 90 percentile
GOM_seasonal<-find_seasonal_threshold(GOM_anomalies)

GOM_MHW<-bind_rows(GOM_seasonal) %>% 
  right_join(.,GOM_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(GOM_oisst,GOM_oisst_monthly,GOM_monthy_clim,GOM_anomalies,GOM_seasonal)

GOM_MHW<- GOM_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within GOM mgmt area
GOM_MHW <- st_join(GOM_MHW,GOM_mgmtarea) %>% as_tibble() %>% na.omit()

rm(GOM_mgmtarea)

#####
# CAR
#####

CAR_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="CAR",]


CAR_oisst<-here("data","water_temp","NEP","oisst",
                "CAR_OISST.rds") %>% readRDS() 


#monthly means
CAR_oisst_monthly<-CAR_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
CAR_monthy_clim<-CAR_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
CAR_anomalies<-CAR_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,CAR_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon)) 


#seasonal varying 90 percentile
CAR_seasonal<-find_seasonal_threshold(CAR_anomalies)

CAR_MHW<-bind_rows(CAR_seasonal) %>% 
  right_join(.,CAR_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(CAR_oisst,CAR_oisst_monthly,CAR_monthy_clim,CAR_anomalies,CAR_seasonal)

CAR_MHW<- CAR_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within CAR mgmt area
CAR_MHW <- st_join(CAR_MHW,CAR_mgmtarea) %>% as_tibble() %>% na.omit()

rm(CAR_mgmtarea)

#####
# FEC
#####

FEC_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="FEC",]


FEC_oisst<-here("data","water_temp","NEP","oisst",
                "FEC_OISST.rds") %>% readRDS() 


#monthly means
FEC_oisst_monthly<-FEC_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
FEC_monthy_clim<-FEC_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
FEC_anomalies<-FEC_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,FEC_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon)) 


#seasonal varying 90 percentile
FEC_seasonal<-find_seasonal_threshold(FEC_anomalies)

FEC_MHW<-bind_rows(FEC_seasonal) %>% 
  right_join(.,FEC_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(FEC_oisst,FEC_oisst_monthly,FEC_monthy_clim,FEC_anomalies,FEC_seasonal)

FEC_MHW<- FEC_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within FEC mgmt area
FEC_MHW <- st_join(FEC_MHW,FEC_mgmtarea) %>% as_tibble() %>% na.omit()

rm(FEC_mgmtarea)


#####
# SAR
#####

SAR_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="SAR",]


SAR_oisst<-here("data","water_temp","NEP","oisst",
                "SAR_OISST.rds") %>% readRDS() 


#monthly means
SAR_oisst_monthly<-SAR_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
SAR_monthy_clim<-SAR_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
SAR_anomalies<-SAR_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,SAR_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon)) 

#seasonal varying 90 percentile
SAR_seasonal<-find_seasonal_threshold(SAR_anomalies)

SAR_MHW<-bind_rows(SAR_seasonal) %>% 
  right_join(.,SAR_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(SAR_oisst,SAR_oisst_monthly,SAR_monthy_clim,SAR_anomalies,SAR_seasonal)

SAR_MHW<- SAR_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within SAR mgmt area
SAR_MHW <- st_join(SAR_MHW,SAR_mgmtarea) %>% as_tibble() %>% na.omit()

rm(SAR_mgmtarea)


#####
# SAB
#####

SAB_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="SAB",]


SAB_oisst<-here("data","water_temp","NEP","oisst",
                "SAB_OISST.rds") %>% readRDS() 


#monthly means
SAB_oisst_monthly<-SAB_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
SAB_monthy_clim<-SAB_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
SAB_anomalies<-SAB_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,SAB_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon))

#seasonal varying 90 percentile
SAB_seasonal<-find_seasonal_threshold(SAB_anomalies)

SAB_MHW<-bind_rows(SAB_seasonal) %>% 
  right_join(.,SAB_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(SAB_oisst,SAB_oisst_monthly,SAB_monthy_clim,SAB_anomalies,SAB_seasonal)

SAB_MHW<- SAB_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within SAB mgmt area
SAB_MHW <- st_join(SAB_MHW,SAB_mgmtarea) %>% as_tibble() %>% na.omit()

rm(SAB_mgmtarea)

###################################
# Combine southern management areas
###################################

GOM_CAR_FEC_SAR_SAB_MHW<-rbind(GOM_MHW,CAR_MHW,FEC_MHW,SAR_MHW,SAB_MHW)

saveRDS(GOM_CAR_FEC_SAR_SAB_MHW,
        here("data","oisst","NEP_TROLL",
             "GOM_CAR_FEC_SAR_SAB_MHW.rds"))
