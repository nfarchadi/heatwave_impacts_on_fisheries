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


####
# VN
####

VN_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="VN",]


VN_oisst<-here("data","oisst","NEP_TROLL",
                "VN_OISST.rds") %>% readRDS() 


#monthly means
VN_oisst_monthly<-VN_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
VN_monthy_clim<-VN_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
VN_anomalies<-VN_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,VN_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon))


#seasonal varying 90 percentile
VN_seasonal<-find_seasonal_threshold(VN_anomalies)

VN_MHW<-bind_rows(VN_seasonal) %>% 
  right_join(.,VN_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(VN_oisst,VN_oisst_monthly,VN_monthy_clim,VN_anomalies,VN_seasonal)

VN_MHW<- VN_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within VN mgmt area
VN_MHW <- st_join(VN_MHW,VN_mgmtarea) %>% as_tibble() %>% na.omit()

rm(VN_mgmtarea)

####
# CL
####

CL_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="CL",]


CL_oisst<-here("data","oisst","NEP_TROLL",
                "CL_OISST.rds") %>% 
  readRDS() 


#monthly means
CL_oisst_monthly<-CL_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
CL_monthy_clim<-CL_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
CL_anomalies<-CL_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,CL_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon)) 

#seasonal varying 90 percentile
CL_seasonal<-find_seasonal_threshold(CL_anomalies)

CL_MHW<-bind_rows(CL_seasonal) %>% 
  right_join(.,CL_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(CL_oisst,CL_oisst_monthly,CL_monthy_clim,CL_anomalies,CL_seasonal)

CL_MHW<- CL_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


# find points within CL mgmt area
CL_MHW <- st_join(CL_MHW,CL_mgmtarea) %>% as_tibble() %>% na.omit()

rm(CL_mgmtarea)


####
# EK
####

EK_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="EK",]


EK_oisst<-here("data","water_temp","NEP","oisst",
                         "EK_OISST.rds") %>% readRDS() 

#monthly means
EK_oisst_monthly<-EK_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
EK_monthy_clim<-EK_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
EK_anomalies<-EK_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,EK_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon)) 

#seasonal varying 90 percentile
EK_seasonal<-find_seasonal_threshold(EK_anomalies)

#define heatwave periods
EK_MHW<-bind_rows(EK_seasonal) %>% 
  right_join(.,EK_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(EK_oisst,EK_oisst_monthly,EK_monthy_clim,EK_anomalies,EK_seasonal)

EK_MHW<-EK_MHW %>% 
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


# find points within EK mgmt area
EK_MHW <- st_join(EK_MHW, EK_mgmtarea) %>% as_tibble() %>% na.omit()

rm(EK_mgmtarea)


####
# MT
####

MT_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="MT",]


MT_oisst<-here("data","water_temp","NEP","oisst",
                "MT_OISST.rds") %>% readRDS() 


#monthly means
MT_oisst_monthly<-MT_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
MT_monthy_clim<-MT_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
MT_anomalies<-MT_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,MT_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon)) 


#seasonal varying 90 percentile
MT_seasonal<-find_seasonal_threshold(MT_anomalies)

MT_MHW<-bind_rows(MT_seasonal) %>% 
  right_join(.,MT_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(MT_oisst,MT_oisst_monthly,MT_monthy_clim,MT_anomalies,MT_seasonal)

MT_MHW<- MT_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within MT mgmt area
MT_MHW <- st_join(MT_MHW,MT_mgmtarea) %>% as_tibble() %>% na.omit()

rm(MT_mgmtarea)

####
# CP
####

CP_mgmtarea<-NEP_TROLL_areas[NEP_TROLL_areas$ET_ID=="CP",]


CP_oisst<-here("data","water_temp","NEP","oisst",
                "CP_OISST.rds") %>% readRDS() 


#monthly means
CP_oisst_monthly<-CP_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
CP_monthy_clim<-CP_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
CP_anomalies<-CP_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,CP_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim) %>%
  mutate(year = lubridate::year(yearmon)) 


#seasonal varying 90 percentile
CP_seasonal<-find_seasonal_threshold(CP_anomalies)

CP_MHW<-bind_rows(CP_seasonal) %>% 
  right_join(.,CP_anomalies,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(temp_anomaly >= seas, 1, 0))


rm(CP_oisst,CP_oisst_monthly,CP_monthy_clim,CP_anomalies,CP_seasonal)

CP_MHW<- CP_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within CP mgmt area
CP_MHW <- st_join(CP_MHW,CP_mgmtarea) %>% as_tibble() %>% na.omit()

rm(CP_mgmtarea)


##########################
# Combine management areas
##########################

CP_MT_EK_CL_VN_MHW<-rbind(CP_MHW,MT_MHW,EK_MHW,CL_MHW,VN_MHW)

saveRDS(CP_MT_EK_CL_VN_MHW,
        here("data","oisst","NEP_TROLL",
             "CP_MT_EK_CL_VN_MHW.rds"))
