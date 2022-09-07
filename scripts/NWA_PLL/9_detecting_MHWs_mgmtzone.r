library(here)
library(tidyverse)
library(pracma)
library(heatwaveR)
library(sf)
library(patchwork)
library(raster)
library(viridis)

sf_use_s2(FALSE)# need to do this to remove spherical geometry

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_zones<-st_transform(NWA_PLL_zones)

#function to find the seasonal threshold
find_seasonal_threshold<-function(data,thresh = .90){
  
  seas_list<-list()
  
  for (i in 1:12){
    if(i == 1){
      sea_month_df<-data %>%
        group_by(lat,lon) %>%
        filter(year <= 2011 & (month == 12 | month <=2)) %>% 
        summarise(seas = quantile(detrend, thresh, na.rm=TRUE),.groups = "drop") %>% 
        mutate(month = 1)
      
    } else if(i == 12){
      sea_month_df<-data %>%
        group_by(lat,lon) %>%
        filter(year <= 2011 & (month == 1 | month >= 11)) %>% 
        summarise(seas = quantile(detrend, thresh, na.rm=TRUE),.groups = "drop") %>% 
        mutate(month = i)
    } else{
      
      sea_month_df<-data %>%
        group_by(lat,lon) %>%
        filter(year <= 2011 & month >=i-1 & month <= i+1) %>% 
        summarise(seas = quantile(detrend, thresh, na.rm=TRUE),.groups = "drop") %>% 
        mutate(month = i)
    }
    
    seas_list[[length(seas_list)+1]]=sea_month_df
  }
  return(seas_list)
}
############################################################
#MAB
############################################################

MAB_mgmtzone<-NWA_PLL_zones[NWA_PLL_zones$ET_ID=="MAB",]


MAB_oisst<-here("data","water_temp","NWA","oisst",
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
  mutate(temp_anomaly = temp_monthly - temp_clim)


MAB_anomalies_detrend<-MAB_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
MAB_anomalies_detrend<-MAB_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 
  
MAB_seasonal<-find_seasonal_threshold(MAB_anomalies_detrend)

#define heatwave periods
MAB_MHW<-bind_rows(MAB_seasonal) %>% 
  right_join(.,MAB_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


#graph
clim_plot<-MAB_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,MAB_monthy_clim, by=c("lon","lat","month")) %>% 
  group_by(yearmon) %>%
  summarise(temp_monthly = mean(temp_monthly, na.rm=TRUE),
            temp_clim = mean(temp_clim, na.rm=TRUE)) %>% 
  ggplot()+geom_line(aes(x=yearmon,y=temp_clim), color = "grey60", size = 1)+
  geom_line(aes(x=yearmon,y=temp_monthly), color = "magenta3", size = 1)+ theme_bw()+
  labs(x = "", y=expression("SST"(degree~C)), 
       title = expression(bold("1. Calculate climatology"))) + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2010-01-01"),zoo::as.yearmon("2020-12-31"))) 

anomaly_plot<-MAB_anomalies_detrend %>% 
  group_by(yearmon) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            temp_anomaly = mean(temp_anomaly, na.rm=TRUE)) %>% 
  ggplot()+geom_line(aes(x=yearmon,y=detrend), color = "black", size = 1)+
  geom_line(aes(x=yearmon, y=temp_anomaly), color = "magenta3", size = 1)+
  theme_bw()+
  labs(x = "", y=expression("SST Anomaly "(degree~C)), 
       title = expression(bold("2. Calculate anomalies and detrend"))) + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2010-01-01"),zoo::as.yearmon("2020-12-31"))) 

MHW_plot<-MAB_MHW %>% 
  group_by(yearmon) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  mutate(MHW = ifelse(detrend >= seas, 1, 0)) %>% 
  ggplot(aes(x = yearmon)) +
  geom_flame(aes(y = detrend, y2 = seas)) +
  geom_line(aes(y = detrend, colour = "temp"))+
  geom_line(aes(y = seas, colour = "thresh"),linetype = "dashed", size = .5)+
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", 
                                 "thresh" =  "forestgreen"))+
  theme_bw()+ theme(legend.position="none") +
  labs(x = "", y=expression("SST Anomaly "(degree~C)), 
       title = expression(bold("3. Identify MHWs"))) + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2010-01-01"),zoo::as.yearmon("2020-12-31")))

clim_plot / anomaly_plot / MHW_plot

rm(MAB_oisst,MAB_oisst_monthly,MAB_monthy_clim,MAB_anomalies,MAB_anomalies_detrend,MAB_seasonal)

MAB_MHW<-MAB_MHW %>% 
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


# find points within MAB mgmt zone
MAB_MHW <- st_join(MAB_MHW, MAB_mgmtzone) %>% as_tibble() %>% na.omit()

rm(MAB_mgmtzone)
############################################################
#NED
############################################################

NED_mgmtzone<-NWA_PLL_zones[NWA_PLL_zones$ET_ID=="NED",]


NED_oisst<-here("data","water_temp","NWA","oisst",
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
  mutate(temp_anomaly = temp_monthly - temp_clim)


NED_anomalies_detrend<-NED_anomalies %>%
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
NED_anomalies_detrend<-NED_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

NED_seasonal<-find_seasonal_threshold(NED_anomalies_detrend)

NED_MHW<-bind_rows(NED_seasonal) %>% 
  right_join(.,NED_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(NED_oisst,NED_oisst_monthly,NED_monthy_clim,NED_anomalies,NED_anomalies_detrend,NED_seasonal)

NED_MHW<- NED_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within NED mgmt zone
NED_MHW <- st_join(NED_MHW,NED_mgmtzone) %>% as_tibble() %>% na.omit()

rm(NED_mgmtzone)
############################################################
#NEC
############################################################

NEC_mgmtzone<-NWA_PLL_zones[NWA_PLL_zones$ET_ID=="NEC",]


NEC_oisst<-here("data","water_temp","NWA","oisst",
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
  mutate(temp_anomaly = temp_monthly - temp_clim)


NEC_anomalies_detrend<-NEC_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
NEC_anomalies_detrend<-NEC_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

NEC_seasonal<-find_seasonal_threshold(NEC_anomalies_detrend)

NEC_MHW<-bind_rows(NEC_seasonal) %>% 
  right_join(.,NEC_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(NEC_oisst,NEC_oisst_monthly,NEC_monthy_clim,NEC_anomalies,NEC_anomalies_detrend,NEC_seasonal)

NEC_MHW<- NEC_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


# find points within NEC mgmt zone
NEC_MHW <- st_join(NEC_MHW,NEC_mgmtzone) %>% as_tibble() %>% na.omit()

rm(NEC_mgmtzone)

MAB_NEC_NED_MHW<-rbind(MAB_MHW,NEC_MHW,NED_MHW)

saveRDS(MAB_NEC_NED_MHW, 
        here("data","water_temp","NWA","oisst","MAB_NEC_NED_MHW.rds"))

rm(MAB_MHW,NEC_MHW,NED_MHW,MAB_NEC_NED_MHW)
############################################################
#GOM
############################################################

GOM_mgmtzone<-NWA_PLL_zones[NWA_PLL_zones$ET_ID=="GOM",]


GOM_oisst<-here("data","water_temp","NWA","oisst",
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
  mutate(temp_anomaly = temp_monthly - temp_clim)


GOM_anomalies_detrend<-GOM_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
GOM_anomalies_detrend<-GOM_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

GOM_seasonal<-find_seasonal_threshold(GOM_anomalies_detrend)

GOM_MHW<-bind_rows(GOM_seasonal) %>% 
  right_join(.,GOM_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(GOM_oisst,GOM_oisst_monthly,GOM_monthy_clim,GOM_anomalies,GOM_anomalies_detrend,GOM_seasonal)

GOM_MHW<- GOM_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within GOM mgmt zone
GOM_MHW <- st_join(GOM_MHW,GOM_mgmtzone) %>% as_tibble() %>% na.omit()

rm(GOM_mgmtzone)
############################################################
#CAR
############################################################

CAR_mgmtzone<-NWA_PLL_zones[NWA_PLL_zones$ET_ID=="CAR",]


CAR_oisst<-here("data","water_temp","NWA","oisst",
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
  mutate(temp_anomaly = temp_monthly - temp_clim)


CAR_anomalies_detrend<-CAR_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
CAR_anomalies_detrend<-CAR_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

CAR_seasonal<-find_seasonal_threshold(CAR_anomalies_detrend)

CAR_MHW<-bind_rows(CAR_seasonal) %>% 
  right_join(.,CAR_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(CAR_oisst,CAR_oisst_monthly,CAR_monthy_clim,CAR_anomalies,CAR_anomalies_detrend,CAR_seasonal)

CAR_MHW<- CAR_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within CAR mgmt zone
CAR_MHW <- st_join(CAR_MHW,CAR_mgmtzone) %>% as_tibble() %>% na.omit()

rm(CAR_mgmtzone)
############################################################
#FEC
############################################################

FEC_mgmtzone<-NWA_PLL_zones[NWA_PLL_zones$ET_ID=="FEC",]


FEC_oisst<-here("data","water_temp","NWA","oisst",
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
  mutate(temp_anomaly = temp_monthly - temp_clim)


FEC_anomalies_detrend<-FEC_anomalies %>%
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
FEC_anomalies_detrend<-FEC_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

FEC_seasonal<-find_seasonal_threshold(FEC_anomalies_detrend)

FEC_MHW<-bind_rows(FEC_seasonal) %>% 
  right_join(.,FEC_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(FEC_oisst,FEC_oisst_monthly,FEC_monthy_clim,FEC_anomalies,FEC_anomalies_detrend,FEC_seasonal)

FEC_MHW<- FEC_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within FEC mgmt zone
FEC_MHW <- st_join(FEC_MHW,FEC_mgmtzone) %>% as_tibble() %>% na.omit()

rm(FEC_mgmtzone)
############################################################
#SAR
############################################################

SAR_mgmtzone<-NWA_PLL_zones[NWA_PLL_zones$ET_ID=="SAR",]


SAR_oisst<-here("data","water_temp","NWA","oisst",
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
  mutate(temp_anomaly = temp_monthly - temp_clim)


SAR_anomalies_detrend<-SAR_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
SAR_anomalies_detrend<-SAR_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

SAR_seasonal<-find_seasonal_threshold(SAR_anomalies_detrend)

SAR_MHW<-bind_rows(SAR_seasonal) %>% 
  right_join(.,SAR_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(SAR_oisst,SAR_oisst_monthly,SAR_monthy_clim,SAR_anomalies,SAR_anomalies_detrend,SAR_seasonal)

SAR_MHW<- SAR_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within SAR mgmt zone
SAR_MHW <- st_join(SAR_MHW,SAR_mgmtzone) %>% as_tibble() %>% na.omit()

rm(SAR_mgmtzone)
############################################################
#SAB
############################################################

SAB_mgmtzone<-NWA_PLL_zones[NWA_PLL_zones$ET_ID=="SAB",]


SAB_oisst<-here("data","water_temp","NWA","oisst",
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
  mutate(temp_anomaly = temp_monthly - temp_clim)


SAB_anomalies_detrend<-SAB_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
SAB_anomalies_detrend<-SAB_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

SAB_seasonal<-find_seasonal_threshold(SAB_anomalies_detrend)

SAB_MHW<-bind_rows(SAB_seasonal) %>% 
  right_join(.,SAB_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(SAB_oisst,SAB_oisst_monthly,SAB_monthy_clim,SAB_anomalies,SAB_anomalies_detrend,SAB_seasonal)

SAB_MHW<- SAB_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within SAB mgmt zone
SAB_MHW <- st_join(SAB_MHW,SAB_mgmtzone) %>% as_tibble() %>% na.omit()

rm(SAB_mgmtzone)

GOM_CAR_FEC_SAR_SAB_MHW<-rbind(GOM_MHW,CAR_MHW,FEC_MHW,SAR_MHW,SAB_MHW)

saveRDS(GOM_CAR_FEC_SAR_SAB_MHW,
        here("data","water_temp","NWA","oisst",
             "GOM_CAR_FEC_SAR_SAB_MHW.rds"))
