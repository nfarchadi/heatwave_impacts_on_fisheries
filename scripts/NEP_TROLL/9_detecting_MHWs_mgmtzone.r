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
NEP_TROLL_zones<-here("data","shapefiles","NEP_TROLL","Zones_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NEP_TROLL_zones<-st_transform(NEP_TROLL_zones)

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
#Conception
############################################################

Conception_mgmtzone<-NEP_TROLL_zones[NEP_TROLL_zones$ET_ID=="Conception",]


Conception_oisst<-here("data","water_temp","NEP","oisst",
                         "Conception_OISST.rds") %>% readRDS() 

#monthly means
Conception_oisst_monthly<-Conception_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
Conception_monthy_clim<-Conception_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
Conception_anomalies<-Conception_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,Conception_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim)


Conception_anomalies_detrend<-Conception_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
Conception_anomalies_detrend<-Conception_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 
  
Conception_seasonal<-find_seasonal_threshold(Conception_anomalies_detrend)

#define heatwave periods
Conception_MHW<-bind_rows(Conception_seasonal) %>% 
  right_join(.,Conception_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


#graph
clim_plot<-Conception_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,Conception_monthy_clim, by=c("lon","lat","month")) %>% 
  group_by(yearmon) %>%
  summarise(temp_monthly = mean(temp_monthly, na.rm=TRUE),
            temp_clim = mean(temp_clim, na.rm=TRUE)) %>% 
  ggplot()+geom_line(aes(x=yearmon,y=temp_clim), color = "grey60", size = 1)+
  geom_line(aes(x=yearmon,y=temp_monthly), color = "magenta3", size = 1)+ theme_bw()+
  labs(x = "", y=expression("SST"(degree~C)), 
       title = expression(bold("1. Calculate climatology"))) + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2010-01-01"),zoo::as.yearmon("2020-12-31"))) 

anomaly_plot<-Conception_anomalies_detrend %>% 
  group_by(yearmon) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            temp_anomaly = mean(temp_anomaly, na.rm=TRUE)) %>% 
  ggplot()+geom_line(aes(x=yearmon,y=detrend), color = "black", size = 1)+
  geom_line(aes(x=yearmon, y=temp_anomaly), color = "magenta3", size = 1)+
  theme_bw()+
  labs(x = "", y=expression("SST Anomaly "(degree~C)), 
       title = expression(bold("2. Calculate anomalies and detrend"))) + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2010-01-01"),zoo::as.yearmon("2020-12-31"))) 

MHW_plot<-Conception_MHW %>% 
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

rm(Conception_oisst,Conception_oisst_monthly,Conception_monthy_clim,Conception_anomalies,Conception_anomalies_detrend,Conception_seasonal)

Conception_MHW<-Conception_MHW %>% 
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


# find points within Conception mgmt zone
Conception_MHW <- st_join(Conception_MHW, Conception_mgmtzone) %>% as_tibble() %>% na.omit()

rm(Conception_mgmtzone)
############################################################
#Monterey
############################################################

Monterey_mgmtzone<-NEP_TROLL_zones[NEP_TROLL_zones$ET_ID=="Monterey",]


Monterey_oisst<-here("data","water_temp","NEP","oisst",
                "Monterey_OISST.rds") %>% readRDS() 


#monthly means
Monterey_oisst_monthly<-Monterey_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
Monterey_monthy_clim<-Monterey_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
Monterey_anomalies<-Monterey_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,Monterey_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim)


Monterey_anomalies_detrend<-Monterey_anomalies %>%
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
Monterey_anomalies_detrend<-Monterey_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

Monterey_seasonal<-find_seasonal_threshold(Monterey_anomalies_detrend)

Monterey_MHW<-bind_rows(Monterey_seasonal) %>% 
  right_join(.,Monterey_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(Monterey_oisst,Monterey_oisst_monthly,Monterey_monthy_clim,Monterey_anomalies,Monterey_anomalies_detrend,Monterey_seasonal)

Monterey_MHW<- Monterey_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within Monterey mgmt zone
Monterey_MHW <- st_join(Monterey_MHW,Monterey_mgmtzone) %>% as_tibble() %>% na.omit()

rm(Monterey_mgmtzone)
############################################################
#Eureka
############################################################

Eureka_mgmtzone<-NEP_TROLL_zones[NEP_TROLL_zones$ET_ID=="Eureka",]


Eureka_oisst<-here("data","water_temp","NEP","oisst",
                "Eureka_OISST.rds") %>% 
  readRDS() 


#monthly means
Eureka_oisst_monthly<-Eureka_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
Eureka_monthy_clim<-Eureka_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
Eureka_anomalies<-Eureka_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,Eureka_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim)


Eureka_anomalies_detrend<-Eureka_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
Eureka_anomalies_detrend<-Eureka_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

Eureka_seasonal<-find_seasonal_threshold(Eureka_anomalies_detrend)

Eureka_MHW<-bind_rows(Eureka_seasonal) %>% 
  right_join(.,Eureka_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(Eureka_oisst,Eureka_oisst_monthly,Eureka_monthy_clim,Eureka_anomalies,Eureka_anomalies_detrend,Eureka_seasonal)

Eureka_MHW<- Eureka_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


# find points within Eureka mgmt zone
Eureka_MHW <- st_join(Eureka_MHW,Eureka_mgmtzone) %>% as_tibble() %>% na.omit()

rm(Eureka_mgmtzone)

############################################################
#Columbia
############################################################

Columbia_mgmtzone<-NEP_TROLL_zones[NEP_TROLL_zones$ET_ID=="Columbia",]


Columbia_oisst<-here("data","water_temp","NEP","oisst",
                     "Columbia_OISST.rds") %>% readRDS() 


#monthly means
Columbia_oisst_monthly<-Columbia_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
Columbia_monthy_clim<-Columbia_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
Columbia_anomalies<-Columbia_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,Columbia_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim)


Columbia_anomalies_detrend<-Columbia_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
Columbia_anomalies_detrend<-Columbia_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

Columbia_seasonal<-find_seasonal_threshold(Columbia_anomalies_detrend)

Columbia_MHW<-bind_rows(Columbia_seasonal) %>% 
  right_join(.,Columbia_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(Columbia_oisst,Columbia_oisst_monthly,Columbia_monthy_clim,Columbia_anomalies,Columbia_anomalies_detrend,Columbia_seasonal)

Columbia_MHW<- Columbia_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within Columbia mgmt zone
Columbia_MHW <- st_join(Columbia_MHW,Columbia_mgmtzone) %>% as_tibble() %>% na.omit()

rm(Columbia_mgmtzone)
############################################################
#Vancouver
############################################################

Vancouver_mgmtzone<-NEP_TROLL_zones[NEP_TROLL_zones$ET_ID=="Vancouver",]


Vancouver_oisst<-here("data","water_temp","NEP","oisst",
                      "Vancouver_OISST.rds") %>% readRDS() 


#monthly means
Vancouver_oisst_monthly<-Vancouver_oisst %>% 
  mutate(yearmon= zoo::as.yearmon(t)) %>% 
  group_by(lon,lat,yearmon) %>% 
  summarise(temp_monthly = mean(temp,na.rm=TRUE), .groups = "drop")

#climatology
Vancouver_monthy_clim<-Vancouver_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
Vancouver_anomalies<-Vancouver_oisst_monthly %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,Vancouver_monthy_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim)


Vancouver_anomalies_detrend<-Vancouver_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
Vancouver_anomalies_detrend<-Vancouver_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

Vancouver_seasonal<-find_seasonal_threshold(Vancouver_anomalies_detrend)

Vancouver_MHW<-bind_rows(Vancouver_seasonal) %>% 
  right_join(.,Vancouver_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))


rm(Vancouver_oisst,Vancouver_oisst_monthly,Vancouver_monthy_clim,Vancouver_anomalies,Vancouver_anomalies_detrend,Vancouver_seasonal)

Vancouver_MHW<- Vancouver_MHW %>%
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2])


#Keep points only within Vancouver mgmt zone
Vancouver_MHW <- st_join(Vancouver_MHW,Vancouver_mgmtzone) %>% as_tibble() %>% na.omit()

rm(Vancouver_mgmtzone)

############################################################
#Combine
############################################################

Conception_Monterey_Eureka_Columbia_Vancouver_MHW<-rbind(Conception_MHW,Monterey_MHW,Eureka_MHW,Columbia_MHW,Vancouver_MHW)

saveRDS(Conception_Monterey_Eureka_Columbia_Vancouver_MHW, 
        here("data","water_temp","NEP","oisst","Conception_Monterey_Eureka_Columbia_Vancouver_MHW.rds"))

#remove to save space
rm(Conception_MHW,Monterey_MHW,Eureka_MHW,Columbia_MHW,Vancouver_MHW,Conception_Monterey_Eureka_Columbia_Vancouver_MHW)

