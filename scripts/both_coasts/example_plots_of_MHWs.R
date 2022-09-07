library(here)
library(sf)
library(tidyverse)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(cmocean)
library(zoo)

world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#loading in northern region data
Conception_Monterey_Eureka_Columbia_Vancouver_MHW<-here("data","water_temp","NEP"
                                                        ,"oisst", "Conception_Monterey_Eureka_Columbia_Vancouver_MHW.rds") %>% readRDS()

CCS_flavors<-Conception_Monterey_Eureka_Columbia_Vancouver_MHW %>% 
  filter(year %in% c(2015,2019,2020)) %>% 
  mutate(year = factor(year, levels = c(2015,2019,2020))) 

CCS_plot<-CCS_flavors %>% 
  filter(yearmon %in% c(as.yearmon("2015-05-01"),
                        as.yearmon("2019-09-01"), 
                        as.yearmon("2020-10-01"))) %>% 
  ggplot() +
  geom_tile(aes(x=lon,y=lat, fill = temp_anomaly))+
  # geom_point(NWA_PLL_flavors, mapping = aes(x=X, y=Y, color = Geartype), 
  #            shape = 1) +
  geom_sf(data = world, color= "black", fill = "grey") +
  scale_size(range = c(1, 8), name=expression("Proportion\nof MHW"(km^2)))+
  coord_sf(xlim = c(-135, -117), ylim = c(32.5, 54), expand = TRUE) +
  theme_bw()+
  labs(x = "Longitude",
       y = "Latitude")+
  labs(fill = "SSTa\nIntensity (°C)")+
  facet_wrap(~zoo::as.Date(yearmon))+
  scale_fill_cmocean(name="balance",
                     limits = c(-2,2),oob = scales::squish,
                     labels=c("\u2264 -2","-1","0","1","\u2265 2"))+
  #scale_color_manual(values = "black")+
  theme(legend.position="right")+
  theme(strip.background = element_blank())
  #guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5))

NWA_2012_05<-raster("E:/OISST/SSTa/oisst_ssta_2012-05.nc") %>% 
  rasterToPoints() %>% as.data.frame() %>% 
  mutate(date = as.Date("2012-05-01")) %>% 
  rename("lon" = "x",
         "lat" = "y",
         "temp_anomaly" = "layer")

NWA_2016_03<-raster("E:/OISST/SSTa/oisst_ssta_2016-03.nc") %>% 
  rasterToPoints() %>% as.data.frame() %>% 
  mutate(date = as.Date("2016-03-01")) %>% 
  rename("lon" = "x",
         "lat" = "y",
         "temp_anomaly" = "layer")

NWA_2020_09<-raster("E:/OISST/SSTa/oisst_ssta_2020-09.nc") %>% 
  rasterToPoints() %>% as.data.frame() %>% 
  mutate(date = as.Date("2020-09-01")) %>% 
  rename("lon" = "x",
         "lat" = "y",
         "temp_anomaly" = "layer")


CCS_flavors2<-CCS_flavors %>% 
  filter(yearmon %in% c(as.yearmon("2015-05-01"),
                        as.yearmon("2019-09-01"), 
                        as.yearmon("2020-10-01"))) %>% 
  mutate(date = zoo::as.Date(yearmon)) %>% 
  dplyr::select(lon,lat,temp_anomaly,date)
  

both_coasts_flavors<-rbind(NWA_2012_05,NWA_2016_03,NWA_2020_09, CCS_flavors2)


both_coasts_flavors %>% 
  ggplot() +
  geom_tile(aes(x=lon,y=lat, fill = temp_anomaly))+
  # geom_point(NWA_PLL_flavors, mapping = aes(x=X, y=Y, color = Geartype), 
  #            shape = 1) +
  #geom_sf(data = world, color= "black", fill = "grey") +
  #coord_sf(xlim = c(-100, -40), ylim = c(10, 50), expand = TRUE) +
  ggspatial::geom_spatial_polygon(as_Spatial(world), mapping = aes())+
  theme_bw()+
  labs(x = "Longitude",
       y = "Latitude")+
  labs(fill = "SSTa\nIntensity (°C)")+
  facet_wrap(~date, scales = "free")+
  scale_fill_cmocean(name="balance",
                     limits = c(-2,2),oob = scales::squish,
                     labels=c("\u2264 -2","-1","0","1","\u2265 2"))+
  #scale_color_manual(values = "black")+
  theme(legend.position="right")+
  theme(strip.background = element_blank())
#guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5))