library(tidyverse)
library(patchwork)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(cmocean)
library(zoo)
library(here)
library(sf)
library(tidyverse)
library(viridis)

world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#trying to clip the management zones to the coast

land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_zones<-st_difference(NWA_PLL_zones, st_union(st_combine(land)))

NWA_PLL_pred<-here("data","Monthly_Spatial_Predictions",
                   "NWA_PLL","NWA_PLL_monthly_predictions_2012to2020.nc") %>% 
  raster::stack()

time_vec <- seq.POSIXt(as.POSIXct('2012-01-01', tz='UTC'), as.POSIXct('2020-12-01', tz='UTC'), by='month')

names(NWA_PLL_pred) <- time_vec

may_2012_NWA_PLL<-rasterToPoints(NWA_PLL_pred[[5]]) %>% as.data.frame() %>%  
  rename("suitability" = "X2012.05.01")

#center coordinates for each mgmt_zone
mgmtzone_centroid_NWA<-NWA_PLL_zones %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtzone_centroid_NWA<-cbind(NWA_PLL_zones$ET_ID,mgmtzone_centroid_NWA)
colnames(mgmtzone_centroid_NWA)<-c("mgmt_zone","X","Y")

NWA_pred<-may_2012_NWA_PLL %>% 
  ggplot()+
  geom_tile(aes(x=x,y=y,fill=suitability))+
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_sf(data = NWA_PLL_zones, color = "black", fill=NA, size = 1)+ 
  scale_fill_cmocean(name="haline",
                     limits = c(0,1),
                     labels=c("Poor","","","","Good"))+
  coord_sf(xlim = c(-97.5, -40), ylim = c(11.5, 48.5), expand = TRUE) +
  labs(x="",y="",fill = "Suitable\nFishing\nGrounds")+
  theme_bw()+theme(strip.background = element_blank(),
                   legend.spacing.y = unit(0.5, 'cm'))+
  geom_label(data = mgmtzone_centroid_NWA, aes(x = X, y = Y,
                                               label = mgmt_zone),
             alpha = 0.5,fontface = "bold")



################################################
#SSTa raster map of a certain MHW (MHW Intensity)
################################################
time_vec <- seq.POSIXt(as.POSIXct('2012-01-01', tz='UTC'), as.POSIXct('2020-12-01', tz='UTC'), by='month')

fList <- list.files("E:/OISST/SSTa", full.names = TRUE, pattern = ".nc")

fList_years <- c()
for (i in 1:length(fList)) fList_years[i] <- substr(fList[i], 26, 29)

ssta_stack <- raster::stack(fList[which(fList_years %in% lubridate::year(time_vec))])

names(ssta_stack) <- time_vec

may_2012_mhw_NWA<-rasterToPoints(ssta_stack[[5]]) %>% as.data.frame() %>%  
  rename("SSTa" = "X2012.05.01")


#center coordinates for each mgmt_zone
mgmtzone_centroid_NWA<-NWA_PLL_zones %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtzone_centroid_NWA<-cbind(NWA_PLL_zones$ET_ID,mgmtzone_centroid_NWA)
colnames(mgmtzone_centroid_NWA)<-c("mgmt_zone","X","Y")

NWA_SST<-may_2012_mhw_NWA %>% 
  ggplot() +
  geom_tile(aes(x=x,y=y, fill = SSTa))+
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_sf(data = NWA_PLL_zones, color = "black", fill = NA, size = 1)+
  coord_sf(xlim = c(-97.5, -40), ylim = c(11.5, 48.5), expand = TRUE) +
  theme_bw()+
  labs(x = "",
       y = "")+
  labs(fill = "SSTa (°C)")+
  scale_fill_cmocean(name="balance",
                     limits = c(-2,2),oob = scales::squish,
                     labels=c("\u2264 -2","-1","0","1","\u2265 2"))+
  #scale_color_manual(values = "black")+
  theme(legend.position="right")+
  theme(strip.background = element_blank())+
  geom_label(data = mgmtzone_centroid_NWA, aes(x = X, y = Y,
                                                   label = mgmt_zone),
             alpha = 0.5,fontface = "bold")

NWA_SST + NWA_pred


