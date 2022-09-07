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

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#trying to clip the management zones to the coast

land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_zones<-st_difference(NWA_PLL_zones, st_union(st_combine(land)))

NEP_TROLL_zones<-here("data","shapefiles","NEP_TROLL","Zones_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% filter(ET_ID != "CP")

NEP_TROLL_zones<-st_difference(NEP_TROLL_zones, st_union(st_combine(land)))

all_zones<-rbind(NWA_PLL_zones,NEP_TROLL_zones)



#############################################
#load in AIS data
#############################################
NEP_TROLL<-here("data","AIS_processed","NEP_TROLL","Pres_Abs_2013to2020_NWA_USA_TROLL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull_v2_enhanced.rds") %>% readRDS() %>% mutate(fleet = "TROLL") %>% 
  dplyr::select(X,Y,Pres_abs,fleet, fishing_hours)

NWA_PLL<-here("data","AIS_processed","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull_v2_enhanced.rds") %>% readRDS() %>% mutate(fleet = "PLL")%>% 
  dplyr::select(X,Y,Pres_abs,fleet, fishing_hours)

all_AIS<-rbind(NWA_PLL,NEP_TROLL)

#need to reduce the resolution to 0.08 degrees to match ALB
res <- 0.5

all_AIS<-all_AIS %>% filter(Pres_abs == 1) %>% 
  mutate(X = floor(X/res) * res + 0.5 * res,
         Y = floor(Y/res) * res + 0.5 * res) %>% 
  group_by(X,Y) %>% 
  summarise(fishing_hours = sum(fishing_hours, na.rm = T), .groups = "drop") 

#############################################
#plotting the AIS occurrences
#############################################


#center coordinates for each mgmt_zone
mgmtzone_centroid<-all_zones %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtzone_centroid<-cbind(all_zones$ET_ID,mgmtzone_centroid)
colnames(mgmtzone_centroid)<-c("mgmt_zone","X","Y")


ggplot() +
  geom_tile(all_AIS ,
             mapping = aes(x=X, y=Y, fill = fishing_hours)) +
  scale_fill_viridis(option = "cividis", trans = "log",
                     name = "Total Fishing\nHours",
                     breaks = c(1,10,100,1000,4000),
                     guide = guide_colorbar(barwidth = 2, 
                                            barheight = 20)) +
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_sf(data = all_zones, color = "black", fill=NA, size = 1)+
  geom_label(data = mgmtzone_centroid[1,], aes(x = X-1, y = Y-2.5, #CAR
                                                 label = mgmt_zone),
                   fontface = "bold")+
  geom_label(data = mgmtzone_centroid[2:3,], aes(x = X, y = Y-1.5,#GOM & FEC
                                                 label = mgmt_zone),
                            fontface = "bold")+
  geom_label(data = mgmtzone_centroid[4,], aes(x = X-5, y = Y,#MAB
                                               label = mgmt_zone),
                            fontface = "bold")+
  geom_label(data = mgmtzone_centroid[5,], aes(x = X-3.5, y = Y+2.5,#NEC
                                               label = mgmt_zone),
                            fontface = "bold")+
  geom_label(data = mgmtzone_centroid[6,], aes(x = X, y = Y+2.5,#NED
                                               label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid[7,], aes(x = X-6, y = Y+1.5,#SAB
                                               label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid[8,], aes(x = X, y = Y+3,#SAR
                                               label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid[9,], aes(x = X+8.5, y = Y,#MT
                                               label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid[10,], aes(x = X+7, y = Y,#EK
                                               label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid[11,], aes(x = X+7, y = Y,#CL
                                               label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid[12,], aes(x = X+6, y = Y,#VN
                                               label = mgmt_zone),
             fontface = "bold")+
  coord_sf(xlim = c(-132, -44), ylim = c(10, 53.5), expand = TRUE)+
  labs(y = "Latitude", x = "Longitude")+theme_bw()
