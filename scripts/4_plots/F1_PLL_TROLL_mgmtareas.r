# Fig 1. Fishing effort and management areas for both U.S. pelagic fleets

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
#load in mgmt area shapefiles
#############################################

#mangement areas shapefile
NWA_PLL_areas<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#trying to clip the management areas to the coast

land<-here("data","shapefiles","Land","GSHHS_l_L1.shp") %>% st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_areas<-st_difference(NWA_PLL_areas, st_union(st_combine(land)))

NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% filter(ET_ID != "CP")

NEP_TROLL_areas<-st_difference(NEP_TROLL_areas, st_union(st_combine(land)))

all_areas<-rbind(NWA_PLL_areas,NEP_TROLL_areas)



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


#center coordinates for each mgmt_area
mgmtarea_centroid<-all_areas %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtarea_centroid<-cbind(all_areas$ET_ID,mgmtarea_centroid)
colnames(mgmtarea_centroid)<-c("mgmt_area","X","Y")


ggplot() +
  geom_tile(all_AIS ,
             mapping = aes(x=X, y=Y, fill = fishing_hours)) +
  scale_fill_viridis(option = "cividis", trans = "log",
                     name = "Total Fishing\nHours",
                     breaks = c(1,10,100,1000,4000),
                     guide = guide_colorbar(barwidth = 1, 
                                            barheight = 15)) +
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_sf(data = all_areas, color = "black", fill=NA, size = 1)+
  geom_label(data = mgmtarea_centroid[1,], aes(x = X-1.5, y = Y-3, #CAR
                                                 label = mgmt_area),
                   fontface = "bold")+
  geom_label(data = mgmtarea_centroid[2:3,], aes(x = X, y = Y-1.5,#GOM & FEC
                                                 label = mgmt_area),
                            fontface = "bold")+
  geom_label(data = mgmtarea_centroid[4,], aes(x = X-6, y = Y,#MAB
                                               label = mgmt_area),
                            fontface = "bold")+
  geom_label(data = mgmtarea_centroid[5,], aes(x = X-2, y = Y+2.5,#NEC
                                               label = mgmt_area),
                            fontface = "bold")+
  geom_label(data = mgmtarea_centroid[6,], aes(x = X-3, y = Y+2.5,#NED
                                               label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid[7,], aes(x = X-7.5, y = Y+1.5,#SAB
                                               label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid[8,], aes(x = X, y = Y+4.5,#SAR
                                               label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid[9,], aes(x = X+9.5, y = Y,#MT
                                               label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid[10,], aes(x = X+8, y = Y,#EK
                                               label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid[11,], aes(x = X+8, y = Y,#CL
                                               label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid[12,], aes(x = X+8, y = Y,#VN
                                               label = mgmt_area),
             fontface = "bold")+
  coord_sf(xlim = c(-132, -44), ylim = c(10, 53.5), expand = TRUE)+
  labs(y = "Latitude", x = "Longitude")+theme_bw()


ggsave(here("Plots","F1_PLL_TROLL_mgmtareas.png"),
       width = 8, height = 5, units = "in", dpi = 300)
ggsave(here("Plots","F1_PLL_TROLL_mgmtareas.svg"),
       width = 7, height = 5, units = "in", dpi = 300)

