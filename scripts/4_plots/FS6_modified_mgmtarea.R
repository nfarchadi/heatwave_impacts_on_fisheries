# Fig. S6. Management areas for both fishing fleets

library(sf)
library(raster)
library(tidyverse)
library(glue)
library(zoo)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(cmocean)
library(here)
library(patchwork)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry


#################################
#load in NEW mgmt area shapefiles
#################################

#mangement areas shapefile
NWA_PLL_areas<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

#trying to clip the management areas to the coast
land<-here("data","shapefiles","Land","GSHHS_l_L1.shp") %>% st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_areas<-st_difference(NWA_PLL_areas, st_union(st_combine(land)))


NWA_PLL_areas$ET_ID<-factor(NWA_PLL_areas$ET_ID,
                            levels = c("NED","NEC","MAB",
                                       "SAB","SAR","FEC",
                                       "GOM","CAR"))


#mangement areas shapefile
NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

#trying to clip the management areas to the coast
land<-here("data","shapefiles","Land","GSHHS_l_L1.shp") %>% st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NEP_TROLL_areas<-st_difference(NEP_TROLL_areas, st_union(st_combine(land)))

NEP_TROLL_areas$ET_ID<-factor(NEP_TROLL_areas$ET_ID,
                              levels = c("VN","CL","EK",
                                         "MT","CP"))

NEP_TROLL_areasOLD<- NEP_TROLL_areas %>% filter(ET_ID %in% c("VN","CL","EK",
                                                             "MT","CP"))

NEP_TROLL_areas<- NEP_TROLL_areas %>% filter(ET_ID %in% c("VN","CL","EK",
                                                            "MT"))


#################################
#load in old mgmt area shapefiles
#################################
NWA_PLL_areasOLD<- sf::st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/NWA_PLL_Mangementarea/areas_PLL.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_areasOLD$ET_ID<-c("CAR","FEC","GOM","MAB","NCA","NEC","NED","SAB","SAR","TUN","TUS")

NWA_PLL_areasOLD<-st_difference(NWA_PLL_areasOLD, st_union(st_combine(land)))


box <- c(xmin = -135, ymin = 47.30, xmax = -117, ymax = 50)
new<-st_bbox(box) %>% st_as_sfc()
st_crs(new)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
st_geometry(NEP_TROLL_areasOLD)[NEP_TROLL_areasOLD$ET_ID=="VN"]<-new
NEP_TROLL_areasOLD<-st_difference(NEP_TROLL_areasOLD, st_union(st_combine(land)))



#center coordinates for each mgmt_area
NWAmgmtarea_centroid<-NWA_PLL_areasOLD %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
NWAmgmtarea_centroid<-cbind(NWA_PLL_areasOLD$ET_ID,NWAmgmtarea_centroid)
colnames(NWAmgmtarea_centroid)<-c("mgmt_area","X","Y")

NEPmgmtarea_centroid<-NEP_TROLL_areasOLD %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
NEPmgmtarea_centroid<-cbind(NEP_TROLL_areasOLD$ET_ID,NEPmgmtarea_centroid)
colnames(NEPmgmtarea_centroid)<-c("mgmt_area","X","Y")

supp_modified_mgmtareas<-ggplot() +
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_sf(data = NWA_PLL_areasOLD, color = "blue", fill=NA, size = 1)+
  geom_sf(data = NWA_PLL_areas, color = "black", fill=NA, size = 1)+
  geom_sf(data = NEP_TROLL_areasOLD, color = "blue", fill=NA, size = 1)+
  geom_sf(data = NEP_TROLL_areas %>% filter(ET_ID %in% c("VN","CL","EK","MT")),
          color = "black", fill=NA, size = 1)+
  coord_sf(xlim = c(-132, -22), ylim = c(0, 53.5), expand = TRUE)+
  labs(y = "Latitude", x = "Longitude")+theme_bw()+
  geom_label(data = NWAmgmtarea_centroid, aes(x = X, y = Y,label = mgmt_area),
             fontface = "bold")+
  geom_label(data = NEPmgmtarea_centroid, aes(x = X, y = Y,label = mgmt_area),
             fontface = "bold")


ggsave(here("Plots","FS6_modified_mgmtareas.png"),
       width = 8, height = 5, units = "in", dpi = 300)
ggsave(here("Plots","FS6_modified_mgmtareas.svg"),
       width = 8, height = 5, units = "in", dpi = 300)

