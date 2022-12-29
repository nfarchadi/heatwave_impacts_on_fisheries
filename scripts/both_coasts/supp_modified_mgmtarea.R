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
#load in NEW mgmt zone shapefiles
#################################

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#trying to clip the management zones to the coast
land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_zones<-st_difference(NWA_PLL_zones, st_union(st_combine(land)))


NWA_PLL_zones$ET_ID<-factor(NWA_PLL_zones$ET_ID,
                            levels = c("NED","NEC","MAB",
                                       "SAB","SAR","FEC",
                                       "GOM","CAR"))


#mangement zones shapefile
NEP_TROLL_zones<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NEP_TROLL_zones<-st_difference(NEP_TROLL_zones, st_union(st_combine(land)))

NEP_TROLL_zones$ET_ID<-factor(NEP_TROLL_zones$ET_ID,
                              levels = c("VN","CL","EK",
                                         "MT","CP"))

NEP_TROLL_zonesOLD<- NEP_TROLL_zones %>% filter(ET_ID %in% c("VN","CL","EK",
                                                             "MT","CP"))

NEP_TROLL_zones<- NEP_TROLL_zones %>% filter(ET_ID %in% c("VN","CL","EK",
                                                            "MT"))


#################################
#load in old mgmt zone shapefiles
#################################
NWA_PLL_zonesOLD<- sf::st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/NWA_PLL_MangementZone/Zones_PLL.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_zonesOLD$ET_ID<-c("CAR","FEC","GOM","MAB","NCA","NEC","NED","SAB","SAR","TUN","TUS")

NWA_PLL_zonesOLD<-st_difference(NWA_PLL_zonesOLD, st_union(st_combine(land)))


box <- c(xmin = -135, ymin = 47.30, xmax = -117, ymax = 50)
new<-st_bbox(box) %>% st_as_sfc()
st_crs(new)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
st_geometry(NEP_TROLL_zonesOLD)[NEP_TROLL_zonesOLD$ET_ID=="VN"]<-new
NEP_TROLL_zonesOLD<-st_difference(NEP_TROLL_zonesOLD, st_union(st_combine(land)))



#center coordinates for each mgmt_zone
NWAmgmtzone_centroid<-NWA_PLL_zonesOLD %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
NWAmgmtzone_centroid<-cbind(NWA_PLL_zonesOLD$ET_ID,NWAmgmtzone_centroid)
colnames(NWAmgmtzone_centroid)<-c("mgmt_zone","X","Y")

NEPmgmtzone_centroid<-NEP_TROLL_zonesOLD %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
NEPmgmtzone_centroid<-cbind(NEP_TROLL_zonesOLD$ET_ID,NEPmgmtzone_centroid)
colnames(NEPmgmtzone_centroid)<-c("mgmt_zone","X","Y")

supp_modified_mgmtareas<-ggplot() +
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_sf(data = NWA_PLL_zonesOLD, color = "blue", fill=NA, size = 1)+
  geom_sf(data = NWA_PLL_zones, color = "black", fill=NA, size = 1)+
  geom_sf(data = NEP_TROLL_zonesOLD, color = "blue", fill=NA, size = 1)+
  geom_sf(data = NEP_TROLL_zones %>% filter(ET_ID %in% c("VN","CL","EK","MT")),
          color = "black", fill=NA, size = 1)+
  coord_sf(xlim = c(-132, -22), ylim = c(0, 53.5), expand = TRUE)+
  labs(y = "Latitude", x = "Longitude")+theme_bw()+
  geom_label(data = NWAmgmtzone_centroid, aes(x = X, y = Y,label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = NEPmgmtzone_centroid, aes(x = X, y = Y,label = mgmt_zone),
             fontface = "bold")


ggsave(here("Plots","both_coasts","supp & other", "supp_modified_mgmtareas.png"),
       width = 8, height = 5, units = "in", dpi = 300)
ggsave(here("Plots","both_coasts","supp & other", "supp_modified_mgmtareas.svg"),
       width = 8, height = 5, units = "in", dpi = 300)

