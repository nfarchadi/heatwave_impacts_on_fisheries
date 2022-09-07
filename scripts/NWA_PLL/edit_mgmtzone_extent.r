library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#mangement zones shapefile
NWA_PLL_zones<-sf::st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/NWA_PLL_MangementZone/Zones_PLL.shp", crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_zones_old<-NWA_PLL_zones

NWA_PLL_zones<-st_transform(NWA_PLL_zones)

NWA_PLL_zones<-NWA_PLL_zones[c(1:4,6:9),]

NWA_PLL_zones$ET_ID<-c("CAR","FEC","GOM","MAB",
                       "NEC","NED","SAB","SAR")




####----changing CAR extent----####

#new (smaller) extent
box <- c(xmin = -71, ymin = 5, xmax = -60, ymax = 22)
new<-st_bbox(box) %>% st_as_sfc()
st_crs(new)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


st_geometry(NWA_PLL_zones)[NWA_PLL_zones$ET_ID=="CAR"]<-new

####----changing NED extent----####

box <- c(xmin = -60, ymin = 40, xmax = -40, ymax = 50)
new<-st_bbox(box) %>% st_as_sfc()
st_crs(new)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


st_geometry(NWA_PLL_zones)[NWA_PLL_zones$ET_ID=="NED"]<-new

#save the new shapefile
#st_write(NWA_PLL_zones,here("data","shapefiles","NWA_PLL","Zones_PLL.shp"))

####----plotting to see how the new extent looks like---####

NWA_PLL<-here("data","AIS_processed","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull_v2_enhanced.rds") %>% readRDS() %>% mutate(fleet = "PLL")%>% 
  dplyr::select(X,Y,Pres_abs,fleet)

#center coordinates for each mgmt_zone
mgmtzone_centroid<-NWA_PLL_zones %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtzone_centroid<-cbind(NWA_PLL_zones$ET_ID,mgmtzone_centroid)
colnames(mgmtzone_centroid)<-c("mgmt_zone","X","Y")


PLL_plot<-ggplot() +
  geom_bin2d(NWA_PLL %>% filter(Pres_abs ==1), 
             mapping = aes(x=X, y=Y),binwidth=1) +
  scale_fill_viridis(option = "turbo", trans = "log",
                     name = "Frequency of\n Occurence",
                     breaks = c(1,10,100,500),
                     guide = guide_colorbar(barwidth = 2, 
                                            barheight = 20)) +
  geom_sf(data = NWA_PLL_zones, color = "goldenrod3", fill=NA)+
  #geom_sf(data = NWA_PLL_zones_old, color = "red", fill=NA)+
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_label_repel(data = mgmtzone_centroid, aes(x = X, y = Y, 
                                                 label = mgmt_zone), 
                   fontface = "bold",
                   box.padding = 3,
                   segment.size = 1,
                   max.overlaps = Inf,
                   )+
  coord_sf(xlim = c(-98, -40), ylim = c(11, 50), expand = TRUE)+
  labs(y = "Latitude", x = "Longitude")+theme_bw()

PLL_plot
