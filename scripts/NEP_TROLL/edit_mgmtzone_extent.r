library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#mangement zones shapefile
NWA_PLL_zones<-sf::st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/NWA_PLL_MangementZone/Zones_PLL.shp", crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NEP_TROLL_zones<-st_transform(NWA_PLL_zones)

NEP_TROLL_zones<-NEP_TROLL_zones[c(1:5),]

NEP_TROLL_zones$ET_ID<-c("CP","MT","EK",
                       "CL","VN")




####----changing 1st extent (Conception)----####

box <- c(xmin = -135, ymin = 32, xmax = -117, ymax = 36)
new<-st_bbox(box) %>% st_as_sfc()
st_crs(new)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


st_geometry(NEP_TROLL_zones)[NEP_TROLL_zones$ET_ID=="CP"]<-new

####----changing  2nd extent (Monterey)----####

box <- c(xmin = -135, ymin = 36, xmax = -117, ymax = 40.10)
new<-st_bbox(box) %>% st_as_sfc()
st_crs(new)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


st_geometry(NEP_TROLL_zones)[NEP_TROLL_zones$ET_ID=="MT"]<-new

####----changing 3rd extent (Eureka)----####

box <- c(xmin = -135, ymin = 40.10, xmax = -117, ymax = 43)
new<-st_bbox(box) %>% st_as_sfc()
st_crs(new)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


st_geometry(NEP_TROLL_zones)[NEP_TROLL_zones$ET_ID=="EK"]<-new

####----changing 4th extent (Columbia)----####

box <- c(xmin = -135, ymin = 43, xmax = -117, ymax = 47.30)
new<-st_bbox(box) %>% st_as_sfc()
st_crs(new)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


st_geometry(NEP_TROLL_zones)[NEP_TROLL_zones$ET_ID=="CL"]<-new

####----changing 5th extent (Vancouver)----####

box <- c(xmin = -135, ymin = 47.30, xmax = -117, ymax = 54.5)
new<-st_bbox(box) %>% st_as_sfc()
st_crs(new)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


st_geometry(NEP_TROLL_zones)[NEP_TROLL_zones$ET_ID=="VN"]<-new

#save the new shapefile

#st_write(NEP_TROLL_zones,here("data","shapefiles","NEP_TROLL","Zones_TROLL.shp"))

####----plotting to see how the new extent looks like---####

NEP_TROLL<-here("data","AIS_processed","NEP_TROLL","Pres_Abs_2013to2020_NWA_USA_TROLL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull_v2_enhanced.rds") %>% readRDS() %>% mutate(fleet = "TROLL") %>% 
  dplyr::select(X,Y,Pres_abs,fleet)


## general plot setup
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF",
                                 "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


bzz <- c(1,10,100,1000,10000)

#center coordinates for each mgmt_zone
mgmtzone_centroid<-NEP_TROLL_zones %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtzone_centroid<-cbind(NEP_TROLL_zones$ET_ID,mgmtzone_centroid)
colnames(mgmtzone_centroid)<-c("mgmt_zone","X","Y")


troll_plot<-ggplot() +
  geom_bin2d(NEP_TROLL %>% filter(Pres_abs ==1),
             mapping = aes(x=X, y=Y), binwidth=1) +
  scale_fill_viridis(option = "turbo", trans = "log",
                     name = "Frequency of\n Occurence",
                     breaks = c(1,10,100,500),
                     guide = guide_colorbar(barwidth = 2, 
                                            barheight = 20)) +
  geom_sf(data = NEP_TROLL_zones, color = "goldenrod3", fill=NA)+
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_label_repel(data = mgmtzone_centroid, aes(x = X, y = Y, 
                                                 label = mgmt_zone), 
                   fontface = "bold",
                   box.padding = 2,
                   segment.size = 1,
                   max.overlaps = Inf,
                   )+
  coord_sf(xlim = c(-139, -115), ylim = c(32, 54), expand = TRUE)+
  labs(y = "Latitude", x = "Longitude")+theme_bw()

troll_plot
