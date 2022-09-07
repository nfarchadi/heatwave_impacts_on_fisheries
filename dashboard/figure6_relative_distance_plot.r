library(here)
library(sf)
library(tidyverse)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(heatwaveR)
library(ggnewscale)
library(raster)
library(zoo)
library(cmocean)
library(ggridges)
library(viridis)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

#trying to clip the management zones to the coast
land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_zones<-st_difference(NWA_PLL_zones, st_union(st_combine(land)))

####
#PLL
####
NWAPLL_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_total.rds") %>% 
  readRDS()

month_avgCOG<-NWAPLL_MHW_total %>% filter(MHW == 0) %>% 
  group_by(mgmt_zone,month) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG_NWA<-NWAPLL_MHW_total %>% #filter(MHW == 1) %>% 
  dplyr::select(MHW,month,event_index,duration_months, 
                mgmt_zone,COGx,COGy,prop_MHW_area,
                mean_SSTa,date,prop_habitat_cells, 
                prop_MHW_cells) %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326")


MHWCOG_NWA$distance <- map_dbl(1:nrow(MHWCOG_NWA), function(x){
  x <- MHWCOG_NWA[x,]
  y <- month_avgCOG[month_avgCOG$mgmt_zone == x$mgmt_zone & month_avgCOG$month == x$month,]
  st_distance(x, y)
})


MHWCOG_NWA<-MHWCOG_NWA %>% mutate(distance = distance * 0.001,
                                  lon = sf::st_coordinates(.)[,1],
                                  lat = sf::st_coordinates(.)[,2])

mgmt_zone_area<-st_area(NWA_PLL_zones)
units(mgmt_zone_area)<-NULL #remove the units

area_df<-data.frame(mgmt_zone = NWA_PLL_zones$ET_ID %>% unique(),area = mgmt_zone_area*0.000001) #converting to km^2

MHWCOG_NWA<-left_join(MHWCOG_NWA, area_df, by=c("mgmt_zone")) %>% 
  mutate(dis_per_area = distance / area)


MHWCOG_NWA$mgmt_zone<-factor(MHWCOG_NWA$mgmt_zone,
                             levels = rev(c("CAR","GOM","FEC",
                                            "SAR","SAB",
                                            "MAB","NEC","NED")))

MHWCOG_NWA %>% 
  mutate(mgmt_zone = case_when(mgmt_zone == "MAB" ~ "Mid-Atlantic Bight",
                               mgmt_zone == "NEC" ~ "Northeast Coastal",
                               mgmt_zone == "NED" ~ "Northeast Distant",
                               mgmt_zone == "CAR" ~ "Caribbean",
                               mgmt_zone == "FEC" ~ "Florida East Coast",
                               mgmt_zone == "GOM" ~ "Gulf of Mexico",
                               mgmt_zone == "SAB" ~ "South Atlantic Bight",
                               mgmt_zone == "SAR" ~ "Sargasso"),
         mgmt_zone = factor(mgmt_zone, levels = c("Northeast Distant",
                                                  "Northeast Coastal",
                                                  "Mid-Atlantic Bight",
                                                  "South Atlantic Bight",
                                                  "Sargasso",
                                                  "Florida East Coast",
                                                  "Gulf of Mexico",
                                                  "Caribbean"))) %>%
  group_by(mgmt_zone,month) %>% 
  mutate(month_mean_reldis = mean(dis_per_area, na.rm=TRUE),
         percent_change_reldis=((dis_per_area - month_mean_reldis)/month_mean_reldis)*100) %>% ungroup %>%  
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
  ggplot() +
  geom_density(aes(log(dis_per_area*10e5), fill = MHW, color = MHW),
               alpha = 0.1)+
  theme_minimal()+
  facet_wrap(~mgmt_zone, nrow = 4)+
  labs(title = "", x = "Relative Distance", y = "")+
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 17),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 15))


