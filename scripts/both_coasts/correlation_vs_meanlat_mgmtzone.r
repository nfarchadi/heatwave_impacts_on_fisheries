library(sf)
library(tidyverse)
library(patchwork)
library(here)
sf_use_s2(FALSE)# need to do this to remove spherical geometry

###################################################
#correlation (r) vs mean latitude of each mgmt_zone
###################################################


#management zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_zones<-st_transform(NWA_PLL_zones)


NEP_TROLL_zones<-here("data","shapefiles","NEP_TROLL","Zones_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NEP_TROLL_zones<-st_transform(NEP_TROLL_zones)

all_zones<-rbind(NWA_PLL_zones,NEP_TROLL_zones)

#center coordinates for each mgmt_zone
mgmtzone_centroid<-all_zones %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtzone_centroid<-cbind(all_zones$ET_ID,mgmtzone_centroid)
colnames(mgmtzone_centroid)<-c("mgmt_zone","X","Y")


#####################
#NWA PLL
#####################
NWAPLLa_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLLa_MHW_total.rds") %>%
  readRDS()

intensity_cor<-NWAPLLa_MHW_total %>%
  group_by(mgmt_zone) %>% 
  summarise(Intensity = cor(NWA_PLL.area.anomaly,detrend.x)) %>% 
  ungroup()%>% 
  mutate(Fishery = "Longline") %>% 
  left_join(., mgmtzone_centroid, by = c("mgmt_zone"))

size_cor<-NWAPLLa_MHW_total %>%
  na.omit() %>% 
  group_by(mgmt_zone) %>% 
  summarise(Size = cor(NWA_PLL.area.anomaly, prop_MHW_cells)) %>% 
  ungroup() %>% 
  mutate(Fishery = "Longline")%>% 
  left_join(., mgmtzone_centroid, by = c("mgmt_zone"))

NWA_PLL_cor<-left_join(intensity_cor, size_cor, by=c("mgmt_zone","Fishery", 
                                                     "X", "Y"))%>% 
  arrange(desc(Y))


###################
#NEP TROLL
###################
NEPTROLLa_MHW_total<-here("data","Mgmt_zone","NEP_TROLL","NEPTROLLa_MHW_total.rds") %>% readRDS() %>% filter(mgmt_zone != "CP")

intensity_cor<-NEPTROLLa_MHW_total %>%
  group_by(mgmt_zone) %>% 
  summarise(Intensity = cor(NEP_TROLL.area.anomaly,detrend)) %>% 
  ungroup()%>% 
  mutate(Fishery = "Troll")%>% 
  left_join(., mgmtzone_centroid, by = c("mgmt_zone"))

size_cor<-NEPTROLLa_MHW_total %>% 
  na.omit() %>% 
  group_by(mgmt_zone) %>% 
  summarise(Size = cor(NEP_TROLL.area.anomaly, prop_MHW_cells)) %>% 
  ungroup() %>% 
  mutate(Fishery = "Troll")%>% 
  left_join(., mgmtzone_centroid, by = c("mgmt_zone"))

NEP_TROLL_cor<-left_join(intensity_cor, size_cor, by=c("mgmt_zone","Fishery", 
                                                       "X", "Y")) %>% 
  arrange(desc(Y))


#######################
#combine both coasts df
#######################

both_coasts_corr<-rbind(NWA_PLL_cor,NEP_TROLL_cor) %>% 
  gather(variable, corr, -mgmt_zone,-Fishery, -X, -Y)

both_coasts_corr %>% 
  ggplot() +
  geom_path(aes(x = corr, y = Y, color = Fishery),
            size = 1) +
  geom_label(aes(x = corr, y = Y,label = mgmt_zone))+
  facet_wrap(~variable) + theme_bw() + 
  labs(x = "Pearson Correlation", y = "Latitude", color = "Fishery")+
  scale_color_manual(values=c("orange","lightseagreen"))

