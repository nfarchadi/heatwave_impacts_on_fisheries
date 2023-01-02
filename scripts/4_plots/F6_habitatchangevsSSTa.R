# Fig 6. Examples of the predicted change in the U.S. Atlantic longline core fishing grounds during two MHWs.

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

source(here("scripts","2_VDM","functions","threshold_finder.r"))

#############################################
#load in mgmt area shapefiles
#############################################

#mangement areas shapefile
NWA_PLL_areas<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


#trying to clip the management areas to the coast
land<-here("data","shapefiles","Land","GSHHS_l_L1.shp") %>% st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_areas<-st_difference(NWA_PLL_areas, st_union(st_combine(land)))


NWA_PLL_areas$ET_ID<-factor(NWA_PLL_areas$ET_ID,
                            levels = c("NED","NEC","MAB",
                                       "SAB","SAR","FEC",
                                       "GOM","CAR"))

rm(land)

#############################################
#load in VDM and VDM anomaly raster stacks
#############################################

#VDM stack
VDM_stack<-here("data","spatial_predictions","NWA_PLL",
                "NWA_PLL_monthly_predictions_2012to2020.nc") %>% stack()

tt<-as.yearmon(seq(as.Date("2012-01-01"),as.Date("2020-12-01"),by="month"))

VDM_stack<-setZ(VDM_stack,tt)

#####################################################################
#function to find the upper 25% threshold (aka the core fishing area)
#####################################################################

# Daily VDM prediction raster path
#VDMdaily_path<-"E:/VDM_results/NWA_PLL/spatial_predictions"
VDMdaily_path<-here("data","spatial_predictions","NWA_PLL")
VDMdaily_stack<-list.files(VDMdaily_path, full.names = TRUE) %>% stack()

dates<-c()
for(i in 1:length(list.files(VDMdaily_path))){
  day_date<-as.Date(substr(list.files(VDMdaily_path)[i], 1, 12),format = "%Y-%m-%d")
  dates<-append(day_date,dates)
}

VDMdaily_stack<-setZ(VDMdaily_stack,rev(dates))


#run the threshold finder function on NWA_PLL predictions
NWA_PLL_VDMthres<-threshold_finder(VDM_stack)

#mean 75% quantile raster
thresh<-NWA_PLL_VDMthres %>% summarise(threshold = mean(quantile75, na.rm=T))



#######################################
#Make binary maps of core fishing areas
#######################################

#reclassify into 1s and 0s based on the threshold
for(i in 1:nlayers(VDM_stack)){
  VDM_stack[[i]][values(VDM_stack[[i]])>=thresh$threshold]=1 
  VDM_stack[[i]][values(VDM_stack[[i]])<thresh$threshold]=0
}

tt<-as.yearmon(seq(as.Date("2012-01-01"),as.Date("2020-12-01"),by="month"))
names(VDM_stack)<-tt

#######################
#anomaly of binary maps
#######################
VDM_stack1<-rasterToPoints(VDM_stack)

VDM_stack1<-VDM_stack1 %>% as_tibble %>%
  gather("date","core_fishing_ground",-x,-y)

VDM_stack1$date<-VDM_stack1$date %>% gsub("\\."," ",.)


#monthly climatology for core fishing area
VDM_CFG_month_clim<-VDM_stack1 %>%
  mutate(date = zoo::as.yearmon(date),
         month = lubridate::month(date)) %>% 
  group_by(x,y,month) %>% 
  summarise(CFG_clim = mean(core_fishing_ground, na.rm=TRUE), .groups = "drop")


#monthly anomalies for core fishing area
CFG_anomalies<-VDM_stack1 %>% 
  mutate(date = zoo::as.yearmon(date),
         month = lubridate::month(date)) %>% 
  left_join(.,VDM_CFG_month_clim, by=c("x","y","month")) %>%
  mutate(CFG_anomaly = core_fishing_ground - CFG_clim)

#classiftying how the cells changed....FYI THIS TAKES A LONG TIME!!!
CFG_anomalies<-CFG_anomalies %>% 
  mutate(mask = core_fishing_ground + CFG_clim) %>%
  mutate(CFG_anomaly = ifelse(mask == 0, NA, CFG_anomaly)) %>% 
  mutate(Change = case_when(CFG_anomaly > 0 ~ "Gain",
                            CFG_anomaly < 0 ~ "Loss",
                            CFG_anomaly == 0 ~ "No Change"))


saveRDS(CFG_anomalies,here("data","spatial_predictions","NWA_PLL",
             "NWA_PLL_CFG_anomalies.rds"))

CFG_anomalies<-here("data","Monthly_Spatial_Predictions","NWA_PLL",
                                  "NWA_PLL_CFG_anomalies.rds") %>% readRDS()

NWAPLL_CFG_MHW_total<-here("data","mgmt_area_metrics","NWA_PLL",
                       "NWAPLL_CFG_MHW_total.rds") %>%
  readRDS()



NED_MHW<-NWAPLL_CFG_MHW_total %>% filter(mgmt_area == "NED")
NED_MHW %>% 
  ggplot()+
  geom_line(aes(x=date,y=mean_SSTa))

NED_MHW %>% 
  ggplot()+
  geom_line(aes(x=date,y=prop_MHW_cells))


CFG_anomalies<-CFG_anomalies %>%
  filter(x >= -60 & x <= -40 & y >=40) 

hab_change2012<-CFG_anomalies %>%
  filter(date == "May 2012" & Change != "NA") %>%
  dplyr::select(x,y,Change) %>% 
  ggplot() +
  geom_tile(aes(x=x,y=y, fill = Change), )+
  geom_sf(data = NWA_PLL_areas, color = "black", fill = NA, size = 1)+
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_label(aes(x = -56, y = 48.5,label = "May 2012")) +
  scale_fill_manual(values = c("yellow", "red", "black"))+
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  theme_bw() +
  labs(x = "", y = "", fill = "Core Fishing\nGrounds", title = "")

hab_change2014<-CFG_anomalies %>% 
  filter(date == "May 2014" & Change != "NA") %>%
  dplyr::select(x,y,Change) %>% 
  ggplot() +
  geom_tile(aes(x=x,y=y, fill = Change))+
  geom_sf(data = NWA_PLL_areas, color = "black", fill = NA, size = 1)+
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_label(aes(x = -56, y = 48.5,label = "May 2014")) +
  scale_fill_manual(values = c("yellow", "red", "black"))+
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  theme_bw() +
  labs(x = "", y = "", fill = "Core Fishing\nGrounds", title = "")



################################################
#SSTa raster map of a certain MHW (MHW Intensity)
################################################
NED_MHW <- here("data","oisst","NWa_PLL",
                "MAB_NEC_NED_MHW.rds") %>% readRDS() %>% 
  filter(ET_ID == "NED")

NED_MHW<-NED_MHW %>%
  filter(lon >= -60 & lon <= -40 & lat >=40)

NWA_mhw_2012<-NED_MHW %>%
  filter(yearmon == "May 2012") %>% 
  ggplot() +
  geom_tile(aes(x=lon,y=lat, fill = temp_anomaly))+
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_sf(data = NWA_PLL_areas %>% filter(ET_ID == "NED") , color = "black", fill = NA, size = 1)+
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  geom_contour(aes(x=lon,y=lat,z=MHW),color = "black") + 
  geom_label(aes(x = -56, y = 48.5,label = "May 2012")) +
  theme_bw()+
  labs(x = "",
       y = "")+
  labs(fill = "SSTa (°C)", title = "Size: 48%, Intensity: 2°C")+
  scale_fill_cmocean(name="balance",
                     limits = c(-5,5),oob = scales::squish,
                     labels=c("\u2264 -5","-2.5","0","2.5","\u2265 5"))+
  theme(legend.position="right")+
  theme(strip.background = element_blank())

NWA_mhw_2014<-NED_MHW %>%
  filter(yearmon == "May 2014") %>% 
  ggplot() +
  geom_tile(aes(x=lon,y=lat, fill = temp_anomaly))+
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_sf(data = NWA_PLL_areas %>% filter(ET_ID == "NED") , color = "black", fill = NA, size = 1)+
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  geom_contour(aes(x=lon,y=lat,z=MHW),color = "black") +
  geom_label(aes(x = -56, y = 48.5,label = "May 2014")) +
  theme_bw()+
  labs(x = "", y = "")+
  labs(fill = "SSTa (°C)", title = "Size: 4%, Intensity: 2.1°C")+
  scale_fill_cmocean(name="balance",
                     limits = c(-5,5),oob = scales::squish,
                     labels=c("\u2264 -5","-2.5","0","2.5","\u2265 5"))+
  theme(legend.position="right")+
  theme(strip.background = element_blank())

F6_habitatchangevsSSTa<-cowplot::plot_grid(NWA_mhw_2012, hab_change2012,
          NWA_mhw_2014, hab_change2014,
          labels = c('(A)','(B)','(C)','(D)'), label_size = 10,
          rel_widths = c(1,1.05,1,1.05))

ggsave(here("Plots","both_coasts","F6_habitatchangevsSSTa.png"),
       width = 10, height = 7, units = "in", dpi = 300)
ggsave(here("Plots","both_coasts","F6_habitatchangevsSSTa.svg"),
       width = 10, height = 7, units = "in", dpi = 300)

