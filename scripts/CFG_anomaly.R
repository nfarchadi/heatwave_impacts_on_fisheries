#Identifying MHWs in a mangement zone
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

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#trying to clip the management zones to the coast
land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_zones<-st_difference(NWA_PLL_zones, st_union(st_combine(land)))


NWA_PLL_zones$ET_ID<-factor(NWA_PLL_zones$ET_ID,
                            levels = c("NED","NEC","MAB",
                                       "SAB","SAR","FEC",
                                       "GOM","CAR"))

rm(land)
#############################################
#load in VDM and VDM anomaly raster stacks
#############################################

#VDM stack
VDM_stack<-here("data","Monthly_Spatial_Predictions","NWA_PLL",
                "NWA_PLL_monthly_predictions_2012to2020.nc") %>% stack()

tt<-as.yearmon(seq(as.Date("2012-01-01"),as.Date("2020-12-01"),by="month"))

VDM_stack<-setZ(VDM_stack,tt)

#####################################################################
#function to find the upper 25% threshold (aka the core fishing area)
#####################################################################
# Daily VDM prediction raster path
VDMdaily_path<-"E:/VDM_results/NWA_PLL/spatial_predictions"
VDMdaily_stack<-list.files(VDMdaily_path, full.names = TRUE) %>% stack()

dates<-c()
for(i in 1:length(list.files(VDMdaily_path))){
  day_date<-as.Date(substr(list.files(VDMdaily_path)[i], 1, 12),format = "%Y-%m-%d")
  dates<-append(day_date,dates)
}

VDMdaily_stack<-setZ(VDMdaily_stack,rev(dates))

threshold_finder<-function(stack_VDM){
  
  thresh_df<-list()
  
  for(i in 1:nlayers(stack_VDM)){
    print(getZ(stack_VDM[[i]]))
    
    ras<-stack_VDM[[i]]
    quant75<-quantile(ras)[4]
    
    daily_thresh<-data.frame(quantile75=quant75,
                             date=getZ(stack_VDM[[i]]) %>% zoo::as.Date())
    
    thresh_df[[length(thresh_df)+1]]<-daily_thresh
  }
  thresh_df<-bind_rows(thresh_df)
  return(thresh_df)
  
}


#run the threshold finder function on NWA_PLL predictions
NWA_PLL_VDMthres<-threshold_finder(VDM_stack) #VDMdaily_stack

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

CFG_anomalies<-CFG_anomalies %>% 
  mutate(mask = core_fishing_ground + CFG_clim) %>%
  mutate(CFG_anomaly = ifelse(mask == 0, NA, CFG_anomaly)) %>% 
  mutate(Change = case_when(CFG_anomaly > 0 ~ "Gain",
                            CFG_anomaly < 0 ~ "Loss",
                            CFG_anomaly == 0 ~ "No Change"))


# saveRDS(CFG_anomalies,here("data","Monthly_Spatial_Predictions","NWA_PLL",
#              "NWA_PLL_CFG_anomalies.rds"))

CFG_anomalies<-here("data","Monthly_Spatial_Predictions","NWA_PLL",
                                  "NWA_PLL_CFG_anomalies.rds") %>% readRDS()

NWAPLL_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_total.rds") %>%
  readRDS()



NED_MHW<-NWAPLL_MHW_total %>% filter(mgmt_zone == "NED")
NED_MHW %>% 
  ggplot()+
  geom_line(aes(x=date,y=mean_SSTa))

NED_MHW %>% 
  ggplot()+
  geom_line(aes(x=date,y=prop_MHW_cells))
# CFG_anomalies<-CFG_anomalies %>% sf::st_as_sf(coords = c("x","y"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
#   st_join(NWA_PLL_zones)
#                     
# 
# CFG_anomalies<-CFG_anomalies %>% 
#   mutate(month=lubridate::month(date),
#          season = case_when(month %in% c(12,1,2)~ "winter",
#                             month %in% c(3,4,5)~ "spring",
#                             month %in% c(6,7,8)~ "summer",
#                             month %in% c(9,10,11)~ "fall")) %>% 
#   group_by(x,y,season) %>% 
#   summarise(CFG_anomaly = mean(CFG_anomaly, na.rm=TRUE),.groups = "drop") %>% 
#   mutate(Change = case_when(CFG_anomaly > 0 ~ "Gain",
#                             CFG_anomaly < 0 ~ "Loss",
#                             CFG_anomaly == 0 ~ "No Change"))





# CFG_anomalies %>%
#   filter(date == "Jun 2012") %>% 
#   dplyr::select(x,y,core_fishing_ground) %>% 
#   ggplot() +
#   geom_tile(aes(x=x,y=y, fill = core_fishing_ground))+
#   geom_sf(data = world, color= "black", fill = "grey")+
#   scale_fill_cmocean(name="amp")+
#   coord_sf(xlim = c(-100, -40), ylim = c(35, 50), expand = TRUE) +
#   theme_bw()
# 
# CFG_anomalies %>%
#   filter(date == "May 2012") %>% 
#   dplyr::select(x,y,CFG_clim) %>% 
#   ggplot() +
#   geom_tile(aes(x=x,y=y, fill = CFG_clim))+
#   geom_sf(data = world, color= "black", fill = "grey")+
#   scale_fill_cmocean(name="amp")+
#   coord_sf(xlim = c(-100, -40), ylim = c(35, 50), expand = TRUE) +
#   theme_bw()
#   
# CFG_anomalies %>%
#   filter(date == "May 2012") %>% 
#   dplyr::select(x,y,Change) %>% 
#   ggplot() +
#   geom_tile(aes(x=x,y=y, fill = Change))+
#   geom_sf(data = world, color= "black", fill = "grey")+
#   scale_fill_manual(values = c("yellow", "red", "black"), na.value = NA)+
#   #scale_fill_cmocean(name="dense", na.value = NA)+
#   coord_sf(xlim = c(-80, -40), ylim = c(35, 50), expand = TRUE) +
#   theme_bw()


CFG_anomalies<-CFG_anomalies %>%
  filter(x >= -60 & x <= -40 & y >=40) 

hab_change2012<-CFG_anomalies %>%
  #filter(x >= -60 & x <= -40 & y >=40) %>% 
  filter(date == "May 2012" & Change != "NA") %>%
  dplyr::select(x,y,Change) %>% 
  ggplot() +
  geom_tile(aes(x=x,y=y, fill = Change), )+
  geom_sf(data = NWA_PLL_zones, color = "black", fill = NA, size = 1)+
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
  geom_sf(data = NWA_PLL_zones, color = "black", fill = NA, size = 1)+
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_label(aes(x = -56, y = 48.5,label = "May 2014")) +
  scale_fill_manual(values = c("yellow", "red", "black"))+
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  theme_bw() +
  labs(x = "", y = "", fill = "Core Fishing\nGrounds", title = "")



################################################
#SSTa raster map of a certain MHW (MHW Intensity)
################################################
NED_MHW <- here("data","water_temp","NWA","oisst",
                "MAB_NEC_NED_MHW.rds") %>% readRDS() %>% 
  filter(ET_ID == "NED")

NED_MHW<-NED_MHW %>%
  filter(lon >= -60 & lon <= -40 & lat >=40)

NWA_mhw_2012<-NED_MHW %>%
  filter(yearmon == "May 2012") %>% 
  ggplot() +
  geom_tile(aes(x=lon,y=lat, fill = temp_anomaly))+
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_sf(data = NWA_PLL_zones %>% filter(ET_ID == "NED") , color = "black", fill = NA, size = 1)+
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
  #scale_color_manual(values = "black")+
  theme(legend.position="right")+
  theme(strip.background = element_blank())

NWA_mhw_2014<-NED_MHW %>%
  filter(yearmon == "May 2014") %>% 
  ggplot() +
  geom_tile(aes(x=lon,y=lat, fill = temp_anomaly))+
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_sf(data = NWA_PLL_zones %>% filter(ET_ID == "NED") , color = "black", fill = NA, size = 1)+
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  geom_contour(aes(x=lon,y=lat,z=MHW),color = "black") +
  geom_label(aes(x = -56, y = 48.5,label = "May 2014")) +
  theme_bw()+
  labs(x = "", y = "")+
  labs(fill = "SSTa (°C)", title = "Size: 4%, Intensity: 2.1°C")+
  scale_fill_cmocean(name="balance",
                     limits = c(-5,5),oob = scales::squish,
                     labels=c("\u2264 -5","-2.5","0","2.5","\u2265 5"))+
  #scale_color_manual(values = "black")+
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

# a<-NWAPLL_MHW_total %>% 
#   filter(mgmt_zone == "NED") %>% 
#   dplyr::select(date, prop_MHW_cells) %>% view()
#   ggplot()+
#   geom_line(aes(x = date, y = prop_MHW_cells))
# 
# b<-NWAPLL_MHW_total %>% 
#   filter(mgmt_zone == "NED") %>% 
#   dplyr::select(date, mean_SSTa) %>% view()
#   ggplot()+
#   geom_line(aes(x = date, y = mean_SSTa))
# a/b
