# Fig 2. Spatiotemporal SSTa and MHW trends in North America
# & Fig. S1. SSTa and MHW time series in the NWA.

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

####################################
#load in mgmt area shapefile for NWA
####################################

#mangement areas shapefile
NWA_PLL_areas<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

#trying to clip the management areas to the coast
land<-here("data","shapefiles","Land","GSHHS_l_L1.shp") %>% st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_areas<-st_difference(NWA_PLL_areas, st_union(st_combine(land)))


NWA_PLL_areas$ET_ID<-factor(NWA_PLL_areas$ET_ID,
                                   levels = c("NED","NEC","MAB",
                                              "SAB","SAR","FEC",
                                              "GOM","CAR"))

#subsetting shapefile
NWA_PLL_areas_subset<-NWA_PLL_areas %>% 
  filter(ET_ID %in% c("NED","MAB","FEC","CAR"))

###############################################
#Plotting MHW timeseries for each NWA mgmt area
###############################################

#FYI need to get MHW_mgmtarea_all from 10_CFG_change_analysis.R

MHW_mgmtarea_all_NWA<-MHW_mgmtarea_all
MHW_mgmtarea_NWA_subset<-MHW_mgmtarea_all_NWA %>% 
  filter(mgmt_area %in% c("NED","MAB","FEC","CAR")) %>% 
  mutate(mgmt_area = factor(mgmt_area, levels = c("NED","MAB","FEC","CAR")))

may_2012_mhw_NWA_circle<-MHW_mgmtarea_all_NWA %>% 
  filter(mgmt_area %in% c("NED","MAB","FEC","CAR")) %>% 
  mutate(mgmt_area = factor(mgmt_area, levels = c("NED","MAB","FEC","CAR"))) %>% 
  filter(date == as.yearmon("2012-05-01"))

ts_labels_NWA<-data.frame(mgmt_area = factor(c("NED","MAB","FEC","CAR")),
                      label = c("(G)","(H)","(I)","(J)"))

NWA_mhw_signature<-ggplot() +
  geom_flame(MHW_mgmtarea_NWA_subset, mapping = aes(x = date, y = detrend, y2 = seas)) +
  geom_line(MHW_mgmtarea_NWA_subset, mapping = aes(x = date, y = detrend))+
  geom_line(MHW_mgmtarea_NWA_subset, mapping = aes(x = date, y = seas),color = "forestgreen",linetype = "dashed", size = .5)+
  facet_wrap(~mgmt_area, nrow = 4)+
  geom_rect(may_2012_mhw_NWA_circle, mapping=aes(xmin = date, xmax=date + 0.1, ymin=-Inf,
                                 ymax=Inf),fill="gray", color = "black",alpha = 0.50)+
  scale_color_manual(name = "",
                     values = c("black","forestgreen"),
                     labels = c("SSTa", "Threshold"))+
  scale_linetype_manual(values = c("solid", "dashed"))+
  theme_bw()+ theme(legend.position="none",
                    panel.spacing = unit(.30, "lines"),
                    strip.text = element_text(size=7),
                    strip.background = element_blank(),
                    strip.text.x = element_text(margin = margin(0,0,.05,0, "cm"))) +
  labs(x = "", y=expression("SSTa "(degree~C))) + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2012-01-01"),zoo::as.yearmon("2020-12-31")))+
  scale_y_continuous(position = "right")+
  geom_text(data=ts_labels_NWA, aes(label = label, x = -Inf, y = Inf),
            hjust = 0, vjust = 1, fontface="bold")
  

#################################################
#SSTa raster map of a certain MHW (MHW Intensity)
#################################################

#stitched together the oisst data for each mgmt for each day into NetCDF files
#code do do that is not included

time_vec <- seq.POSIXt(as.POSIXct('2012-01-01', tz='UTC'), as.POSIXct('2020-12-01', tz='UTC'), by='month')

fList <- list.files("D:/OISST/SSTa", full.names = TRUE, pattern = ".nc")

fList_years <- c()
for (i in 1:length(fList)) fList_years[i] <- substr(fList[i], 26, 29)

ssta_stack <- raster::stack(fList[which(fList_years %in% lubridate::year(time_vec))])

names(ssta_stack) <- time_vec

may_2012_mhw_NWA<-rasterToPoints(ssta_stack[[5]]) %>% as.data.frame() %>%  
  rename("SSTa" = "X2012.05.01")


#center coordinates for each mgmt_area
mgmtarea_centroid_NWA<-NWA_PLL_areas_subset %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtarea_centroid_NWA<-cbind(NWA_PLL_areas_subset$ET_ID,mgmtarea_centroid_NWA)
colnames(mgmtarea_centroid_NWA)<-c("mgmt_area","X","Y")

NWA_mhw<-may_2012_mhw_NWA %>% 
  ggplot() +
  geom_raster(aes(x=x,y=y, fill = SSTa))+
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_sf(data = NWA_PLL_areas_subset, color = "black", fill = NA, size = 1)+
  coord_sf(xlim = c(-97.5, -40), ylim = c(11.5, 48.5), expand = TRUE) +
  theme_bw()+
  labs(x = "",
       y = "")+
  labs(fill = "SSTa (°C)")+
  scale_fill_cmocean(name="balance",
                     limits = c(-2,2),oob = scales::squish,
                     labels=c("\u2264 -2","-1","0","1","\u2265 2"))+
  #scale_color_manual(values = "black")+
  theme(legend.position="right")+
  theme(strip.background = element_blank())+
  geom_label(data = mgmtarea_centroid_NWA[1,], aes(x = X+5, y = Y+3.5, #CAR
                                               label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid_NWA[2,], aes(x = X-7, y = Y+2,#FEC
                                                 label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid_NWA[3,], aes(x = X-6, y = Y,#MAB
                                               label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid_NWA[4,], aes(x = X-4, y = Y+4,#NED
                                               label = mgmt_area),
             fontface = "bold")+
  geom_text(data=data.frame(), aes(label = '(F)', x = -Inf, y = Inf),
            hjust = 0, vjust = 1, fontface="bold")

NWA_mhw / NWA_mhw_signature  


###################
#all the NWA areas
###################

#FYI need to get MHW_mgmtarea_all from 10_CFG_change_analysis.R

MHW_mgmtarea_all_NWA<-MHW_mgmtarea_all %>% 
  mutate(mgmt_area = factor(mgmt_area, level= c("NED","NEC","MAB",
                                                "SAB","SAR", "FEC",
                                                "GOM", "CAR")))


ts_labels_NWA<-data.frame(mgmt_area = factor(c("NED","NEC","MAB",
                                               "SAB","SAR", "FEC",
                                               "GOM", "CAR")),
                          label = c("(A)","(B)","(C)","(D)",
                                    "(E)","(F)","(G)","(H)"))

ggplot() +
  geom_flame(MHW_mgmtarea_all_NWA, mapping = aes(x = date, y = detrend, y2 = seas)) +
  geom_line(MHW_mgmtarea_all_NWA, mapping = aes(x = date, y = detrend))+
  geom_line(MHW_mgmtarea_all_NWA, mapping = aes(x = date, y = seas),color = "forestgreen",linetype = "dashed", size = .5)+
  facet_wrap(~mgmt_area, nrow = 4)+
  scale_color_manual(name = "",
                     values = c("black","forestgreen"),
                     labels = c("SSTa", "Threshold"))+
  scale_linetype_manual(values = c("solid", "dashed"))+
  theme_bw()+ theme(legend.position="none",
                    panel.spacing = unit(.30, "lines"),
                    strip.text = element_text(size=7),
                    strip.background = element_blank(),
                    strip.text.x = element_text(margin = margin(0,0,.05,0, "cm"))) +
  labs(x = "", y=expression("SSTa "(degree~C))) + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2012-01-01"),zoo::as.yearmon("2020-12-31")))+
  scale_y_continuous(position = "right")

ggsave(here("Plots", "FS1_MHWsignatures_NWA.png"),
       width = 9, height = 7, units = "in", dpi = 300)
ggsave(here("Plots", "FS1_MHWsignatures_NWA.svg"),
       width = 9, height = 7, units = "in", dpi = 300)







####
#NEP
####



#####################################
#load in mgmt area shapefiles for NEP
#####################################

#mangement areas shapefile
NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NEP_TROLL_areas<-st_difference(NEP_TROLL_areas, st_union(st_combine(land)))

NEP_TROLL_areas$ET_ID<-factor(NEP_TROLL_areas$ET_ID,
                            levels = c("VN","CL","EK",
                                       "MT","CP"))

#subsetting shapefile
NEP_TROLL_areas_subset<-NEP_TROLL_areas %>% 
  filter(ET_ID %in% c("VN","CL","EK","MT"))

################################################
#Plotting MHW timeseries for each NEP mgmt area
################################################

#FYI need to get MHW_mgmtarea_all from 10_CFG_change_analysis.R

MHW_mgmtarea_all_NEP<-MHW_mgmtarea_all
MHW_mgmtarea_NEP_subset<-MHW_mgmtarea_all_NEP %>% 
  filter(mgmt_area %in% c("VN","CL","EK","MT")) %>% 
  mutate(mgmt_area = factor(mgmt_area, levels = c("VN","CL","EK","MT")))


may_2015_mhw_NEP_circle<-MHW_mgmtarea_NEP_subset %>%
  filter(date == as.yearmon("2015-05-01"))

ts_labels_NEP<-data.frame(mgmt_area = factor(c("VN","CL","EK","MT")),
                      label = c("(B)","(C)","(D)","(E)"))

NEP_mhw_signature<-ggplot() +
  geom_flame(MHW_mgmtarea_NEP_subset, mapping = aes(x = date, y = detrend, y2 = seas)) +
  geom_line(MHW_mgmtarea_NEP_subset, mapping = aes(x = date, y = detrend))+
  geom_line(MHW_mgmtarea_NEP_subset, mapping = aes(x = date, y = seas),color = "forestgreen",linetype = "dashed", size = .5)+
  facet_wrap(~mgmt_area, nrow = 4)+
  geom_rect(may_2015_mhw_NEP_circle, mapping=aes(xmin = date, xmax=date + 0.1, ymin=-Inf,
                                                 ymax=Inf),fill="gray", color = "black",alpha = 0.50)+
  scale_color_manual(name = "",
                     values = c("black","forestgreen"),
                     labels = c("SSTa", "Threshold"))+
  scale_linetype_manual(values = c("solid", "dashed"))+
  theme_bw()+ theme(legend.position="none",
                    panel.spacing = unit(.30, "lines"),
                    strip.text = element_text(size=7),
                    strip.background = element_blank(),
                    strip.text.x = element_text(margin = margin(0,0,.05,0, "cm"))) +
  labs(x = "", y=expression("SSTa "(degree~C))) + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2012-01-01"),zoo::as.yearmon("2020-12-31")))+
  scale_y_continuous(position = "left")+
  geom_text(data=ts_labels_NEP, aes(label = label, x = -Inf, y = Inf),
            hjust = 0, vjust = 1, fontface="bold")

################################################
#SSTa raster map of a certain MHW (MHW Intensity)
################################################

may_2015_mhw_NEP<-here("data","water_temp","NEP","oisst","Conception_Monterey_Eureka_Columbia_Vancouver_MHW.rds") %>% readRDS() %>% 
  filter(yearmon == as.yearmon("2015-05-01"))


#center coordinates for each mgmt_area
mgmtarea_centroid_NEP<-NEP_TROLL_areas_subset %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtarea_centroid_NEP<-cbind(NEP_TROLL_areas_subset$ET_ID,mgmtarea_centroid_NEP)
colnames(mgmtarea_centroid_NEP)<-c("mgmt_area","X","Y")

NEP_mhw<-may_2015_mhw_NEP %>% 
  ggplot() +
  geom_raster(aes(x=lon,y=lat, fill = detrend))+
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_sf(data = NEP_TROLL_areas_subset, color = "black", fill = NA, size = 1)+
  coord_sf(xlim = c(-134.5, -117), ylim = c(36.75, 53.75), expand = TRUE) +
  theme_bw()+
  labs(x = "", y = "")+
  labs(fill = "SSTa (°C)")+
  scale_fill_cmocean(name="balance",
                     limits = c(-2,2),oob = scales::squish,
                     labels=c("\u2264 -2","-1","0","1","\u2265 2"))+
  #scale_color_manual(values = "black")+
  theme(legend.position="right")+
  theme(strip.background = element_blank(), legend.position = "none")+
  geom_label(data = mgmtarea_centroid_NEP[4,], aes(x = X+7, y = Y,#VN
                                                   label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid_NEP[3,], aes(x = X+7, y = Y,#CL
                                                label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid_NEP[2,], aes(x = X+7.5, y = Y,#EK
                                                label = mgmt_area),
             fontface = "bold")+
  geom_label(data = mgmtarea_centroid_NEP[1,], aes(x = X+8, y = Y+1,#MT
                                               label = mgmt_area),
             fontface = "bold")+
  geom_text(data=data.frame(), aes(label = '(A)', x = -119, y = Inf),
            hjust = 0, vjust = 1, fontface="bold")

(NEP_mhw | NWA_mhw)/ (NEP_mhw_signature | NWA_mhw_signature)  


ggsave(here("Plots","F2_MHW_signatures&maps.png"),
       width = 10, height = 7, units = "in", dpi = 300)
ggsave(here("Plots","F2_MHW_signatures&maps.svg"),
       width = 10, height = 7, units = "in", dpi = 300)


