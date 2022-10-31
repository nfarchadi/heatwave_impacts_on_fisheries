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
library(heatwaveR)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#########################################
#load in MHW property metrics for the NED
#########################################
NWAPLL_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_total.rds") %>% 
  readRDS()

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#trying to clip the management zones to the coast
land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_zones<-st_difference(NWA_PLL_zones, st_union(st_combine(land)))


NWA_PLL_zones$ET_ID<-factor(NWA_PLL_zones$ET_ID,
                            levels = c("NED","NEC","MAB",
                                       "SAB","SAR","FEC",
                                       "GOM","CAR"))


#########################
#load in NED MHW data set
#########################

NED_MHW <- here("data","water_temp","NWA","oisst",
                "MAB_NEC_NED_MHW.rds") %>% readRDS() %>% 
  filter(ET_ID == "NED")


###################
#grid cell analysis
###################

#SSTa raster map of a certain MHW (MHW Intensity)
NED_MHW_SSTa<-NED_MHW %>%
  filter(yearmon == "May 2012") %>% 
  ggplot() +
  geom_raster(aes(x=lon,y=lat, fill = temp_anomaly))+
  geom_sf(data = world, color= "black", fill = "grey") +
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  geom_label(aes(x = -56, y = 48.5,label = "May 2012")) +
  theme_bw()+
  labs(x = "",
       y = "")+
  labs(fill = "SSTa (°C)", title = "")+
  scale_fill_cmocean(name="balance",
                     limits = c(-4,4),oob = scales::squish)+
  ggnewscale::new_scale_fill()+
  geom_tile(NED_MHW %>% filter(lat==42.375 & lon==-52.375),
            mapping = aes(x=lon,y=lat, height = 0.25,width=0.25),
            color = "yellow", fill = NA, size = 1) +
  theme(legend.position="right")+
  theme(strip.background = element_blank())



#cell MHW timeseries
cell_timeseries<-NED_MHW %>%
  filter(yearmon >= "Jan 2012" & lat==42.375 & lon==-52.375) %>%
  ggplot() +
  geom_flame(mapping = aes(x = yearmon, y = detrend, y2 = seas)) +
  geom_line(mapping = aes(x = yearmon, y = detrend))+
  geom_line(mapping = aes(x = yearmon, y = seas),color = "forestgreen",linetype = "dashed", size = .5)+
  geom_rect(NED_MHW %>% filter(yearmon == "May 2012" & lat==42.375 & lon==-52.375),
            mapping=aes(xmin = yearmon, xmax=yearmon +0.1, ymin=-Inf,ymax=Inf),
            fill="gray", color = "yellow",alpha = 0.50)+
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
  scale_y_continuous(position = "left")

#inset the timeseries over the SST anomaly map
NED_MHW_inset<-cowplot::ggdraw() +
  cowplot::draw_plot(NED_MHW_SSTa) +
  cowplot::draw_plot(cell_timeseries, width = 0.4, height = 0.35, x = 0.3, y = 0.4)




##################################
#SSTa raster map of a certain MHW
##################################

NED_MHW_SSTa_contour<-NED_MHW %>%
  filter(yearmon == "May 2012") %>% 
  ggplot() +
  geom_raster(aes(x=lon,y=lat, fill = temp_anomaly))+
  geom_sf(data = world, color= "black", fill = "grey") +
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  geom_contour(aes(x=lon,y=lat,z=MHW),color = "yellow", size = 1)+
  geom_label(aes(x = -56, y = 48.5,label = "May 2012")) +
  theme_bw()+
  labs(x = "",
       y = "")+
  labs(fill = "SSTa (°C)", title = "")+
  scale_fill_cmocean(name="balance",
                     limits = c(-4,4),oob = scales::squish)+
  theme(legend.position="right")+
  theme(strip.background = element_blank())


################################################################
#SSTa raster map of a certain MHW (with MHW Intensity indicated)
################################################################

intensity<-NWAPLL_MHW_total %>% filter(mgmt_zone=="NED" & date=="May 2012") %>% 
  pull(mean_SSTa) %>% round(.,2)

NED_MHW_SSTa_intensity<-NED_MHW %>%
  filter(yearmon == "May 2012") %>% 
  ggplot() +
  geom_raster(aes(x=lon,y=lat, fill = temp_anomaly))+
  geom_sf(data = world, color= "black", fill = "grey") +
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  geom_contour(aes(x=lon,y=lat,z=MHW),color = "yellow", size = 1)+
  geom_label(aes(x = -56, y = 48.5,label = "May 2012")) +
  theme_bw()+
  labs(x = "",
       y = "")+
  labs(fill = "SSTa (°C)", title = "")+
  scale_fill_cmocean(name="balance",
                     limits = c(-4,4),oob = scales::squish)+
  theme(legend.position="right")+
  theme(strip.background = element_blank())+
  geom_text(aes(x =-52,y=45, label = paste0(intensity,"°C")), size = 12, color = "yellow")


################################################################
#SSTa raster map of a certain MHW (with MHW size indicated)
################################################################

size<-NWAPLL_MHW_total %>% filter(mgmt_zone=="NED" & date=="May 2012") %>% 
  pull(prop_MHW_cells) %>% round(.,2)*100

NED_MHW_SSTa_size<-NED_MHW %>%
  filter(yearmon == "May 2012") %>% 
  ggplot() +
  geom_raster(aes(x=lon,y=lat, fill = temp_anomaly))+
  geom_sf(data = world, color= "black", fill = "grey") +
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  geom_contour(aes(x=lon,y=lat,z=MHW),color = "yellow", size = 1)+
  geom_label(aes(x = -56, y = 48.5,label = "May 2012")) +
  theme_bw()+
  labs(x = "",
       y = "")+
  labs(fill = "SSTa (°C)", title = "")+
  scale_fill_cmocean(name="balance",
                     limits = c(-4,4),oob = scales::squish)+
  theme(legend.position="right")+
  theme(strip.background = element_blank())+
  geom_text(aes(x =-52,y=45, label = paste0(size,"%")), size = 12, color = "yellow")

######################
#Area-average analysis
######################


#map of the NED with a yellow contour
NED_MHW_region<-NED_MHW %>%
  filter(yearmon == "May 2012") %>% 
  ggplot() +
  geom_raster(aes(x=lon,y=lat, fill = temp_anomaly))+
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_sf(data = NWA_PLL_zones %>% filter(ET_ID == "NED") , color = "yellow", 
          fill = NA, size = 2)+
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  geom_label(aes(x = -56, y = 48.5,label = "May 2012")) +
  theme_bw()+
  labs(x = "",
       y = "")+
  labs(fill = "SSTa (°C)", title = "")+
  scale_fill_cmocean(name="balance",
                     limits = c(-4,4),oob = scales::squish)+
  theme(legend.position="right")+
  theme(strip.background = element_blank())

NED_MHW_region


cell_timeseries<-NED_MHW %>%
  group_by(yearmon) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  filter(yearmon >= "Jan 2012" & yearmon <= "Jan 2014") %>% 
  ggplot() +
  geom_flame(aes(x = yearmon, y = detrend, y2 = seas, fill = "MHW")) +
  geom_line(aes(x = yearmon, y = detrend, color = "black"),size = 1)+
  geom_line(aes(x = yearmon, y = seas, color = "forestgreen"),linetype = "dashed", size = 1)+
  geom_point(NED_MHW %>% filter(yearmon == "May 2012"),
             mapping=aes(x=yearmon, y=1.45),color = "yellow", size = 5,
             stroke=3, alpha = 0.009)+
  scale_color_manual(values = c("black","forestgreen"),
                     labels = c("SSTa", "Threshold"))+
  scale_linetype_manual(values = c("solid", "dashed"))+
  theme_bw()+ theme(legend.position = "bottom",
                    legend.text = element_text(size=20)) +
  labs(x = "", y=expression("SSTa "(degree~C)), fill = "", color = "") + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2012-01-01"),zoo::as.yearmon("2014-01-01")))

cell_timeseries

#same as above but without the yellow circle. Will be used to show duration
cell_timeseries<-NED_MHW %>%
  group_by(yearmon) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  filter(yearmon >= "Jan 2012" & yearmon <= "Jan 2014") %>% 
  ggplot() +
  geom_flame(aes(x = yearmon, y = detrend, y2 = seas, fill = "MHW")) +
  geom_line(aes(x = yearmon, y = detrend, color = "black"),size = 1)+
  geom_line(aes(x = yearmon, y = seas, color = "forestgreen"),linetype = "dashed", size = 1)+
  scale_color_manual(values = c("black","forestgreen"),
                     labels = c("SSTa", "Threshold"))+
  scale_linetype_manual(values = c("solid", "dashed"))+
  theme_bw()+ theme(legend.position = "bottom",
                    legend.text = element_text(size=20)) +
  labs(x = "", y=expression("SSTa "(degree~C)), fill = "", color = "") + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2012-01-01"),zoo::as.yearmon("2014-01-01")))

cell_timeseries
