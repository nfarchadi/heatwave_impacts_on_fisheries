#Fleet Displacement

library(here)
library(sf)
library(tidyverse)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(heatwaveR)
library(cowplot)
library(introdataviz)
library(raster)
library(zoo)
library(viridis)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#############################################
#load in mgmt area shapefiles
#############################################

# NWA mangement areas shapefile
NWA_PLL_areas<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

#trying to clip the management areas to the coast
land<-here("data","shapefiles","Land","GSHHS_l_L1.shp") %>% st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_areas<-st_difference(NWA_PLL_areas, st_union(st_combine(land)))


#NEP mangement areas shapefile
NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


#trying to clip the management areas to the coast
land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NEP_TROLL_areas<-st_difference(NEP_TROLL_areas, st_union(st_combine(land)))

######################################
#shift / total area in mgmt area
######################################

####
#PLL
####
NWAPLL_CFG_MHW_total<-here("data","mgmt_area_metrics","NWA_PLL",
                       "NWAPLL_CFG_MHW_total.rds") %>%
  readRDS()

month_avgCOG<-NWAPLL_CFG_MHW_total %>% filter(MHW == 0) %>% 
  group_by(mgmt_area,month) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG_NWA<-NWAPLL_CFG_MHW_total %>%  
  dplyr::select(MHW,month,event_index,duration_months, 
                mgmt_area,COGx,COGy,prop_MHW_area,
                mean_SSTa,date,prop_habitat_cells, 
                prop_MHW_cells) %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326")


MHWCOG_NWA$distance <- map_dbl(1:nrow(MHWCOG_NWA), function(x){
  x <- MHWCOG_NWA[x,]
  y <- month_avgCOG[month_avgCOG$mgmt_area == x$mgmt_area & month_avgCOG$month == x$month,]
  st_distance(x, y)
})


MHWCOG_NWA<-MHWCOG_NWA %>% mutate(distance = distance * 0.001,
                          lon = sf::st_coordinates(.)[,1],
                          lat = sf::st_coordinates(.)[,2])

mgmt_area_area<-st_area(NWA_PLL_areas)
units(mgmt_area_area)<-NULL #remove the units

area_df<-data.frame(mgmt_area = NWA_PLL_areas$ET_ID %>% unique(),area = mgmt_area_area*0.000001) #converting to km^2

MHWCOG_NWA<-left_join(MHWCOG_NWA, area_df, by=c("mgmt_area")) %>% 
  mutate(dis_per_area = distance / area)


MHWCOG_NWA$mgmt_area<-factor(MHWCOG_NWA$mgmt_area,
                         levels = c("CAR","GOM","FEC",
                                        "SAR","SAB",
                                        "MAB","NEC","NED"))

#ks test
MHWCOG_NWA<-MHWCOG_NWA %>%
  group_by(mgmt_area,month) %>% 
  mutate(month_mean_reldis = mean(dis_per_area, na.rm=TRUE),
         percent_change_reldis=((dis_per_area - month_mean_reldis)/month_mean_reldis)*100) %>% ungroup()

mgmtareas<-unique(as.character(MHWCOG_NWA$mgmt_area))
ks<-data.frame(mgmt_area=NA, pvalue=NA, D=NA)
counter = 1
for(i in 1:length(mgmtareas)){
  a<-MHWCOG_NWA %>% filter(mgmt_area == mgmtareas[i])
  b<-a %>% filter(MHW == 1)
  c<-a %>% filter(MHW == 0)
  print(mgmtareas[i])
  ks_test<-ks.test(log(c$dis_per_area),log(b$dis_per_area),
                   alternative = 'greater')
  pvalue<-round(ks_test$p.value,4)
  d<-round(ks_test$statistic,4)
  ks[counter,1]<-mgmtareas[i]
  ks[counter,2]<-pvalue
  ks[counter,3]<-d

  counter = counter + 1
}

ks_pvD_NWA <- ks


NWA_plot<-MHWCOG_NWA %>%
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
  ggplot()+
  geom_split_violin(aes(mgmt_area, y=dis_per_area*1000000,
                                      fill=MHW), alpha = 0.4)+
  geom_boxplot(aes(mgmt_area, y=dis_per_area*1000000,fill=MHW),
               width = .2, alpha = .6, show.legend = FALSE) + 
  theme_bw()+
  #north
  labs(title = "", y = "Relative Distance x 10e5", 
       x = "", fill = "")+
  geom_label(ks_pvD_NWA %>% filter(mgmt_area %in% c("NED","NEC","MAB")), 
             mapping=aes(x=c(8.2,7.2,6.2), y=200, label = D))+
  geom_label(ks_pvD_NWA %>% filter(mgmt_area %in% c("NED","NEC","MAB")), 
             mapping=aes(x=c(8.2,7.2,6.2), y=150, label = pvalue))+
  geom_text(ks_pvD_NWA %>% filter(mgmt_area %in% c("NED","NEC","MAB")), 
            mapping=aes(x=c(8.2,7.2,6.2),y=178, label = "D:"))+
  geom_text(ks_pvD_NWA %>% filter(mgmt_area %in% c("NED","NEC","MAB")), 
            mapping=aes(x=c(8.2,7.2,6.2),y=116, label = "p-value:"))+
  #south
  geom_label(ks_pvD_NWA %>% filter(mgmt_area %in% c("SAB","SAR","FEC",
                                                    "GOM","CAR")), 
             mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2), y=200, label = D),
             fontface = "bold")+
  geom_label(ks_pvD_NWA %>% filter(mgmt_area %in% c("SAB","SAR","FEC",
                                                    "GOM","CAR")), 
             mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2), y=150, label = pvalue),
             fontface = "bold")+
  geom_text(ks_pvD_NWA %>% filter(mgmt_area %in% c("SAB","SAR","FEC",
                                                   "GOM","CAR")), 
            mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2),y=178, label = "D:"),
            fontface = "bold")+
  geom_text(ks_pvD_NWA %>% filter(mgmt_area %in% c("SAB","SAR","FEC",
                                                   "GOM","CAR")), 
            mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2),y=116, label = "p-value:"),
            fontface = "bold")+
  coord_flip()



######
#TROLL
######
NEPTROLL_CFG_MHW_total<-here("data","mgmt_area_metrics","NEP_TROLL",
                         "NEPTROLL_CFG_MHW_total.rds") %>% 
  readRDS()

month_avgCOG<-NEPTROLL_CFG_MHW_total %>% filter(MHW == 0) %>% 
  group_by(mgmt_area,month) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG_NEP<-NEPTROLL_CFG_MHW_total %>% 
  dplyr::select(MHW,month,event_index,duration_months, 
                mgmt_area,COGx,COGy,prop_MHW_area,
                mean_SSTa,date,prop_habitat_cells, 
                prop_MHW_cells) %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326")


MHWCOG_NEP$distance <- map_dbl(1:nrow(MHWCOG_NEP), function(x){
  x <- MHWCOG_NEP[x,]
  y <- month_avgCOG[month_avgCOG$mgmt_area == x$mgmt_area & month_avgCOG$month == x$month,]
  st_distance(x, y)
})


MHWCOG_NEP<-MHWCOG_NEP %>% mutate(distance = distance * 0.001,
                                  lon = sf::st_coordinates(.)[,1],
                                  lat = sf::st_coordinates(.)[,2])

mgmt_area_area<-st_area(NEP_TROLL_areas)
units(mgmt_area_area)<-NULL #remove the units

area_df<-data.frame(mgmt_area = NEP_TROLL_areas$ET_ID %>% unique(),area = mgmt_area_area*0.000001) #converting to km^2

MHWCOG_NEP<-left_join(MHWCOG_NEP, area_df, by=c("mgmt_area")) %>% 
  mutate(dis_per_area = distance / area)


MHWCOG_NEP$mgmt_area<-factor(MHWCOG_NEP$mgmt_area,
                             levels = rev(c("VN","CL","EK",
                                        "MT","CP")))


#ks test
MHWCOG_NEP<-MHWCOG_NEP %>%
  group_by(mgmt_area,month) %>% 
  mutate(month_mean_reldis = mean(dis_per_area, na.rm=TRUE),
         percent_change_reldis=((dis_per_area - month_mean_reldis)/month_mean_reldis)*100) %>% ungroup()

mgmtareas<-unique(as.character(MHWCOG_NEP$mgmt_area))
ks<-data.frame(mgmt_area=NA, pvalue=NA, D=NA)
counter = 1
for(i in 1:length(mgmtareas)){
  a<-MHWCOG_NEP %>% filter(mgmt_area == mgmtareas[i])
  b<-a %>% filter(MHW == 1)
  c<-a %>% filter(MHW == 0)
  print(mgmtareas[i])
  ks_test<-ks.test(log(c$dis_per_area),log(b$dis_per_area),
                   alternative = 'greater')
  pvalue<-round(ks_test$p.value,4)
  d<-round(ks_test$statistic,4)
  ks[counter,1]<-mgmtareas[i]
  ks[counter,2]<-pvalue
  ks[counter,3]<-d
  
  counter = counter + 1
}

ks_pvD_NEP <- ks


NEP_plot<-MHWCOG_NEP %>%
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
  filter(mgmt_area %in% c("VN","CL","EK","MT")) %>% 
  ggplot()+
  geom_split_violin(aes(mgmt_area, y=dis_per_area*1000000,
                                      fill=MHW), alpha = 0.4,show.legend = FALSE)+
  geom_boxplot(aes(mgmt_area, y=dis_per_area*1000000,fill=MHW),
               width = .2, alpha = .6, show.legend = FALSE) + 
  theme_bw()+
  labs(title = "", y = "Relative Distance x 10e5", 
       x = "")+
  #peripheral
  geom_text(ks_pvD_NEP %>% filter(mgmt_area %in% c("VN","MT")), 
             mapping=aes(x=c(1.2,4.2), y=440, label = "D:"),
             fontface = "bold")+
  geom_text(ks_pvD_NEP %>% filter(mgmt_area %in% c("VN","MT")), 
             mapping=aes(x=c(1.2,4.2), y=255, label = "p-value:"),
             fontface = "bold")+
  geom_label(ks_pvD_NEP %>% filter(mgmt_area %in% c("VN","MT")), 
            mapping=aes(x=c(1.2,4.2),y=505, label = D),
            fontface = "bold")+
  geom_label(ks_pvD_NEP %>% filter(mgmt_area %in% c("VN","MT")), 
            mapping=aes(x=c(1.2,4.2),y=355, label = pvalue),
            fontface = "bold")+
  #central
  geom_text(ks_pvD_NEP %>% filter(mgmt_area %in% c("CL","EK")), 
             mapping=aes(x=c(2.2,3.2), y=440, label = "D:"))+
  geom_text(ks_pvD_NEP %>% filter(mgmt_area %in% c("CL","EK")), 
             mapping=aes(x=c(2.2,3.2), y=260, label = "p-value:"))+
  geom_label(ks_pvD_NEP %>% filter(mgmt_area %in% c("CL","EK")), 
            mapping=aes(x=c(2.2,3.2),y=505, label = D))+
  geom_label(ks_pvD_NEP %>% filter(mgmt_area %in% c("CL","EK")), 
            mapping=aes(x=c(2.2,3.2),y=355, label = pvalue))+
  coord_flip()



F5_Relative_Displacement_distributions<-cowplot::plot_grid(NEP_plot,NWA_plot,labels = c('(A)', '(B)'), label_size = 12,
                   rel_widths = c(1,1.5))

ggsave(here("Plots","F5_Relative_Displacement_distributions.png"),
       width = 10, height = 7, units = "in", dpi = 300)
ggsave(here("Plots","F5_Relative_Displacement_distributions.svg"),
       width = 10, height = 7, units = "in", dpi = 300)
