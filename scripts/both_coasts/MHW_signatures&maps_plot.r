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

#subsetting shapefile
NWA_PLL_zones_subset<-NWA_PLL_zones %>% 
  filter(ET_ID %in% c("NED","MAB","FEC","CAR"))

###########################################
#Plotting MHW timeseries for each mgmt zone
###########################################

#FYI need to get MHW_mgmtzone_all from 11_COG_HabitatChange_analysis.R
MHW_mgmtzone_all_NWA<-MHW_mgmtzone_all
MHW_mgmtzone_NWA_subset<-MHW_mgmtzone_all_NWA %>% 
  filter(mgmt_zone %in% c("NED","MAB","FEC","CAR"))

may_2012_mhw_NWA_circle<-MHW_mgmtzone_all_NWA %>% 
  filter(mgmt_zone %in% c("NED","MAB","FEC","CAR")) %>% 
  filter(date == as.yearmon("2012-05-01"))

ts_labels_NWA<-data.frame(mgmt_zone = factor(c("NED","MAB","FEC","CAR")),
                      label = c("(G)","(H)","(I)","(J)"))

NWA_mhw_signature<-ggplot() +
  geom_flame(MHW_mgmtzone_NWA_subset, mapping = aes(x = date, y = detrend, y2 = seas)) +
  geom_line(MHW_mgmtzone_NWA_subset, mapping = aes(x = date, y = detrend))+
  geom_line(MHW_mgmtzone_NWA_subset, mapping = aes(x = date, y = seas),color = "forestgreen",linetype = "dashed", size = .5)+
  facet_wrap(~mgmt_zone, nrow = 4)+
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
  

################################################
#SSTa raster map of a certain MHW (MHW Intensity)
################################################
time_vec <- seq.POSIXt(as.POSIXct('2012-01-01', tz='UTC'), as.POSIXct('2020-12-01', tz='UTC'), by='month')

fList <- list.files("E:/OISST/SSTa", full.names = TRUE, pattern = ".nc")

fList_years <- c()
for (i in 1:length(fList)) fList_years[i] <- substr(fList[i], 26, 29)

ssta_stack <- raster::stack(fList[which(fList_years %in% lubridate::year(time_vec))])

names(ssta_stack) <- time_vec

may_2012_mhw_NWA<-rasterToPoints(ssta_stack[[5]]) %>% as.data.frame() %>%  
  rename("SSTa" = "X2012.05.01")


#center coordinates for each mgmt_zone
mgmtzone_centroid_NWA<-NWA_PLL_zones_subset %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtzone_centroid_NWA<-cbind(NWA_PLL_zones_subset$ET_ID,mgmtzone_centroid_NWA)
colnames(mgmtzone_centroid_NWA)<-c("mgmt_zone","X","Y")

NWA_mhw<-may_2012_mhw_NWA %>% 
  ggplot() +
  geom_tile(aes(x=x,y=y, fill = SSTa))+
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_sf(data = NWA_PLL_zones_subset, color = "black", fill = NA, size = 1)+
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
  geom_label(data = mgmtzone_centroid_NWA[1,], aes(x = X+5, y = Y+3.5, #CAR
                                               label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid_NWA[2,], aes(x = X-7, y = Y+2,#FEC
                                                 label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid_NWA[3,], aes(x = X-6, y = Y,#MAB
                                               label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid_NWA[4,], aes(x = X-4, y = Y+4,#NED
                                               label = mgmt_zone),
             fontface = "bold")+
  geom_text(data=data.frame(), aes(label = '(F)', x = -Inf, y = Inf),
            hjust = 0, vjust = 1, fontface="bold")

NWA_mhw / NWA_mhw_signature  



###############################################################################
#CCS
###############################################################################

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NEP_TROLL_zones<-here("data","shapefiles","NEP_TROLL","Zones_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NEP_TROLL_zones<-st_difference(NEP_TROLL_zones, st_union(st_combine(land)))

NEP_TROLL_zones$ET_ID<-factor(NEP_TROLL_zones$ET_ID,
                            levels = c("VN","CL","EK",
                                       "MT","CP"))

#subsetting shapefile
NEP_TROLL_zones_subset<-NEP_TROLL_zones %>% 
  filter(ET_ID %in% c("VN","CL","EK","MT"))

###########################################
#Plotting MHW timeseries for each mgmt zone
###########################################
#FYI need to get MHW_mgmtzone_all from 11_COG_HabitatChange_analysis.R
MHW_mgmtzone_all_NEP<-MHW_mgmtzone_all
MHW_mgmtzone_NEP_subset<-MHW_mgmtzone_all_NEP %>% 
  filter(mgmt_zone %in% c("VN","CL","EK","MT")) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("VN","CL","EK","MT")))


may_2015_mhw_NEP_circle<-MHW_mgmtzone_NEP_subset %>%
  filter(date == as.yearmon("2015-05-01"))

ts_labels_NEP<-data.frame(mgmt_zone = factor(c("VN","CL","EK","MT")),
                      label = c("(B)","(C)","(D)","(E)"))

NEP_mhw_signature<-ggplot() +
  geom_flame(MHW_mgmtzone_NEP_subset, mapping = aes(x = date, y = detrend, y2 = seas)) +
  geom_line(MHW_mgmtzone_NEP_subset, mapping = aes(x = date, y = detrend))+
  geom_line(MHW_mgmtzone_NEP_subset, mapping = aes(x = date, y = seas),color = "forestgreen",linetype = "dashed", size = .5)+
  facet_wrap(~mgmt_zone, nrow = 4)+
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
may_2015_mhw_NEP<-here("data","water_temp",
                                                        "NEP","oisst","Conception_Monterey_Eureka_Columbia_Vancouver_MHW.rds") %>% readRDS() %>% 
  filter(yearmon == as.yearmon("2015-05-01"))


#center coordinates for each mgmt_zone
mgmtzone_centroid_NEP<-NEP_TROLL_zones_subset %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtzone_centroid_NEP<-cbind(NEP_TROLL_zones_subset$ET_ID,mgmtzone_centroid_NEP)
colnames(mgmtzone_centroid_NEP)<-c("mgmt_zone","X","Y")

NEP_mhw<-may_2015_mhw_NEP %>% 
  ggplot() +
  geom_tile(aes(x=lon,y=lat, fill = detrend))+
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_sf(data = NEP_TROLL_zones_subset, color = "black", fill = NA, size = 1)+
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
  geom_label(data = mgmtzone_centroid_NEP[4,], aes(x = X+7, y = Y,#VN
                                                   label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid_NEP[3,], aes(x = X+7, y = Y,#CL
                                                label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid_NEP[2,], aes(x = X+7.5, y = Y,#EK
                                                label = mgmt_zone),
             fontface = "bold")+
  geom_label(data = mgmtzone_centroid_NEP[1,], aes(x = X+8, y = Y+1,#MT
                                               label = mgmt_zone),
             fontface = "bold")+
  geom_text(data=data.frame(), aes(label = '(A)', x = -119, y = Inf),
            hjust = 0, vjust = 1, fontface="bold")

(NEP_mhw | NWA_mhw)/ (NEP_mhw_signature | NWA_mhw_signature)  


##########################################################################
#all the NWA zones
##########################################################################

#FYI need to get MHW_mgmtzone_all from 11_COG_HabitatChange_analysis.R
MHW_mgmtzone_all_NWA<-MHW_mgmtzone_all


ts_labels_NWA<-data.frame(mgmt_zone = factor(c("NED","NEC","MAB",
                                               "SAB","SAR", "FEC",
                                               "GOM", "CAR")),
                          label = c("(A)","(B)","(C)","(D)",
                                    "(E)","(F)","(G)","(H)"))

ggplot() +
  geom_flame(MHW_mgmtzone_all_NWA, mapping = aes(x = date, y = detrend, y2 = seas)) +
  geom_line(MHW_mgmtzone_all_NWA, mapping = aes(x = date, y = detrend))+
  geom_line(MHW_mgmtzone_all_NWA, mapping = aes(x = date, y = seas),color = "forestgreen",linetype = "dashed", size = .5)+
  facet_wrap(~mgmt_zone, nrow = 4)+
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

##########################################################################
#all the NEP zones now
##########################################################################

#FYI need to get MHW_mgmtzone_all from 11_COG_HabitatChange_analysis.R
MHW_mgmtzone_all_NEP<-MHW_mgmtzone_all


ts_labels_NEP<-data.frame(mgmt_zone = factor(c("VN","CL","EK",
                                               "MT","CP")),
                          label = c("(A)","(B)","(C)","(D)",
                                    "(E)"))

ggplot() +
  geom_flame(MHW_mgmtzone_all_NEP, mapping = aes(x = date, y = detrend, y2 = seas)) +
  geom_line(MHW_mgmtzone_all_NEP, mapping = aes(x = date, y = detrend))+
  geom_line(MHW_mgmtzone_all_NEP, mapping = aes(x = date, y = seas),color = "forestgreen",linetype = "dashed", size = .5)+
  facet_wrap(~mgmt_zone, nrow = 4)+
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
  geom_text(data=ts_labels_NEP, aes(label = label, x = -Inf, y = Inf),
            hjust = 0, vjust = 1, fontface="bold")
  

############
#MHW metrics
############

#PLL
NWAPLL_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_total.rds") %>% 
  readRDS() %>% dplyr::select(mgmt_zone,MHW,date,prop_MHW_cells,mean_SSTa,prop_habitat_cells, duration_months, event_index)

#TROLL
NEPTROLL_MHW_total<-here("data","Mgmt_zone","NEP_TROLL",
                         "NEPTROLL_MHW_total.rds") %>% 
  readRDS() %>% filter(mgmt_zone != "CP") %>% 
  dplyr::select(mgmt_zone,MHW,date,prop_MHW_cells,mean_SSTa,prop_habitat_cells, duration_months, event_index)

MHW_total<-rbind(NWAPLL_MHW_total,NEPTROLL_MHW_total) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("VN","CL","EK","MT",
                                                  "NED","NEC","MAB","SAB",
                                                  "SAR","FEC","GOM","CAR")))


MHW_total %>% filter(MHW == 1) %>% 
  group_by(mgmt_zone) %>% 
  summarise(Totalmonths=sum(MHW),
            mediansize=quantile(prop_MHW_cells)[3] %>% round(.,2),
            Q25size = quantile(prop_MHW_cells)[2]%>% round(.,2),
            Q75size = quantile(prop_MHW_cells)[4]%>% round(.,2),
            medianintensity=quantile(mean_SSTa)[3]%>% round(.,2),
            Q25intensity = quantile(mean_SSTa)[2]%>% round(.,2),
            Q75intensity = quantile(mean_SSTa)[4]%>% round(.,2),
            medianhabsize=quantile(prop_habitat_cells)[3]%>% round(.,2),
            Q25habsize = quantile(prop_habitat_cells)[2]%>% round(.,2),
            Q75habsize = quantile(prop_habitat_cells)[4]%>% round(.,2),) %>% 
  View()

MHW_total %>% filter(MHW == 1) %>% 
  group_by(mgmt_zone) %>% 
  summarise(Totalmonths=sum(MHW),
            meansize=mean(prop_MHW_cells, na.rm=TRUE) %>% round(.,2),
            Minsize = min(prop_MHW_cells) %>% round(.,2),
            Maxsize = max(prop_MHW_cells) %>% round(.,2),
            meanintensity=mean(mean_SSTa, na.rm=TRUE)%>% round(.,2),
            Minintensity = min(mean_SSTa) %>% round(.,2),
            Maxintensity = max(mean_SSTa) %>% round(.,2),
            meanhabsize=mean(prop_habitat_cells, na.rm=TRUE)%>% round(.,2),
            Minhabsize = min(prop_habitat_cells) %>% round(.,2),
            Maxhabsize = max(prop_habitat_cells) %>% round(.,2),
            Minduration = min(duration_months) %>% round(.,2),
            Maxduration = max(duration_months) %>% round(.,2)) %>% 
  View()


a<-MHW_total %>% 
  ggplot(aes(x = mgmt_zone, y=mean_SSTa))+
  geom_boxplot(color = "red")+
  theme_bw() +
  labs(x = "Management Area",y = "MHW Intensity")

b<-MHW_total %>%
  ggplot(aes(x = mgmt_zone, y=prop_MHW_cells))+
  geom_boxplot(color = "Orange")+
  theme_bw() +
  labs(x = "Management Area",y = "MHW Size")

c<-MHW_total %>% filter(MHW == 1) %>%
  dplyr::select(mgmt_zone,event_index,duration_months) %>% unique() %>%  
  ggplot(aes(x = mgmt_zone, y=duration_months))+
  geom_boxplot(color = "gold")+
  theme_bw() +
  labs(x = "Management Area",y = "MHW Duration")


# d<-MHW_total %>%
#   ggplot(aes(x = mgmt_zone, y=prop_habitat_cells))+
#   geom_boxplot(color = "blue")+
#   theme_bw()+
#   labs(x = "Management Area",y = "Habitat Size")

d<-MHW_total %>% filter(MHW == 1) %>% 
  group_by(mgmt_zone) %>% 
  summarise(Totalmonths=sum(MHW)) %>% 
  ggplot(aes(x = mgmt_zone, y=Totalmonths))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75, fill = "salmon")+
  theme_bw()+
  labs(x = "Management Area",y = "Total MHW Months")

#frequency
d<-MHW_total %>% filter(MHW == 1) %>%
  dplyr::select(mgmt_zone,event_index,MHW) %>% unique() %>% 
  group_by(mgmt_zone) %>% summarise(freq=sum(MHW) / (9)) %>%  
  ggplot(aes(x = mgmt_zone, y=freq))+
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75, fill = "salmon")+
  theme_bw()+
  labs(x = "Management Area",y = expression("MHW "~"Frequency "~(y^-1)))


cowplot::plot_grid(a,b,c,d, align = "hv",labels = c('(A)','(B)','(C)','(D)'), label_size = 10,nrow=2)


#spatially correlated by mgmt areas
#size
MHW_total %>% filter(MHW == 1) %>%
  filter(mgmt_zone %in% c("NED","NEC","MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>% 
  dplyr::select(mgmt_zone,prop_MHW_cells,date) %>% 
  spread(mgmt_zone,prop_MHW_cells) %>% 
  dplyr::select(-date) %>%
  #cor(.,use="pairwise.complete.obs") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  cor(.) %>% 
  corrplot::corrplot(type="upper")
  
###intensity 
MHW_total %>% filter(MHW == 1) %>%
  filter(mgmt_zone %in% c("NED","NEC","MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>% 
  dplyr::select(mgmt_zone,mean_SSTa,date) %>% 
  spread(mgmt_zone,mean_SSTa) %>% 
  dplyr::select(-date) %>%
  #cor(.,use="pairwise.complete.obs") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  cor(.) %>% 
  corrplot::corrplot(type="upper")



intensity_aov<-MHW_total %>% filter(MHW == 1) %>%
  filter(mgmt_zone %in% c("NED","NEC","MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>% 
  dplyr::select(mgmt_zone,mean_SSTa) %>% 
  aov(mean_SSTa ~ mgmt_zone, data = .)

summary(intensity_aov)

plot(TukeyHSD(intensity_aov, conf.level=.95))

intensity_df<-MHW_total %>% filter(MHW == 1) %>%
  filter(mgmt_zone %in% c("NED","NEC","MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>% 
  dplyr::select(mgmt_zone,mean_SSTa) 

#pairwise t-test 
pairwise.t.test(intensity_df$mean_SSTa,intensity_df$mgmt_zone) 

###size
size_aov<-MHW_total %>% filter(MHW == 1) %>%
  filter(mgmt_zone %in% c("NED","NEC","MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>% 
  dplyr::select(mgmt_zone,prop_MHW_cells) %>% 
  aov(prop_MHW_cells ~ mgmt_zone, data = .)

summary(size_aov)

TukeyHSD(size_aov, conf.level=.95)


size_df<-MHW_total %>% filter(MHW == 1) %>%
  filter(mgmt_zone %in% c("NED","NEC","MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>% 
  dplyr::select(mgmt_zone,prop_MHW_cells) 

#pairwise t-test with bonferroni p-value adjustment
pairwise.t.test(size_df$prop_MHW_cells,size_df$mgmt_zone) 


