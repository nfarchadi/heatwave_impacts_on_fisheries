#center of COG
library(here)
library(sf)
library(tidyverse)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NWA_PLL_zones<-sf::st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/NWA_PLL_MangementZone/Zones_PLL.shp", crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_zones<-st_transform(NWA_PLL_zones)

NWA_PLL_zones$ET_ID<-c("CAR","FEC","GOM",
                       "MAB","NCA","NEC",
                       "NED","SAB","SAR",
                       "TUN","TUS")

NWA_PLL_zones<-NWA_PLL_zones[c(1:4,6:9),]



NWAPLL_MHW_summer<-here("data","Mgmt_zone","NWAPLL_MHW_summer.rds") %>% readRDS()
NWAPLL_MHW_winter<-here("data","Mgmt_zone","NWAPLL_MHW_winter.rds") %>% readRDS()
NWAPLL_MHW_total<-rbind(NWAPLL_MHW_summer,NWAPLL_MHW_winter) %>% ungroup()



NWAPLL_MHW_total$MHW<-factor(NWAPLL_MHW_total$MHW,
                              levels = c("Large MHW","Small MHW","Near-Average"))

############################################################
#Plot COG per mgmt zone - mean COG by size
############################################################


NWAPLL_MHW_total%>% 
  group_by(MHW,mgmt_zone) %>% 
  summarise(COGx=mean(COGx, na.rm=TRUE),
            COGy=mean(COGy, na.rm=TRUE),
            COGx.sd=mean(COGx.sd, na.rm=TRUE),
            COGy.sd=mean(COGy.sd, na.rm=TRUE),
            X25=mean(X25, na.rm=TRUE),
            X75=mean(X75, na.rm=TRUE),
            Y25=mean(Y25, na.rm=TRUE),
            Y75=mean(Y75, na.rm=TRUE), .groups = "drop") %>% 
  ggplot() +
  #bring in world data +
  geom_sf(data = NWA_PLL_zones, color = "goldenrod3", fill=NA)+  #bring in EEZ data
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_point(aes(COGx,COGy, color = MHW),shape = 3)+
  geom_errorbar(aes(x=COGx, ymin=COGy.sd, ymax=COGy.sd,color=MHW), 
                show.legend = NA)+
  geom_errorbarh(aes(y=COGy, xmin=COGx.sd, xmax=COGx.sd,color=MHW),
                 show.legend = NA)+
  geom_point(aes(COGx, Y25, color = MHW), show.legend = NA)+
  geom_point(aes(COGx, Y75, color = MHW), show.legend = NA)+
  geom_point(aes(y=COGy, x=X25, color = MHW), show.legend = NA)+
  geom_point(aes(y=COGy, x=X75, color = MHW), show.legend = NA)+
  coord_sf(xlim = c(-100, -30), ylim = c(10, 55), expand = TRUE) +
  ylab("Latitude") + xlab("Longitude") + theme_bw() +
  scale_color_manual(name = "Climate Regime",
                     labels = c("Large MHW", "Small MHW", "Near-Average"), 
                     values = c("red", "orange","blue"))





############################################################
#Plot COG per mgmt zone - mean year COG by size & SST
############################################################

NWAPLL_MHW_total %>% mutate(year = lubridate::year(date)) %>% 
  group_by(MHW, mgmt_zone, year) %>% 
  summarise(COGx=mean(COGx, na.rm=TRUE),
            COGy=mean(COGy, na.rm=TRUE),
            COGx.sd=mean(COGx.sd, na.rm=TRUE),
            COGy.sd=mean(COGy.sd, na.rm=TRUE),
            X25=mean(X25, na.rm=TRUE),
            X75=mean(X75, na.rm=TRUE),
            Y25=mean(Y25, na.rm=TRUE),
            Y75=mean(Y75, na.rm=TRUE),
            prop_MHW_area=mean(prop_MHW_area, na.rm=TRUE),
            mean_SSTa=mean(mean_SSTa, na.rm=TRUE), .groups = "drop") %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  #bring in world data +
  geom_sf(data = NWA_PLL_zones, color = "goldenrod3", fill=NA)+  #bring in EEZ data
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_point(aes(COGx,COGy, fill = year, size = prop_MHW_area), alpha = 0.7,shape=21, color="black")+
  scale_size(range = c(1, 10), name=expression("Proportion of MHW"~(km^2)))+
  coord_sf(xlim = c(-100, -30), ylim = c(10, 55), expand = TRUE) +
  ylab("Latitude") + xlab("Longitude") + theme_bw()+
  scale_fill_viridis(discrete=TRUE, option="mako", name = "Year")+
  guides(fill = guide_legend(override.aes = list(size=5)))


############################################################
#Plot COG per mgmt zone - mean year COG by size and mean ssta
############################################################

NWAPLL_MHW_total %>% mutate(year = lubridate::year(date)) %>% 
  group_by(MHW, mgmt_zone, year) %>% 
  summarise(COGx=mean(COGx, na.rm=TRUE),
            COGy=mean(COGy, na.rm=TRUE),
            COGx.sd=mean(COGx.sd, na.rm=TRUE),
            COGy.sd=mean(COGy.sd, na.rm=TRUE),
            X25=mean(X25, na.rm=TRUE),
            X75=mean(X75, na.rm=TRUE),
            Y25=mean(Y25, na.rm=TRUE),
            Y75=mean(Y75, na.rm=TRUE),
            prop_MHW_area=mean(prop_MHW_area, na.rm=TRUE),
            mean_SSTa=mean(mean_SSTa, na.rm=TRUE), .groups = "drop") %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot() +
  #bring in world data +
  geom_sf(data = NWA_PLL_zones, color = "goldenrod3", fill=NA)+  #bring in EEZ data
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_point(aes(COGx,COGy, fill = mean_SSTa, size = prop_MHW_area), alpha = 0.7,
             shape=21, color="black")+
  scale_size(range = c(1, 10), name=expression("Proportion\nof MHW"(km^2)))+
  coord_sf(xlim = c(-100, -30), ylim = c(10, 55), expand = TRUE) +
  ylab("Latitude") + xlab("Longitude") + theme_bw()+
  scale_fill_viridis(discrete=FALSE, option="inferno", 
                     name=stringr::str_wrap("SSTa\nMagnitude (°C)", width = 13))

########################################################
#Plot near avg and large MWH condition COG per mgmt zone 
########################################################
NWAPLL_MHW_total %>%  
  filter(MHW %in% c("Large MHW", "Near-Average")) %>% 
  group_by(MHW,mgmt_zone) %>% 
  summarise(COGx=mean(COGx, na.rm=TRUE),
            COGy=mean(COGy, na.rm=TRUE),
            COGx.sd=mean(COGx.sd, na.rm=TRUE),
            COGy.sd=mean(COGy.sd, na.rm=TRUE),
            X25=mean(X25, na.rm=TRUE),
            X75=mean(X75, na.rm=TRUE),
            Y25=mean(Y25, na.rm=TRUE),
            Y75=mean(Y75, na.rm=TRUE), .groups = "drop") %>% 
  arrange(mgmt_zone,desc(MHW)) %>% 
  ggplot() +
  #bring in world data +
  geom_sf(data = NWA_PLL_zones, color = "goldenrod3", fill=NA)+  #bring in EEZ data
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_point(aes(COGx,COGy, color = MHW),shape = 3)+
  geom_errorbar(aes(x=COGx, ymin=COGy.sd, ymax=COGy.sd,color=MHW), 
                show.legend = NA)+
  geom_errorbarh(aes(y=COGy, xmin=COGx.sd, xmax=COGx.sd,color=MHW),
                 show.legend = NA)+
  geom_point(aes(COGx, Y25, color = MHW), show.legend = NA)+
  geom_point(aes(COGx, Y75, color = MHW), show.legend = NA)+
  geom_point(aes(y=COGy, x=X25, color = MHW), show.legend = NA)+
  geom_point(aes(y=COGy, x=X75, color = MHW), show.legend = NA)+
  geom_path(aes(x=COGx,COGy, group = mgmt_zone), color = "black",
            size = .75,
            arrow = arrow(length = unit(.2, "in")), 
            show.legend = FALSE)+
  coord_sf(xlim = c(-100, -30), ylim = c(10, 55), expand = TRUE) +
  ylab("Latitude") + xlab("Longitude") + theme_bw() +
  scale_color_manual(name = "Climate Regime",
                     labels = c("Large MHW", "Near-Average"), 
                     values = c("red","blue"))+
  ggspatial::annotation_scale()



# #################################################
# #Plot habitat suitability per mgmt zone per year
# #################################################
# NWAPLL_MHW_total<-NWAPLL_MHW_total %>% mutate(year = lubridate::year(date))
# 
# 
# # NWAPLL_year_mgmtzone_habitat.area<-NWAPLL_MHW_total %>%
# #   dplyr::select(date,mgmt_zone,MHW) %>% 
# #   group_by(mgmt_zone) %>% 
# #   mutate(consecutive_months = data.table::rleid(MHW)) %>% 
# #   filter(MHW == "Large MHW") %>% 
# #   group_by(mgmt_zone,consecutive_months) %>% 
# #   filter(n() != 1) %>%
# #   mutate(year = lubridate::year(date),
# #          MHW_year = 1) %>%
# #   ungroup() %>% 
# #   dplyr::select(mgmt_zone,year,MHW_year) %>%
# #   full_join(.,NWAPLL_MHW_total, by=c("year","mgmt_zone")) %>%
# #   mutate(MHW_year = case_when(MHW_year == 1 ~ 1,
# #                          TRUE ~ 0)) %>%
# #   group_by(mgmt_zone, year, MHW_year) %>% 
# #   summarise(habitat.area.mean = mean(NWA_PLL.total.area, na.rm = TRUE), 
# #             .groups = "drop")
#   
# 
# NWAPLL_year_mgmtzone_habitat.area<-NWAPLL_MHW_total %>%
#   #dplyr::select(date,mgmt_zone,MHW) %>% 
#   #group_by(mgmt_zone) %>% 
#   #mutate(consecutive_months = data.table::rleid(MHW)) %>% 
#   #filter(MHW == "Large MHW") %>% 
#   #group_by(mgmt_zone,consecutive_months) %>% 
#   #filter(n() != 1) %>%
#   mutate(year = lubridate::year(date)) %>%
#   # ungroup() %>% 
#   # dplyr::select(mgmt_zone,year,MHW_year) %>%
#   # full_join(.,NWAPLL_MHW_total, by=c("year","mgmt_zone")) %>%
#   # mutate(MHW_year = case_when(MHW_year == 1 ~ 1,
#   #                             TRUE ~ 0)) %>%
#   group_by(mgmt_zone, year, MHW) %>% 
#   summarise(habitat.area.mean = mean(NWA_PLL.total.area, na.rm = TRUE), 
#             .groups = "drop") 
# 
# 
# NWAPLL_total_mgmtzone_habitat.area<-NWAPLL_MHW_total %>% 
#   mutate(year = lubridate::year(date)) %>% 
#   group_by(mgmt_zone) %>% 
#   summarise(habitat.area.total = mean(NWA_PLL.total.area, na.rm = TRUE), 
#             .groups = "drop")
# 
# 
# habitat_change<-NWAPLL_year_mgmtzone_habitat.area %>% 
#   left_join(., NWAPLL_total_mgmtzone_habitat.area, by=c("mgmt_zone")) %>%
#   mutate(habitat_change = 
#            (habitat.area.mean - habitat.area.total)/habitat.area.total,
#          change_direction = if_else(habitat_change >= 0, 1,0))
# 
# habitat_change$mgmt_zone<-factor(habitat_change$mgmt_zone,
#                                  levels = c("SAR","SAB","GOM",
#                                             "FEC","CAR","NED",
#                                             "NEC","MAB"))
# 
# habitat_change$change_direction<-factor(habitat_change$change_direction)
# 
# ggplot(habitat_change, aes(x=mgmt_zone,y=habitat_change, fill = change_direction))+
#   geom_bar(position="dodge", stat="identity")+
#   scale_fill_manual(values = c("blue","red"))+
#   facet_grid(MHW~year)+
#   theme_bw()+
#   ggtitle("Habitat change (MHW size class yearly mean - overall mean / overall mean)") +
#   labs(y="Percentage change", x="Date") +theme(legend.position="")+
#   geom_vline(xintercept = 5.5, color = "black")+
#   coord_flip()

#########################################
#monthly total area anomaly
#########################################

#loading in northern region data
MAB_NEC_NED_MHW<-here("data","water_temp","NWA","oisst",
                      "MAB_NEC_NED_MHW.rds") %>% readRDS() %>% na.omit()

MAB_NEC_NED_MHW$yearmon<-zoo::as.yearmon(MAB_NEC_NED_MHW$yearmon)

#averaging over the pixels per mgmt zone and yearmon
MAB_NEC_NED_MHW<-MAB_NEC_NED_MHW %>% 
  group_by(yearmon, ET_ID) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  mutate(MHW = ifelse(detrend >= seas, 1, 0))

#loading in southern region
GOM_CAR_FEC_SAR_SAB_MHW<-here("data","water_temp","NWA","oisst",
                              "GOM_CAR_FEC_SAR_SAB_MHW.rds") %>% readRDS()

#averaging over the pixels per mgmt zone and yearmon
GOM_CAR_FEC_SAR_SAB_MHW<-GOM_CAR_FEC_SAR_SAB_MHW %>% 
  group_by(yearmon, ET_ID) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  mutate(MHW = ifelse(detrend >= seas, 1, 0))


#combine
MHW_mgmtzone_all<-rbind(MAB_NEC_NED_MHW,GOM_CAR_FEC_SAR_SAB_MHW) %>% 
  rename("mgmt_zone"="ET_ID",
         "date"="yearmon")


#calculate monthly anomaly
NWAPLLa_MHW_total<-NWAPLL_MHW_total %>% 
  group_by(mgmt_zone,month) %>% 
  mutate(year = lubridate::year(date),
         month_mean = mean(NWA_PLL.total.area, na.rm=TRUE),
         month_SD = mean(NWA_PLL.total.area, na.rm=TRUE),
         NWA_PLL.area.anomaly =
           ((NWA_PLL.total.area - month_mean)/month_mean)*100) %>% 
  ungroup() %>% 
  mutate(change_direction = if_else(NWA_PLL.area.anomaly <= 0, 1,0)) %>% 
  left_join(., MHW_mgmtzone_all, by=c("mgmt_zone","date"))



NWAPLLa_MHW_total$mgmt_zone<-factor(NWAPLLa_MHW_total$mgmt_zone,
                                    levels = c("NED","NEC","MAB",
                                               "SAB","SAR","FEC",
                                               "GOM","CAR"))

NWAPLLa_MHW_total$change_direction<-factor(NWAPLLa_MHW_total$change_direction)
#NWAPLLa_MHW_total$MHW.y<-factor(NWAPLLa_MHW_total$MHW.y)


#pearson correlation between the two
NWAPLLa_MHW_total<-NWAPLLa_MHW_total %>% 
  group_by(mgmt_zone) %>% 
  summarise(corr = cor(NWA_PLL.area.anomaly,detrend)) %>% 
  left_join(., NWAPLLa_MHW_total, by=c("mgmt_zone"))



northern_zones<-ggplot(NWAPLLa_MHW_total %>% filter(mgmt_zone %in% c("NED","NEC","MAB")), aes(x=date))+
  geom_bar(aes(y=NWA_PLL.area.anomaly, fill=change_direction),position="dodge", stat="identity")+
  geom_line(aes(y=detrend*20, color = MHW.y))+
  scale_y_continuous(name = "Habitat Change (%)",
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(trans=~.*.02, 
                                         name= expression("Detrended SSTa"~(degree~C))))+
  facet_wrap(~mgmt_zone, scales = "free", ncol = 1)+
  scale_fill_manual(values = c("blue","red"))+
  scale_colour_gradient(low = "black", high = "red")+
  geom_label(aes(label=paste("r =",round(corr,2))), 
             x = Inf, y = Inf, hjust=1, vjust=1)+
  theme_bw()+
  ggtitle("") +
  labs(x="Date") +theme(legend.position="",
                        strip.background = element_blank())

northern_zones

southern_zones<-ggplot(NWAPLLa_MHW_total %>% filter(mgmt_zone %in% c("SAB","SAR","FEC","GOM","CAR")), aes(x=date))+
  geom_bar(aes(y=NWA_PLL.area.anomaly, fill=change_direction),position="dodge", stat="identity")+
  geom_line(aes(y=detrend*20, color = MHW.y))+
  scale_y_continuous(name = "Habitat Change (%)",
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(trans=~.*.02, 
                                         name=expression("Detrended SSTa"~(degree~C))))+
  facet_wrap(~mgmt_zone, scales = "free", ncol = 1)+
  scale_fill_manual(values = c("blue","red"))+
  scale_colour_gradient(low = "black", high = "red")+
  geom_label(aes(label=paste("r =",round(corr,2))), 
             x = Inf, y = Inf, hjust=1, vjust=1)+
  theme_bw()+
  ggtitle("") +
  labs(x="Date") +theme(legend.position="",
                        strip.background = element_blank())

southern_zones
