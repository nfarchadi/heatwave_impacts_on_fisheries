library(here)
library(sf)
library(tidyverse)
library(viridis)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(heatwaveR)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_zones<-st_transform(NWA_PLL_zones)

#######################################################
#IDing MHW months by spatially averaging w/in mgmt zone
#######################################################

#loading in northern region data
MAB_NEC_NED_MHW<-here("data","water_temp","NWA","oisst",
                      "MAB_NEC_NED_MHW.rds") %>% readRDS()

MAB_NEC_NED_MHW$yearmon<-zoo::as.yearmon(MAB_NEC_NED_MHW$yearmon)

#averaging over the pixels per mgmt zone and yearmon
MAB_NEC_NED_MHW<-MAB_NEC_NED_MHW %>% 
  filter(yearmon >= 2012) %>% 
  group_by(yearmon, ET_ID) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  mutate(MHW = ifelse(detrend >= seas, 1, 0)) %>%
  #categorizing the intensity of the MHW
  mutate(MHW_category = case_when(detrend >= seas*1.75 ~ "Extreme",
                             detrend >= seas*1.5 ~ "Severe",
                             detrend >= seas*1.25 ~ "Strong",
                             detrend >= seas ~ "Moderate",
                             TRUE ~ "Near-Average"))

#loading in southern region
GOM_CAR_FEC_SAR_SAB_MHW<-here("data","water_temp","NWA","oisst",
                              "GOM_CAR_FEC_SAR_SAB_MHW.rds") %>% readRDS()

#averaging over the pixels per mgmt zone and yearmon
GOM_CAR_FEC_SAR_SAB_MHW<-GOM_CAR_FEC_SAR_SAB_MHW %>% 
  filter(yearmon >= 2012) %>%
  group_by(yearmon, ET_ID) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  mutate(MHW = ifelse(detrend >= seas, 1, 0)) %>% 
  #categorizing the intensity of the MHW
  mutate(MHW_category = case_when(detrend >= seas*1.75 ~ "Extreme",
                                  detrend >= seas*1.5 ~ "Severe",
                                  detrend >= seas*1.25 ~ "Strong",
                                  detrend >= seas ~ "Moderate",
                                  TRUE ~ "Near-Average"))



#combine
MHW_mgmtzone_all<-rbind(MAB_NEC_NED_MHW,GOM_CAR_FEC_SAR_SAB_MHW) %>% 
  rename("mgmt_zone"="ET_ID",
         "date"="yearmon")

MHW_mgmtzone_all$mgmt_zone<-factor(MHW_mgmtzone_all$mgmt_zone,
                                   levels = c("NED","NEC","MAB",
                                              "SAB","SAR","FEC",
                                              "GOM","CAR"))

rm(MAB_NEC_NED_MHW,GOM_CAR_FEC_SAR_SAB_MHW)

###########################################
#Plotting MHW timeseries for each mgmt zone
###########################################
MHW_mgmtzone_all %>% 
  ggplot(aes(x = date)) +
  geom_flame(aes(y = detrend, y2 = seas)) +
  geom_line(aes(y = detrend, colour = "temp"))+
  geom_line(aes(y = seas, colour = "thresh"),linetype = "dashed", size = .5)+
  scale_color_manual(name = "",
                      values = c("black","forestgreen"),
                     labels = c("SSTa", "Threshold"))+
  scale_linetype_manual(values = c("solid", "dashed"))+
  theme_bw()+ theme(legend.position=c(.85,.14),
                    legend.text=element_text(size=16),
                    legend.key.width = unit(1.3,"cm"),
                    panel.spacing = unit(.70, "lines")) +
  guides(color = guide_legend(override.aes = list(size = 1.5,
                                                  linetype = c(1,2))))+
  labs(x = "", y=expression("SSTa "(degree~C))) + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2012-01-01"),zoo::as.yearmon("2020-12-31")))+
  facet_wrap(~mgmt_zone, nrow = 6)

#combine PLL landscape metric datasets 
NWAPLL_MHW_summer<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_summer.rds") %>% readRDS()
NWAPLL_MHW_winter<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_winter.rds") %>% readRDS()
NWAPLL_MHW_total<-rbind(NWAPLL_MHW_summer,NWAPLL_MHW_winter) %>% ungroup()


#tiding up the data
NWAPLL_MHW_total$MHW<-factor(NWAPLL_MHW_total$MHW,
                             levels = c("Large MHW","Small MHW","Near-Average"))

NWAPLL_MHW_total<-NWAPLL_MHW_total %>% mutate(year = lubridate::year(date))

NWAPLL_MHW_total$mgmt_zone<-factor(NWAPLL_MHW_total$mgmt_zone,
                                   levels = c("NED","NEC","MAB",
                                              "SAB","SAR","FEC",
                                              "GOM","CAR"))


#joining the two
NWAPLL_MHW_total<-NWAPLL_MHW_total %>% 
  left_join(., MHW_mgmtzone_all, by=c("mgmt_zone","date")) %>% 
  rename("size_category"="MHW.x","MHW"="MHW.y")



########################################
#Finding the event duration for each MHW
########################################

#indexing each event by mgmt_zone
NWAPLL_MHW_total<-NWAPLL_MHW_total %>%
  dplyr::select(date,mgmt_zone,MHW) %>%
  group_by(mgmt_zone) %>%
  mutate(event_index = data.table::rleid(MHW)) %>% ungroup() %>% 
  left_join(., NWAPLL_MHW_total, by=c("date", "mgmt_zone","MHW"))

#counting the duration months and the sequence for each event
NWAPLL_MHW_total<-NWAPLL_MHW_total %>%
  group_by(mgmt_zone, event_index, MHW) %>% 
  summarise(duration_months = n(), .groups = "drop") %>%
  ungroup() %>% 
  left_join(., NWAPLL_MHW_total, 
            by=c("mgmt_zone","MHW", "event_index")) %>% 
  group_by(mgmt_zone, event_index) %>%
  mutate(sequence_months = seq_along(event_index)) %>% 
  ungroup() 
# %>%
#   mutate(sequence_months = ifelse(MHW == 0,NA,sequence_months))

#taking the average metrics for each event
NWAPLL_MHW_events<-NWAPLL_MHW_total %>%
  group_by(mgmt_zone,event_index,MHW, duration_months) %>% 
  summarise(prop_habitat_area_event = mean(prop_habitat_area, na.rm = TRUE),
            prop_habitat_cells_event = mean(prop_habitat_cells, na.rm=TRUE),
            prop_MHW_area_event = mean(prop_MHW_area, na.rm = TRUE),
            prop_MHW_cells_event = mean(prop_MHW_cells, na.rm=TRUE),
            NWA_PLL.total.area_event = mean(NWA_PLL.total.area, na.rm=TRUE),
            NWA_MHW.total.area_event = mean(NWA_MHW.total.area, na.rm=TRUE),
            mean_SSTa_event = weighted.mean(mean_SSTa,n.cell_MHW, na.rm = TRUE),
            max_SSTa_event = max(max_SSTa, na.rm = TRUE),.groups = "drop") %>%
  filter(MHW == 1) #only want MHW events 

NWAPLL_MHW_events$mgmt_zone<-factor(NWAPLL_MHW_events$mgmt_zone,
                                    levels = c("NED","NEC","MAB",
                                               "SAB","SAR","FEC",
                                               "GOM","CAR"))

#now I have two datasets 
#1) NWAPLL_MHW_total - monthly data for each mgmt zone
#2) NWAPLL_MHW_events - summarized metrics for each MHW

#now I have two datasets 
#1) NWAPLL_MHW_total - monthly data for each mgmt zone

saveRDS(NWAPLL_MHW_total, 
        here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_total.rds"))

#2) NWAPLL_MHW_events - summarized metrics for each MHW

saveRDS(NWAPLL_MHW_events, 
        here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_events.rds"))

############################################################
#proportion of suitable fishing habitat vs proportion of MHW, 
#mean SSTa, and duration
############################################################


NWAPLL_MHW_events %>%  
  ggplot(aes(x=prop_MHW_area_event, y=prop_habitat_area_event,
             color = mean_SSTa_event))+
  scale_size(range = c(1, 7), name="Duration (Months)")+
  geom_point(aes(size = duration_months))+
  geom_smooth(method='lm', formula= y~x, show.legend = FALSE)+
  scale_color_viridis(name=stringr::str_wrap("SSTa\nMagnitude (°C)", width = 13),
                      option ="inferno",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 12,
                                             title.position = "top",
                                             title.hjust = .5))+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x=expression("Proportion of MHW"~(km^2)), y=expression("Proportion of Suitable Fishing Habitat"~(km^2)))

NWAPLL_MHW_total %>%  
  ggplot(aes(x=prop_MHW_area, y=prop_habitat_area,
             color = mean_SSTa))+
  scale_size(range = c(1, 7), name="Duration (Months)")+
  geom_point()+
  geom_smooth(method='lm', formula= y~x, show.legend = FALSE)+
  scale_color_viridis(name=stringr::str_wrap("SSTa\nMagnitude (°C)", width = 13),
                      option ="inferno",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 12,
                                             title.position = "top",
                                             title.hjust = .5))+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x=expression("Proportion of MHW"~(km^2)), y=expression("Proportion of Suitable Fishing Habitat"~(km^2)))

############################################################
#proportion of suitable fishing habitat vs proportion of MHW, 
#max SSTa, and duration
############################################################

NWAPLL_MHW_events %>% 
  ggplot(aes(x=prop_MHW_area_event, y=prop_habitat_area_event,
             color = max_SSTa_event))+
  geom_point(aes(size = duration_months))+
  scale_size(range = c(1, 7), name="Duration (Months)")+
  geom_smooth(method='lm', formula= y~x, show.legend = FALSE)+
  scale_color_viridis(name=stringr::str_wrap("Maximum Intensity (°C)", width = 13),
                      option ="inferno",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 12,
                                             title.position = "top",
                                             title.hjust = .5))+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x=expression("Proportion of MHW"~(km^2)), y=expression("Proportion of Suitable Fishing Habitat"~(km^2)))


############################################################
#Plot COG per mgmt zone - mean year COG by size & SST
############################################################

NWAPLL_MHW_total %>% filter(MHW == 1) %>% 
  group_by(MHW, event_index, mgmt_zone) %>% 
  summarise(COGx=mean(COGx, na.rm=TRUE),
            COGy=mean(COGy, na.rm=TRUE),
            COGx.sd=mean(COGx.sd, na.rm=TRUE),
            COGy.sd=mean(COGy.sd, na.rm=TRUE),
            X25=mean(X25, na.rm=TRUE),
            X75=mean(X75, na.rm=TRUE),
            Y25=mean(Y25, na.rm=TRUE),
            Y75=mean(Y75, na.rm=TRUE),
            prop_MHW_area=mean(prop_MHW_area, na.rm=TRUE),
            mean_SSTa= weighted.mean(mean_SSTa,n.cell_MHW, na.rm = TRUE,
                                     .groups = "drop")) %>% 
  ggplot() +
  geom_sf(data = NWA_PLL_zones, color = "goldenrod3", fill=NA)+  #bring in EEZ data
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_point(aes(COGx,COGy, fill = mean_SSTa, size = prop_MHW_area), alpha = 0.7,shape=21, color="black")+
  scale_size(range = c(1, 8), name=expression("Proportion\nof MHW"(km^2)))+
  coord_sf(xlim = c(-100, -40), ylim = c(10, 55), expand = TRUE) +
  ylab("Latitude") + xlab("Longitude") + theme_bw()+
  scale_fill_viridis(discrete=FALSE, option="inferno", 
                     name=stringr::str_wrap("SSTa\nMagnitude (°C)", width = 13))


#############################################################
#GLMM for proportion of suitable fishing habitat 
#vs proportion of MHW, max SSTa (or mean SSTa), and duration
#############################################################

##Interaction plots
library(graphics)

#MHW area and mean SSTa interaction
coplot(NWA_PLL.total.area ~ mean_SSTa|NWA_MHW.total.area,
       data = NWAPLL_MHW_total %>% filter(mgmt_zone == "NEC"),
       #overlap = 0, 
       panel = function(x, y, ...) {
         points(x, y, ...)
         abline(lm(y ~ x), col = "blue")})

coplot(NWA_PLL.total.area ~ NWA_MHW.total.area|duration_months,
       data = NWAPLL_MHW_total %>% filter(mgmt_zone == "NEC"),
       #overlap = 0, 
       panel = function(x, y, ...) {
         points(x, y, ...)
         abline(lm(y ~ x), col = "blue")})

#since we are looking at proportions we have to do a binomial distribution
#and lets scale it to make sure everything runs smooth and converges

NWAPLL_MHW_total %>% 
  ggplot()+
  stat_smooth(aes(x=prop_MHW_area,y=prop_habitat_area, color = "MHW area"),
              method='lm', formula= y~x, se= FALSE)+
  stat_smooth(aes(x=mean_SSTa,y=prop_habitat_area, color = "mean SSTa"),
              method='lm', formula= y~x, se= FALSE)+
  stat_smooth(aes(x=duration_months,y=prop_habitat_area, color = "months"),
              method='lm', formula= y~x, se= FALSE)+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x="")


library(lme4)

#need to scale the data for the monthly dataset
NWAPLL_MHW_total_scaled<-NWAPLL_MHW_total %>%
  mutate(prop_MHW_area = scale(prop_MHW_area, 
                               center = TRUE, scale = TRUE),
         NWA_MHW.total.area = scale(NWA_MHW.total.area,
                                    center=TRUE, scale = TRUE),
         mean_SSTa = scale(mean_SSTa, 
                           center = TRUE, scale = TRUE),
         max_SSTa = scale(max_SSTa, 
                          center = TRUE, scale = TRUE),
         duration_months = scale(duration_months,
                                 center = TRUE, scale = TRUE),
         sequence_months = scale(sequence_months,
                                 center = TRUE, scale = TRUE),
         event_index = as.factor(event_index))

#need to scale the data for the event dataset
NWAPLL_MHW_events_scaled<-NWAPLL_MHW_events %>%
  mutate(prop_MHW_area_event = scale(prop_MHW_area_event, 
                                     center = TRUE, scale = TRUE),
         NWA_MHW.total.area_event = scale(NWA_MHW.total.area_event, 
                                          center = TRUE, scale = TRUE),
         mean_SSTa_event = scale(mean_SSTa_event, 
                                 center = TRUE, scale = TRUE),
         max_SSTa_event = scale(max_SSTa_event, 
                                center = TRUE, scale = TRUE),
         duration_months = scale(duration_months, 
                                 center = TRUE, scale = TRUE),
         event_index = as.factor(event_index),
         mgmt_zone = as.factor(mgmt_zone)) %>% as.data.frame()

#lm
lmlist_MHWevent<-lmList(NWA_PLL.total.area_event ~ NWA_MHW.total.area_event*mean_SSTa_event*duration_months | mgmt_zone , data = NWAPLL_MHW_events_scaled)

lmlist_MHWevent

plot(lmlist_MHWevent)


#build the mixed model
MHW_glmm<-lmer(NWA_PLL.total.area_event ~ NWA_MHW.total.area_event*mean_SSTa_event*duration_months + (1|mgmt_zone), data = NWAPLL_MHW_events_scaled,na.action = na.omit)


summary(MHW_glmm)
coefficients(MHW_glmm)
plot(MHW_glmm)
confint(MHW_glmm)



#######################################
#Plot MHW and not MHW COG per mgmt zone 
#######################################
NWAPLL_MHW_total %>%
  mutate(MHW = as.factor(MHW)) %>% 
  group_by(MHW,mgmt_zone) %>% 
  summarise(COGx=mean(COGx, na.rm=TRUE),
            COGy=mean(COGy, na.rm=TRUE),
            COGx.sd=mean(COGx.sd, na.rm=TRUE),
            COGy.sd=mean(COGy.sd, na.rm=TRUE),
            X25=mean(X25, na.rm=TRUE),
            X75=mean(X75, na.rm=TRUE),
            Y25=mean(Y25, na.rm=TRUE),
            Y75=mean(Y75, na.rm=TRUE), .groups = "drop") %>% 
  arrange(mgmt_zone,MHW) %>% 
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
  coord_sf(xlim = c(-100, -40), ylim = c(10, 55), expand = TRUE) +
  ylab("Latitude") + xlab("Longitude") + theme_bw() +
  scale_color_manual(name = "Climate Regime",
                     labels = c("Near-Average", "MHW"), 
                     values = c("blue","red"))+
  ggspatial::annotation_scale()



############################
#monthly total area anomaly
############################

#loading in northern region data
MAB_NEC_NED_MHW<-here("data","water_temp","NWA","oisst",
                      "MAB_NEC_NED_MHW.rds") %>% readRDS()

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
         month_mean = mean(n.cell_habitat, na.rm=TRUE),
         month_SD = mean(n.cell_total, na.rm=TRUE),
         NWA_PLL.area.anomaly =
           ((n.cell_habitat - month_mean)/month_mean)*100) %>% 
  ungroup() %>% 
  mutate(change_direction = if_else(NWA_PLL.area.anomaly <= 0, 1,0)) %>% 
  left_join(., MHW_mgmtzone_all, by=c("mgmt_zone","date"))



NWAPLLa_MHW_total$mgmt_zone<-factor(NWAPLLa_MHW_total$mgmt_zone,
                                    levels = c("NED","NEC","MAB",
                                               "SAB","SAR","FEC",
                                               "GOM","CAR"))

NWAPLLa_MHW_total$change_direction<-factor(NWAPLLa_MHW_total$change_direction)
#NWAPLLa_MHW_total$MHW.y<-factor(NWAPLLa_MHW_total$MHW.y)

#save it
saveRDS(NWAPLLa_MHW_total, 
        here("data","Mgmt_zone","NWA_PLL","NWAPLLa_MHW_total.rds"))

#pearson correlation between the two
NWAPLLa_MHW_total<-NWAPLLa_MHW_total %>% 
  group_by(mgmt_zone) %>% 
  summarise(corr = cor(NWA_PLL.area.anomaly,detrend.x)) %>% 
  left_join(., NWAPLLa_MHW_total, by=c("mgmt_zone"))



northern_zones<-ggplot(NWAPLLa_MHW_total %>% filter(mgmt_zone %in% c("NED","NEC","MAB")), aes(x=date))+
  geom_bar(aes(y=NWA_PLL.area.anomaly, fill=change_direction),position="dodge", stat="identity")+
  geom_line(aes(y=detrend.x*20, color = MHW.y))+
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
  geom_line(aes(y=detrend.y*20, color = MHW.y))+
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
