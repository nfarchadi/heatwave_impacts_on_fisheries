#center of COG
library(here)
library(sf)
library(tidyverse)
library(viridis)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_zones<-st_transform(NWA_PLL_zones)

#combine datasets 
NWAPLL_MHW_summer<-here("data","Mgmt_zone","NWAPLL_MHW_summer.rds") %>% readRDS()
NWAPLL_MHW_winter<-here("data","Mgmt_zone","NWAPLL_MHW_winter.rds") %>% readRDS()
NWAPLL_MHW_total<-rbind(NWAPLL_MHW_summer,NWAPLL_MHW_winter) %>% ungroup()


#tiding up the data
NWAPLL_MHW_total$MHW<-factor(NWAPLL_MHW_total$MHW,
                             levels = c("Large MHW","Small MHW","Near-Average"))

NWAPLL_MHW_total<-NWAPLL_MHW_total %>% mutate(year = lubridate::year(date))

NWAPLL_MHW_total$mgmt_zone<-factor(NWAPLL_MHW_total$mgmt_zone,
                                    levels = c("NED","NEC","MAB",
                                               "SAB","SAR","FEC",
                                               "GOM","CAR"))

#########################################################################
#Finding the duration for each classification (large, small, near-average)
#########################################################################

#indexing each event by mgmt_zone
NWAPLL_MHW_total<-NWAPLL_MHW_total %>%
  dplyr::select(date,mgmt_zone,MHW) %>% 
  group_by(mgmt_zone) %>%
  mutate(consecutive_months = data.table::rleid(MHW)) %>% ungroup() %>% 
  left_join(., NWAPLL_MHW_total, by=c("date", "mgmt_zone","MHW"))

#counting the duration months for each event
NWAPLL_MHW_total<-NWAPLL_MHW_total %>%
  group_by(mgmt_zone, consecutive_months, MHW) %>% 
  summarise(duration_months = n(), .groups = "drop") %>%
  ungroup() %>% 
  left_join(., NWAPLL_MHW_total, 
            by=c("mgmt_zone","MHW", "consecutive_months")) %>% 
  group_by(mgmt_zone, consecutive_months) %>%
  mutate(sequence_months = seq_along(consecutive_months)) %>% 
  ungroup()

#taking the average metrics for each event
NWAPLL_MHW_events<-NWAPLL_MHW_total %>%
  group_by(mgmt_zone,consecutive_months,MHW) %>% 
  summarise(prop_habitat_area_event = mean(prop_habitat_area, na.rm = TRUE),
            prop_MHW_area_event = mean(prop_MHW_area, na.rm = TRUE),
            mean_SSTa_event = weighted.mean(mean_SSTa,n.cell_MHW, na.rm = TRUE),
            max_SSTa_event = max(max_SSTa, na.rm = TRUE),.groups = "drop") %>%
  left_join(., NWAPLL_MHW_total, by=c("mgmt_zone","consecutive_months","MHW")) %>% 
  dplyr::select(1:8) %>% distinct()
  
#getting rid of the Inf and NaN values from the calculations above
NWAPLL_MHW_events<-do.call(tibble,lapply(NWAPLL_MHW_events, function(x) replace(x, is.infinite(x),NA)))

NWAPLL_MHW_events<-do.call(tibble,lapply(NWAPLL_MHW_events, function(x) replace(x, is.nan(x),NA)))

NWAPLL_MHW_events$mgmt_zone<-factor(NWAPLL_MHW_events$mgmt_zone,
                                    levels = c("NED","NEC","MAB",
                                               "SAB","SAR","FEC",
                                               "GOM","CAR"))
                              
############################################################
#proportion of suitable fishing habitat vs proportion of MHW, 
#mean SSTa, and duration
############################################################


NWAPLL_MHW_events %>% 
ggplot(aes(x=prop_MHW_area_event, y=prop_habitat_area_event,
             color = mean_SSTa_event, size = duration_months))+
  geom_point()+
  scale_size(range = c(1, 10), name="Duration (Months)")+
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
             color = max_SSTa_event, size = duration_months))+
  geom_point()+
  scale_size(range = c(1, 10), name="Duration (Months)")+
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

#############################################################
#GLMM for proportion of suitable fishing habitat 
#vs proportion of MHW, max SSTa (or mean SSTa), and duration
#############################################################

##Interaction plots
library(graphics)
#MHW area and mean SSTa interaction - yes
coplot(prop_habitat_area_event ~ prop_MHW_area_event|mean_SSTa_event,
       data = NWAPLL_MHW_events,
       overlap = 0, 
       panel = function(x, y, ...) {
         points(x, y, ...)
         abline(lm(y ~ x), col = "blue")})

#MHW area and mean SSTa interaction
coplot(prop_habitat_area_event ~ prop_MHW_area_event|mean_SSTa_event,
       data = NWAPLL_MHW_events %>% filter(mgmt_zone == "SAB"),
       #overlap = 0, 
       panel = function(x, y, ...) {
         points(x, y, ...)
         abline(lm(y ~ x), col = "blue")})

NWAPLL_MHW_total %>% 
ggplot()+
  stat_smooth(aes(x=prop_MHW_area,y=prop_habitat_area, color = "MHW area"),
              method='lm', formula= y~x, se= FALSE)+
  stat_smooth(aes(x=mean_SSTa,y=prop_habitat_area, color = "mean SSTa"),
              method='lm', formula= y~x, se= FALSE)+
  stat_smooth(aes(x=sequence_months,y=prop_habitat_area, color = "months"),
              method='lm', formula= y~x, se= FALSE)+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x="")


library(lme4)

#need to scale the data
NWAPLL_MHW_total_scaled<-NWAPLL_MHW_total %>%
  mutate(prop_MHW_area = scale(prop_MHW_area, 
                                     center = TRUE, scale = TRUE),
         mean_SSTa = scale(mean_SSTa, 
                                center = TRUE, scale = TRUE),
         max_SSTa = scale(max_SSTa, 
                                center = TRUE, scale = TRUE),
         duration_months = scale(duration_months, 
                                 center = TRUE, scale = TRUE),
         sequence_months = scale(sequence_months, 
                                 center = TRUE, scale = TRUE),
         consecutive_months = as.factor(consecutive_months))

#need to scale the data
NWAPLL_MHW_events_scaled<-NWAPLL_MHW_events %>%
  mutate(prop_MHW_area_event = scale(prop_MHW_area_event, 
                               center = TRUE, scale = TRUE),
         mean_SSTa_event = scale(mean_SSTa_event, 
                           center = TRUE, scale = TRUE),
         max_SSTa_event = scale(max_SSTa_event, 
                          center = TRUE, scale = TRUE),
         duration_months = scale(duration_months, 
                                 center = TRUE, scale = TRUE),
         consecutive_months = as.factor(consecutive_months))

MHW_glmm<-lmer(prop_habitat_area_event ~ 1 + (1|mgmt_zone), data = NWAPLL_MHW_events_scaled,na.action = na.exclude)

RandomEffects <- as.data.frame(VarCorr(MHW_glmm))
RandomEffects

#intra-class correlation (ICC) as the ratio of the random intercept variance (between-mgmt zone) to the total variance, defined as the sum of the random intercept variance and residual variance (between + within)
ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between #good proportion of the total variance is explained by between mgmt zone variance and less is within mgmt zone

MHW_glmm<-lmer(prop_habitat_area_event ~ prop_MHW_area_event * mean_SSTa_event + duration_months + (1+mean_SSTa_event|mgmt_zone), data = NWAPLL_MHW_events_scaled,na.action = na.exclude)

summary(MHW_glmm)
coefficients(MHW_glmm)
plot(MHW_glmm)
confint(MHW_glmm)


MHW_glmm_randomslopes<-lmer(prop_habitat_area ~ prop_MHW_area*mean_SSTa + sequence_months + (1+prop_MHW_area*mean_SSTa|mgmt_zone), data = NWAPLL_MHW_total_scaled)

summary(MHW_glmm_randomslopes)
coefficients(MHW_glmm_randomslopes)
plot(MHW_glmm_randomslopes)

MHW_lm<-lm(prop_habitat_area ~ prop_MHW_area * mean_SSTa * sequence_months, data = NWAPLL_MHW_total_scaled)
summary(MHW_lm)
plot(MHW_lm)
###########################################################
#Identify prominent MWH class >=.3 proportion & >=3 months
###########################################################
prominent_mhw<-NWAPLL_MHW_total %>% 
  filter(MHW=="Large MHW" & duration_months >=3)

nearavg_mhw<-NWAPLL_MHW_total %>% 
  filter(MHW=="Near-Average")

prominent_nearavg_mhw<-rbind(prominent_mhw,nearavg_mhw)

############################################################
#Plot near avg and prominent COG per mgmt zone 
############################################################
prominent_nearavg_mhw %>%  
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
                     labels = c("Prominent MHW", "Near-Average"), 
                     values = c("red","blue"))+
  ggspatial::annotation_scale()


###############################################################
#Plot yearly habitat change during prominent MHWs per mgmt zone
###############################################################
prominent_nearavg_mhw<-prominent_nearavg_mhw %>% mutate(year = lubridate::year(date))

NWAPLL_year_mgmtzone_prominenthabitat.area<-prominent_nearavg_mhw %>%
  filter(MHW == "Large MHW") %>%
  group_by(mgmt_zone, year, MHW) %>% 
  summarise(prominent.yearly.habitat.area.mean = mean(NWA_PLL.total.area, na.rm = TRUE), 
            .groups = "drop") 


NWAPLL_total_mgmtzone_nearavghabitat.area<-prominent_nearavg_mhw %>% 
  filter(MHW=="Near-Average") %>% 
  group_by(mgmt_zone) %>% 
  summarise(nearavg.total.habitat.area.mean = mean(NWA_PLL.total.area, na.rm = TRUE), 
            .groups = "drop")


habitat_change<-NWAPLL_year_mgmtzone_prominenthabitat.area %>% 
  left_join(., NWAPLL_total_mgmtzone_nearavghabitat.area, by=c("mgmt_zone")) %>%
  mutate(habitat_change = 
           (prominent.yearly.habitat.area.mean - nearavg.total.habitat.area.mean)/nearavg.total.habitat.area.mean,
         change_direction = if_else(habitat_change <= 0, 1,0))

habitat_change$mgmt_zone<-factor(habitat_change$mgmt_zone,
                                 levels = c("CAR","GOM","FEC",
                                            "SAR","SAB","MAB",
                                            "NEC","NED"))

habitat_change$change_direction<-factor(habitat_change$change_direction)

ggplot(habitat_change, aes(x=mgmt_zone,y=habitat_change, fill = change_direction))+
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = c("blue","red"))+
  facet_grid(~year)+
  theme_bw()+
  ggtitle("Habitat change: Prominent Class MHW (>=.3 proportion & >=3 months) yearly mean - Near-Average mean / Near-Average mean") +
  labs(y="Percentage change", x="Date") +theme(legend.position="")+
  geom_vline(xintercept = 4.5, color = "black")+
  coord_flip()



##############################################################################
#Plot yearly habitat change per mgmt zone (pattern = years with prominent MHW)
##############################################################################
NWAPLL_MHW_total<-NWAPLL_MHW_total %>% mutate(year = lubridate::year(date))

NWAPLL_year_mgmtzone_habitat.area<-NWAPLL_MHW_total %>%
  mutate(prominent_mhw = if_else(MHW == "Large MHW" & duration_months>=3,1,0)) %>% 
  group_by(mgmt_zone, year) %>% 
  summarise(yearly.habitat.area.mean = mean(NWA_PLL.total.area, na.rm = TRUE),
            prominent_mhw = mean(prominent_mhw,na.rm=TRUE),
            .groups = "drop") %>% 
  mutate(prominent_mhw = if_else(prominent_mhw > 0,1,0),
         prominent_mhw = as.factor(prominent_mhw))


NWAPLL_total_mgmtzone_habitat.area<-NWAPLL_MHW_total %>% 
  group_by(mgmt_zone) %>% 
  summarise(total.habitat.area.mean = mean(NWA_PLL.total.area, na.rm = TRUE),
            .groups = "drop")


habitat_change<-NWAPLL_year_mgmtzone_habitat.area %>% 
  left_join(., NWAPLL_total_mgmtzone_habitat.area, by=c("mgmt_zone")) %>% 
  mutate(habitat_change = 
           ((yearly.habitat.area.mean - total.habitat.area.mean)/total.habitat.area.mean)*100,
         change_direction = if_else(habitat_change <= 0, 1,0))

habitat_change$mgmt_zone<-factor(habitat_change$mgmt_zone,
                                 levels = c("CAR","GOM","FEC",
                                            "SAR","SAB","MAB",
                                            "NEC","NED"))

habitat_change$change_direction<-factor(habitat_change$change_direction)

ggplot(habitat_change, aes(x=mgmt_zone,y=habitat_change, fill = change_direction,
                           pattern=prominent_mhw))+
  geom_bar_pattern(position="dodge", stat="identity",
           color = "black", 
           pattern_fill = "black",
           pattern_angle = 45,
           pattern_density = 0.09,
           pattern_spacing = 0.1,
           pattern_key_scale_factor = 1)+
  scale_fill_manual(values = c("blue","red"))+
  scale_pattern_manual(values = c("1" = "stripe", "0" = "none"))+
  facet_grid(~year)+
  theme_bw()+
  ggtitle("Habitat change: yearly mean - overall mean / overall mean (pattern = years with prominent MHW)") +
  labs(y="Percentage change", x="Date") +theme(legend.position="")+
  geom_vline(xintercept = 5.5, color = "black")+
  coord_flip()




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
  geom_line(aes(y=detrend*30, color = MHW.y))+
  scale_y_continuous(name = "Habitat Change (%)",
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(trans=~.*.03, 
                                         name= expression("SSTa"~(degree~C))))+
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
  geom_line(aes(y=detrend*30, color = MHW.y))+
  scale_y_continuous(name = "Habitat Change (%)",
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(trans=~.*.03, 
                                         name=expression("SSTa"~(degree~C))))+
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



















































