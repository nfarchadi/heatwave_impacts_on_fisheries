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
NEP_TROLL_zones<-here("data","shapefiles","NEP_TROLL","Zones_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NEP_TROLL_zones<-st_transform(NEP_TROLL_zones)

#######################################################
#IDing MHW months by spatially averaging w/in mgmt zone
#######################################################


Conception_Monterey_Eureka_Columbia_Vancouver_MHW<-here("data","water_temp",
                                                        "NEP","oisst","Conception_Monterey_Eureka_Columbia_Vancouver_MHW.rds") %>% readRDS()


#averaging over the pixels per mgmt zone and yearmon
Conception_Monterey_Eureka_Columbia_Vancouver_MHW<-Conception_Monterey_Eureka_Columbia_Vancouver_MHW %>% 
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


#rename
MHW_mgmtzone_all<-Conception_Monterey_Eureka_Columbia_Vancouver_MHW %>% 
  rename("mgmt_zone"="ET_ID",
         "date"="yearmon") %>% 
  mutate(mgmt_zone = case_when(mgmt_zone == "Vancouver" ~ "VN",
                               mgmt_zone == "Columbia" ~ "CL",
                               mgmt_zone == "Eureka" ~ "EK",
                               mgmt_zone == "Monterey" ~ "MT",
                               mgmt_zone == "Conception" ~ "CP"))
  

MHW_mgmtzone_all$mgmt_zone<-factor(MHW_mgmtzone_all$mgmt_zone,
                                   levels = c("VN","CL",
                                              "EK","MT","CP"))

rm(Conception_Monterey_Eureka_Columbia_Vancouver_MHW)

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
  labs(x = "", y=expression("SST Anomaly "(degree~C))) + 
  zoo::scale_x_yearmon(limits=c(zoo::as.yearmon("2012-01-01"),zoo::as.yearmon("2020-12-31")))+
  facet_wrap(~mgmt_zone)

#load TROLL landscape metric dataset
NEPTROLL_MHW_total<-here("data","Mgmt_zone","NEP_TROLL","NEPTROLL_MHW.rds") %>% readRDS()%>% ungroup()


#tiding up the data
NEPTROLL_MHW_total$MHW<-factor(NEPTROLL_MHW_total$MHW,
                             levels = c("Large MHW","Small MHW","Near-Average"))

NEPTROLL_MHW_total<-NEPTROLL_MHW_total %>% mutate(year = lubridate::year(date))

NEPTROLL_MHW_total$mgmt_zone<-factor(NEPTROLL_MHW_total$mgmt_zone,
                                   levels = c("VN","CL",
                                              "EK","MT","CP"))


#joining the two
NEPTROLL_MHW_total<-NEPTROLL_MHW_total %>% 
  left_join(., MHW_mgmtzone_all, by=c("mgmt_zone","date")) %>% 
  rename("size_category"="MHW.x","MHW"="MHW.y")



########################################
#Finding the event duration for each MHW
########################################

#indexing each event by mgmt_zone
NEPTROLL_MHW_total<-NEPTROLL_MHW_total %>%
  dplyr::select(date,mgmt_zone,MHW) %>%
  group_by(mgmt_zone) %>%
  mutate(event_index = data.table::rleid(MHW)) %>% ungroup() %>% 
  left_join(., NEPTROLL_MHW_total, by=c("date", "mgmt_zone","MHW"))

#counting the duration months and the sequence for each event
NEPTROLL_MHW_total<-NEPTROLL_MHW_total %>%
  group_by(mgmt_zone, event_index, MHW) %>% 
  summarise(duration_months = n(), .groups = "drop") %>%
  ungroup() %>% 
  left_join(., NEPTROLL_MHW_total, 
            by=c("mgmt_zone","MHW", "event_index")) %>% 
  group_by(mgmt_zone, event_index) %>%
  mutate(sequence_months = seq_along(event_index)) %>% 
  ungroup() 
# %>%
#   mutate(sequence_months = ifelse(MHW == 0,NA,sequence_months))

#taking the average metrics for each event
NEPTROLL_MHW_events<-NEPTROLL_MHW_total %>%
  group_by(mgmt_zone,event_index,MHW, duration_months) %>% 
  summarise(prop_habitat_area_event = mean(prop_habitat_area, na.rm = TRUE),
            prop_habitat_cells_event = mean(prop_habitat_cells, na.rm=TRUE),
            prop_MHW_area_event = mean(prop_MHW_area, na.rm = TRUE),
            prop_MHW_cells_event = mean(prop_MHW_cells, na.rm=TRUE),
            NEP_TROLL.total.area_event = mean(NEP_TROLL.total.area, na.rm=TRUE),
            NEP_MHW.total.area_event = mean(NEP_MHW.total.area, na.rm=TRUE),
            mean_SSTa_event = weighted.mean(mean_SSTa,n.cell_MHW, na.rm = TRUE),
            max_SSTa_event = max(max_SSTa, na.rm = TRUE),.groups = "drop") %>%
  filter(MHW == 1) #only want MHW events 

NEPTROLL_MHW_events$mgmt_zone<-factor(NEPTROLL_MHW_events$mgmt_zone,
                                    levels = c("VN","CL",
                                               "EK","MT","CP"))

#now I have two datasets 
#1) NEPTROLL_MHW_total - monthly data for each mgmt zone

saveRDS(NEPTROLL_MHW_total, here("data","Mgmt_zone","NEP_TROLL","NEPTROLL_MHW_total.rds"))

#2) NEPTROLL_MHW_events - summarized metrics for each MHW 

saveRDS(NEPTROLL_MHW_events, here("data","Mgmt_zone","NEP_TROLL","NEPTROLL_MHW_events.rds"))

############################################################
#proportion of suitable fishing habitat vs proportion of MHW, 
#mean SSTa, and duration
############################################################


NEPTROLL_MHW_events %>%  
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

NEPTROLL_MHW_total %>%  
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

NEPTROLL_MHW_events %>% 
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

NEPTROLL_MHW_total %>% filter(MHW == 1) %>%
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
            mean_SSTa= weighted.mean(mean_SSTa,n.cell_MHW, na.rm = TRUE),
            .groups = "drop") %>% 
  ggplot() +
  geom_sf(data = NEP_TROLL_zones, color = "goldenrod3", fill=NA)+  #bring in EEZ data
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_point(aes(COGx,COGy, fill = mean_SSTa, size = prop_MHW_area), alpha = 0.7,shape=21, color="black")+
  scale_size(range = c(1, 8), name=expression("Proportion\nof MHW"(km^2)))+
  coord_sf(xlim = c(-135, -117), ylim = c(32, 54), expand = TRUE) +
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
coplot(NEP_TROLL.total.area ~ mean_SSTa|NEP_MHW.total.area,
       data = NEPTROLL_MHW_total %>% filter(mgmt_zone == "NEC"),
       #overlap = 0, 
       panel = function(x, y, ...) {
         points(x, y, ...)
         abline(lm(y ~ x), col = "blue")})

coplot(NEP_TROLL.total.area ~ NEP_MHW.total.area|duration_months,
       data = NEPTROLL_MHW_total %>% filter(mgmt_zone == "NEC"),
       #overlap = 0, 
       panel = function(x, y, ...) {
         points(x, y, ...)
         abline(lm(y ~ x), col = "blue")})

#since we are looking at proportions we have to do a binomial distribution
#and lets scale it to make sure everything runs smooth and converges

NEPTROLL_MHW_total %>% 
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
NEPTROLL_MHW_total_scaled<-NEPTROLL_MHW_total %>%
  mutate(prop_MHW_area = scale(prop_MHW_area, 
                               center = TRUE, scale = TRUE),
         NEP_MHW.total.area = scale(NEP_MHW.total.area,
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
NEPTROLL_MHW_events_scaled<-NEPTROLL_MHW_events %>%
  mutate(prop_MHW_area_event = scale(prop_MHW_area_event, 
                                     center = TRUE, scale = TRUE),
         NEP_MHW.total.area_event = scale(NEP_MHW.total.area_event, 
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
lmlist_MHWevent<-lmList(NEP_TROLL.total.area_event ~ NEP_MHW.total.area_event*mean_SSTa_event*duration_months | mgmt_zone , data = NEPTROLL_MHW_events_scaled)

lmlist_MHWevent

plot(lmlist_MHWevent)


#build the mixed model
MHW_glmm<-lmer(NEP_TROLL.total.area_event ~ NEP_MHW.total.area_event*mean_SSTa_event*duration_months + (1|mgmt_zone), data = NEPTROLL_MHW_events_scaled,na.action = na.omit)


summary(MHW_glmm)
coefficients(MHW_glmm)
plot(MHW_glmm)
confint(MHW_glmm)



#######################################
#Plot MHW and not MHW COG per mgmt zone 
#######################################
NEPTROLL_MHW_total %>%
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
  geom_sf(data = NEP_TROLL_zones, color = "goldenrod3", fill=NA)+  #bring in EEZ data
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
  coord_sf(xlim = c(-135, -117), ylim = c(32, 54), expand = TRUE) +
  ylab("Latitude") + xlab("Longitude") + theme_bw() +
  scale_color_manual(name = "Climate Regime",
                     labels = c("Near-Average", "MHW"), 
                     values = c("blue","red"))+
  ggspatial::annotation_scale()



############################
#monthly total area anomaly
############################

#loading in northern region data
Conception_Monterey_Eureka_Columbia_Vancouver_MHW<-here("data","water_temp","NEP"
                                                        ,"oisst", "Conception_Monterey_Eureka_Columbia_Vancouver_MHW.rds") %>% readRDS()


#averaging over the pixels per mgmt zone and yearmon
Conception_Monterey_Eureka_Columbia_Vancouver_MHW<-Conception_Monterey_Eureka_Columbia_Vancouver_MHW %>% 
  group_by(yearmon, ET_ID) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  mutate(MHW = ifelse(detrend >= seas, 1, 0))



#rename
MHW_mgmtzone_all<-Conception_Monterey_Eureka_Columbia_Vancouver_MHW %>% 
  rename("mgmt_zone"="ET_ID",
         "date"="yearmon")


#calculate monthly anomaly
NEPTROLLa_MHW_total<-NEPTROLL_MHW_total %>% 
  group_by(mgmt_zone,month) %>% 
  mutate(year = lubridate::year(date),
         month_mean = mean(n.cell_habitat, na.rm=TRUE),
         month_SD = mean(n.cell_total, na.rm=TRUE),
         NEP_TROLL.area.anomaly =
           ((n.cell_habitat - month_mean)/month_mean)*100) %>% 
  ungroup() %>% 
  mutate(change_direction = if_else(NEP_TROLL.area.anomaly <= 0, 1,0)) %>% 
  left_join(., MHW_mgmtzone_all, by=c("mgmt_zone","date","detrend","seas","MHW"))



NEPTROLLa_MHW_total$mgmt_zone<-factor(NEPTROLLa_MHW_total$mgmt_zone,
                                    levels = c("VN","CL",
                                               "EK","MT","CP"))

NEPTROLLa_MHW_total$change_direction<-factor(NEPTROLLa_MHW_total$change_direction)

#save it
saveRDS(NEPTROLLa_MHW_total, here("data","Mgmt_zone","NEP_TROLL","NEPTROLLa_MHW_total.rds"))

#pearson correlation between the two
NEPTROLLa_MHW_total<-NEPTROLLa_MHW_total %>% 
  group_by(mgmt_zone) %>% 
  summarise(corr = cor(NEP_TROLL.area.anomaly,detrend.y),.groups = "drop") %>% 
  left_join(., NEPTROLLa_MHW_total, by=c("mgmt_zone"))


##habitat change and SSTa plot
ggplot(NEPTROLLa_MHW_total, aes(x=date))+
  geom_bar(aes(y=NEP_TROLL.area.anomaly, fill=change_direction),position="dodge", stat="identity")+
  geom_line(aes(y=detrend.y*20, color = MHW.y))+
  scale_y_continuous(name = "Habitat Change (%)",
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(trans=~.*.02, 
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



##habitat change and MHW plot
MHW_dates<-NEPTROLLa_MHW_total %>% filter(MHW.x==1) %>%  
  dplyr::select(mgmt_zone,event_index,duration_months,date) %>% 
  rename("start_date"="date") %>% 
  group_by(mgmt_zone,event_index) %>% 
  filter(start_date == min(start_date)) %>% ungroup %>% 
  mutate(end_date= start_date + as.numeric(paste0(".",duration_months)))

NEPTROLLa_MHW_total %>% 
ggplot()+
  geom_bar(aes(y=NEP_TROLL.area.anomaly,x=date, fill=change_direction),position="dodge", stat="identity")+
  geom_rect(data = MHW_dates,aes(xmin = start_date, xmax=end_date, ymin=-Inf,
                                 ymax=Inf),fill="red", color = "black",alpha = 0.25)+
  scale_y_continuous(name = "Habitat Change (%)")+
  facet_wrap(~mgmt_zone, scales = "free", ncol = 1)+
  scale_fill_manual(values = c("blue","red"))+
  scale_colour_gradient(low = "black", high = "red")+
  geom_label(aes(label=paste("r =",round(corr,2))), 
             x = Inf, y = Inf, hjust=1, vjust=1)+
  theme_bw()+
  ggtitle("") +
  labs(x="Date") +theme(legend.position="",
                        strip.background = element_blank())
