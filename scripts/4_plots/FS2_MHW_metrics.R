# Fig. S2. Marine heatwave (MHW) properties (intensity, size, duration) for each management area during fleet specific fishing seasons.

# and ANOVA analysis among mgmt areas

############
#MHW metrics
############

#PLL
NWAPLL_CFG_MHW_total<-here("data","mgmt_area_metrics","NWA_PLL",
                       "NWAPLL_CFG_MHW_total.rds") %>% 
  readRDS() %>% dplyr::select(mgmt_area,MHW,date,prop_MHW_cells,mean_SSTa,prop_habitat_cells, duration_months,sequence_months, event_index)

#TROLL
NEPTROLL_CFG_MHW_total<-here("data","mgmt_area_metrics","NEP_TROLL",
                         "NEPTROLL_CFG_MHW_total.rds") %>% 
  readRDS() %>% filter(mgmt_area != "CP") %>% 
  dplyr::select(mgmt_area,MHW,date,prop_MHW_cells,mean_SSTa,prop_habitat_cells, duration_months,sequence_months, event_index)

MHW_total<-rbind(NWAPLL_CFG_MHW_total,NEPTROLL_CFG_MHW_total) %>% 
  mutate(mgmt_area = factor(mgmt_area, levels = c("VN","CL","EK","MT",
                                                  "NED","NEC","MAB","SAB",
                                                  "SAR","FEC","GOM","CAR")))


a<-MHW_total %>% 
  mutate(Fleet = case_when(mgmt_area == "VN" ~ "Troll",
                           mgmt_area == "CL" ~ "Troll",
                           mgmt_area == "EK" ~ "Troll",
                           mgmt_area == "MT" ~ "Troll",
                           TRUE ~ "Longline")) %>%
  ggplot(aes(x = mgmt_area, y=mean_SSTa, color = Fleet))+
  geom_boxplot()+
  scale_color_manual(values=c("orange","lightseagreen")) +
  stat_summary(fun=mean, geom="point", shape=8, size=1.5) +
  theme_bw() +
  labs(x = "Management Area",y = "MHW Intensity")

b<-MHW_total %>% 
  mutate(Fleet = case_when(mgmt_area == "VN" ~ "Troll",
                           mgmt_area == "CL" ~ "Troll",
                           mgmt_area == "EK" ~ "Troll",
                           mgmt_area == "MT" ~ "Troll",
                           TRUE ~ "Longline")) %>%
  ggplot(aes(x = mgmt_area, y=prop_MHW_cells, color = Fleet))+
  geom_boxplot()+
  scale_color_manual(values=c("orange","lightseagreen")) +
  stat_summary(fun=mean, geom="point", shape=8, size=1.5) +
  theme_bw() +
  labs(x = "Management Area",y = "MHW Size")

c<-MHW_total %>% filter(MHW == 1) %>%
  mutate(Fleet = case_when(mgmt_area == "VN" ~ "Troll",
                           mgmt_area == "CL" ~ "Troll",
                           mgmt_area == "EK" ~ "Troll",
                           mgmt_area == "MT" ~ "Troll",
                           TRUE ~ "Longline")) %>%
  dplyr::select(mgmt_area,event_index,sequence_months, Fleet) %>%  
  ggplot(aes(x = mgmt_area, y=sequence_months, color = Fleet))+
  geom_boxplot()+
  scale_color_manual(values=c("orange","lightseagreen")) +
  stat_summary(fun=mean, geom="point", shape=8, size=1.5) +
  theme_bw() +
  labs(x = "Management Area",y = "MHW Duration")


supp_MHW_metrics<-cowplot::plot_grid(a,b,c, align = "v",labels = c('(A)','(B)','(C)'), label_size = 10,nrow=3)


ggsave(here("Plots","FS2_MHW_metrics.png"),
       width = 7, height = 6, units = "in", dpi = 300)
ggsave(here("Plots","FS2_MHW_metrics.svg"),
       width = 7, height = 6, units = "in", dpi = 300)


######################################################################
#ANOVA analysis of how did MHW properties compare among NWA mgmt areas
######################################################################


#intensity ANOVA
intensity_aov<-MHW_total %>%
  filter(mgmt_area %in% c("NED","NEC","MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>% 
  filter(prop_MHW_cells > 0) %>% 
  dplyr::select(mgmt_area,mean_SSTa) %>%  
  aov(mean_SSTa ~ mgmt_area, data = .)

summary(intensity_aov)
TukeyHSD(intensity_aov, conf.level=.95)

#size ANOVA
size_aov<-MHW_total %>% 
  filter(mgmt_area %in% c("NED","NEC","MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>% 
  dplyr::select(mgmt_area,prop_MHW_cells) %>% 
  aov(prop_MHW_cells ~ mgmt_area, data = .)

summary(size_aov)
TukeyHSD(size_aov, conf.level=.95)

#duration ANOVA
duration_aov<-MHW_total %>% filter(MHW == 1) %>%
  filter(mgmt_area %in% c("NED","NEC","MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>% 
  dplyr::select(mgmt_area,sequence_months) %>% 
  aov(sequence_months ~ mgmt_area, data = .)

summary(duration_aov)
TukeyHSD(duration_aov, conf.level=.95)


######################################################################
#ANOVA analysis of how did MHW properties compare among NWA mgmt areas
######################################################################

#intensity ANOVA
intensity_aov<-MHW_total %>% 
  filter(mgmt_area %in% c("VN","EK",
                          "CL","MT")) %>% 
  dplyr::select(mgmt_area,mean_SSTa) %>% 
  aov(mean_SSTa ~ mgmt_area, data = .)

summary(intensity_aov)
TukeyHSD(intensity_aov, conf.level=.95)


#size ANOVA
size_aov<-MHW_total %>% 
  filter(mgmt_area %in% c("VN","EK",
                          "CL","MT")) %>% 
  dplyr::select(mgmt_area,prop_MHW_cells) %>% 
  aov(prop_MHW_cells ~ mgmt_area, data = .)

summary(size_aov)
TukeyHSD(size_aov, conf.level=.95)


#duration ANOVA
duration_aov<-MHW_total %>% 
  filter(mgmt_area %in% c("VN","EK",
                          "CL","MT")) %>% 
  dplyr::select(mgmt_area,sequence_months) %>% 
  aov(sequence_months ~ mgmt_area, data = .)

summary(duration_aov)
TukeyHSD(duration_aov, conf.level=.95)