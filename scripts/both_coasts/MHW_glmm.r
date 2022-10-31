library(sf)
library(purrr)
library(lme4)
library(tidyverse)
library(plotly)
library(patchwork)
library(here)
sf_use_s2(FALSE)


####################################
#exploratory plots before doing glmm
####################################

#PLL
NWAPLLa_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLLa_MHW_total.rds") %>%
  readRDS()

size_plot<-NWAPLLa_MHW_total %>%  
  na.omit() %>% 
  mutate(month = lubridate::month(date)) %>% group_by(mgmt_zone) %>% 
  mutate(slope = lm(NWA_PLL.area.anomaly ~ prop_MHW_cells)$coefficients[2]) %>%
  ggplot(aes(x=prop_MHW_cells, y=NWA_PLL.area.anomaly))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x, show.legend = FALSE)+
  facet_wrap(~mgmt_zone, scales = "free", nrow = 1)+
  geom_label(aes(label=paste("Slope =",round(slope,2))), color = "black", 
            x = Inf, y = Inf, hjust=1, vjust=1)+
  theme_minimal()+
  labs(x=expression("MHW Size"), y=expression("% Change in Available Fishing Grounds"))

intensity_plot<-NWAPLLa_MHW_total %>%  
  na.omit() %>% 
  mutate(month = lubridate::month(date)) %>% group_by(mgmt_zone) %>% 
  mutate(slope = lm(NWA_PLL.area.anomaly ~ mean_SSTa)$coefficients[2]) %>%
  ggplot(aes(x=mean_SSTa, y=NWA_PLL.area.anomaly))+
  scale_size(range = c(1, 7), name="Duration (Months)")+
  geom_point()+
  geom_smooth(method='lm', formula= y~x, show.legend = FALSE)+
  facet_wrap(~mgmt_zone, scales = "free", nrow = 1)+
  geom_label(aes(label=paste("Slope =",round(slope,2))), color = "black", 
             x = Inf, y = Inf, hjust=1, vjust=1)+
  theme_minimal()+
  labs(x=expression("MHW Intensity (°C)"), y=expression("% Change in Available Fishing Grounds"))
  
size_plot / intensity_plot

#TROLL
NEPTROLLa_MHW_total<-here("data","Mgmt_zone","NEP_TROLL","NEPTROLLa_MHW_total.rds") %>% readRDS()

size_plot<-NEPTROLLa_MHW_total %>%  
  na.omit() %>% 
  mutate(month = lubridate::month(date)) %>% group_by(mgmt_zone) %>% 
  mutate(slope = lm(NEP_TROLL.area.anomaly ~ prop_MHW_cells)$coefficients[2]) %>%
  ggplot(aes(x=prop_MHW_cells, y=NEP_TROLL.area.anomaly))+
  scale_size(range = c(1, 7), name="Duration (Months)")+
  geom_point()+
  geom_smooth(method='lm', formula= y~x, show.legend = FALSE)+
  facet_wrap(~mgmt_zone, scales = "free", nrow = 1)+
  geom_label(aes(label=paste("Slope =",round(slope,2))), color = "black", 
             x = Inf, y = Inf, hjust=1, vjust=1)+
  theme_minimal()+
  labs(x=expression("MHW Size"), y=expression("% Change in Available Fishing Grounds"))

intensity_plot<-NEPTROLLa_MHW_total %>%  
  na.omit() %>% 
  mutate(month = lubridate::month(date)) %>% group_by(mgmt_zone) %>% 
  mutate(slope = lm(NEP_TROLL.area.anomaly ~ mean_SSTa)$coefficients[2]) %>%
  ggplot(aes(x=mean_SSTa, y=NEP_TROLL.area.anomaly))+
  scale_size(range = c(1, 7), name="Duration (Months)")+
  geom_point()+
  geom_smooth(method='lm', formula= y~x, show.legend = FALSE)+
  facet_wrap(~mgmt_zone, scales = "free", nrow=1)+
  geom_label(aes(label=paste("Slope =",round(slope,2))), color = "black", 
             x = Inf, y = Inf, hjust=1, vjust=1)+
  theme_minimal()+
  labs(x=expression("MHW Intensity (°C)"), y=expression("% Change in Available Fishing Grounds"))

size_plot / intensity_plot

################################################################################
#glmm - habitat size ~ MHW size + intensity
################################################################################

########
#PLL
########
NWAPLLa_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLLa_MHW_total.rds") %>%
  readRDS()

# #lets normalize/scale the data so we can better interpret the reg coeifficents
NWAPLLa_MHW_total_scaled<-NWAPLLa_MHW_total %>%
  na.omit() %>%
  mutate(prop_MHW_area = scale(prop_MHW_area), #center and scales by default
         mean_SSTa = scale(mean_SSTa),
         n.cell_MHW = scale(n.cell_MHW),
         mean_SSTa = as.numeric(mean_SSTa)) %>% 
  as.data.frame()


#build the mixed model
# MHW_glmm<-glmer(prop_habitat_cells ~ prop_MHW_cells + mean_SSTa + (1+prop_MHW_cells + mean_SSTa|mgmt_zone), data = NWAPLL_MHW_total_scaled, family = binomial, 
#                 weights = n.cell_total, 
#                 na.action = na.omit)

# MHW_glmm<-lmer(NWA_PLL.area.anomaly ~ prop_MHW_cells + mean_SSTa + (1+prop_MHW_cells|mgmt_zone), data = NWAPLLa_MHW_total_scaled)

MHW_glmm<-lmList(NWA_PLL.area.anomaly ~ prop_MHW_cells + mean_SSTa|mgmt_zone , data = NWAPLLa_MHW_total_scaled, na.action = na.omit)
 
summary(MHW_glmm)
coefficients(MHW_glmm)
NWA_coef<-coefficients(MHW_glmm)
plot(MHW_glmm)
confint(MHW_glmm, oldNames = FALSE)
fixef(MHW_glmm)
ranef(MHW_glmm)

mgmt_zones<-unique(NWAPLLa_MHW_total$mgmt_zone) %>% as.vector()

#need to make predictions at different combinations for mean_SSTa and prop_MHW_area
mgmt_zone_variables_PLL<-list()

for(i in 1:length(mgmt_zones)){
  
  SSTa_seq<-seq(min(NWAPLLa_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(mean_SSTa), na.rm=TRUE),max(NWAPLLa_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(mean_SSTa), na.rm=TRUE),length = 50)
  
  area_seq<-seq(min(NWAPLLa_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(prop_MHW_cells), na.rm=TRUE),max(NWAPLLa_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(prop_MHW_cells), na.rm=TRUE),length = 50)
  
  mz_vars <- crossing(SSTa_seq,area_seq,mgmt_zones[i]) %>% 
    rename("mean_SSTa"="SSTa_seq",
           "prop_MHW_cells"="area_seq",
           "mgmt_zone"="mgmt_zones[i]")
  
  # mgmt_zone_seq<-rep(mgmt_zones[i],length = length(area_seq))
  # 
  # mz_vars<-data.frame(mean_SSTa = SSTa_seq, prop_MHW_area = area_seq, 
  #                    mgmt_zone = mgmt_zone_seq)
  # 
  mgmt_zone_variables_PLL[[length(mgmt_zone_variables_PLL)+1]]<-mz_vars
}

mgmt_zone_variables_PLL<-bind_rows(mgmt_zone_variables_PLL)

mgmt_zone_variables_PLL$predict_habitat_area<-predict(MHW_glmm, newdata=mgmt_zone_variables_PLL, type = "response")

mgmt_zone_variables_PLL_split<-split(mgmt_zone_variables_PLL, 
                                 f = mgmt_zone_variables_PLL$mgmt_zone)

Lp1<-mgmt_zone_variables_PLL_split$NED %>%
  ggplot() +
  geom_tile(aes(x = prop_MHW_cells, y = mean_SSTa, 
                fill = predict_habitat_area)) +
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_bw() +
  # cmocean::scale_fill_cmocean(name="delta",
  #                             limits = c(-20,20),
  #                             guide = guide_colorbar(title.position = "right"))+
  # scale_fill_gradient2(low = "#400505FF",
  #                      mid = "#9A9A99FF",
  #                      high = "#DDAF19FF",
  #                      midpoint = 0,
  #                      guide = guide_colorbar(title.position = "right"))+
  scale_fill_gradientn(colours = jet.colors(100), 
                       limits = c(-50,50),
                       oob = scales::squish,
                       breaks = bzz,
                       guide = guide_colorbar(title.position = "right"))+
  labs(x = expression("MHW Size"), y = "MHW Intensity",
      fill = expression("% Change in Available Fishing Grounds")) +
  theme(legend.spacing.y = unit(0.2, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(5,5,-7,-7),
        legend.title = element_text(angle = 90, size = 7),
        legend.title.align = 0.5,
        strip.background = element_blank())

Lp2<- Lp1 %+% mgmt_zone_variables_PLL_split$NEC 
Lp3<- Lp1 %+% mgmt_zone_variables_PLL_split$MAB
Lp4<- Lp1 %+% mgmt_zone_variables_PLL_split$SAB
Lp5<- Lp1 %+% mgmt_zone_variables_PLL_split$SAR
Lp6<- Lp1 %+% mgmt_zone_variables_PLL_split$FEC
Lp7<- Lp1 %+% mgmt_zone_variables_PLL_split$GOM
Lp8<- Lp1 %+% mgmt_zone_variables_PLL_split$CAR


(Lp1 + Lp2 + Lp3 + Lp4 + Lp5 + Lp6 + Lp7 + Lp8)

width <- max(mgmt_zone_variables_PLL$prop_MHW_cells)-min(mgmt_zone_variables_PLL$prop_MHW_cells)/length(mgmt_zone_variables_PLL$prop_MHW_cells)

height <- max(mgmt_zone_variables_PLL$mean_SSTa)-min(mgmt_zone_variables_PLL$mean_SSTa)/length(mgmt_zone_variables_PLL$mean_SSTa)

PLL_plots<-mgmt_zone_variables_PLL %>% 
  mutate(mgmt_zone = factor(mgmt_zone,levels = c("NED","NEC","MAB",
                                                 "SAB","SAR","FEC",
                                                 "GOM", "CAR"))) %>% 
  #filter(mgmt_zone %in% c("NED","MAB","FEC","CAR")) %>% 
  ggplot() +
  geom_raster(aes(x = prop_MHW_cells, y = mean_SSTa, 
                fill = predict_habitat_area)) +
  facet_wrap(~mgmt_zone, scales = "free", nrow = 4)+
  theme_bw() +
  # cmocean::scale_fill_cmocean(name="delta",
  #                             limits = c(-20,20),
  #                             guide = guide_colorbar(title.position = "right"))+
  # scale_fill_gradient2(low = "#400505FF",
  #                      mid = "#9A9A99FF",
  #                      high = "#DDAF19FF",
  #                      midpoint = 0,
  #                      guide = guide_colorbar(title.position = "right"))+
  scale_fill_gradientn(colours = jet.colors(100), 
                       limits = c(-50,50),
                       oob = scales::squish,
                       breaks = bzz,
                       guide = guide_colorbar(title.position = "right"))+
  labs(x = expression("MHW Size"), y = "MHW Intensity",
       fill = expression("% Change in Fishing Grounds")) +
  theme(legend.spacing.y = unit(0.2, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(5,5,-7,-7),
        legend.title = element_text(angle = 90, size = 7),
        legend.title.align = 0.5,
        strip.background = element_blank())
PLL_plots

#testing assumptions
m_PLL<-lm(NWA_PLL.area.anomaly ~ prop_MHW_cells + mean_SSTa , data = NWAPLLa_MHW_total_scaled %>% filter(mgmt_zone == "CAR"), na.action = na.omit)
summary(m_PLL)
plot(m_PLL, which=1)#equal variances of residuals 
plot(m_PLL, which=2)#normal distribution of residuals
hist(resid(m_PLL))# look for normality again

summary(m_PLL)$r.squared #r squared


##########
#TROLL
##########
NEPTROLLa_MHW_total<-here("data","Mgmt_zone","NEP_TROLL","NEPTROLLa_MHW_total.rds") %>% readRDS()

# #lets normalize/scale the data so we can better interpret the reg coeifficents
NEPTROLLa_MHW_total_scaled<-NEPTROLLa_MHW_total %>%
  na.omit() %>%
  mutate(prop_MHW_area = scale(prop_MHW_area), #center and scales by default
         mean_SSTa = scale(mean_SSTa) %>% as.numeric(),
         n.cell_MHW = scale(n.cell_MHW)) %>% 
  as.data.frame()



# # #build the mixed model
# MHW_glmm<-lmer(prop_habitat_cells ~ prop_MHW_cells + mean_SSTa + (1 + prop_MHW_cells|mgmt_zone), data = NEPTROLL_MHW_total, na.action = na.omit)


# MHW_glmm<-lmer(NEP_TROLL.area.anomaly ~ prop_MHW_cells + mean_SSTa + (1+prop_MHW_cells|mgmt_zone), data = NEPTROLLa_MHW_total_scaled)


MHW_glmm<-lmList(NEP_TROLL.area.anomaly ~ prop_MHW_cells + mean_SSTa|mgmt_zone , data = NEPTROLLa_MHW_total_scaled, na.action = na.omit)

summary(MHW_glmm)
coefficients(MHW_glmm)
NEP_coef<-coefficients(MHW_glmm)
plot(MHW_glmm)
confint(MHW_glmm, oldNames = FALSE)
fixef(MHW_glmm)
ranef(MHW_glmm)

mgmt_zones<-unique(NEPTROLLa_MHW_total$mgmt_zone) %>% as.vector()


#need to make predictions at different combinations for mean_SSTa and prop_MHW_area
mgmt_zone_variables_TROLL<-list()

for(i in 1:length(mgmt_zones)){
  
  SSTa_seq<-seq(min(NEPTROLLa_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(mean_SSTa), na.rm=TRUE),max(NEPTROLLa_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(mean_SSTa), na.rm=TRUE),length = 50)
  
  area_seq<-seq(min(NEPTROLLa_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(prop_MHW_cells), na.rm=TRUE),max(NEPTROLLa_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(prop_MHW_cells), na.rm=TRUE),length = 50)
  
  mz_vars <- crossing(SSTa_seq,area_seq,mgmt_zones[i]) %>% 
    rename("mean_SSTa"="SSTa_seq",
           "prop_MHW_cells"="area_seq",
           "mgmt_zone"="mgmt_zones[i]")
  
  # mgmt_zone_seq<-rep(mgmt_zones[i],length = length(area_seq))
  # 
  # mz_vars<-data.frame(mean_SSTa = SSTa_seq, prop_MHW_area = area_seq, 
  #                    mgmt_zone = mgmt_zone_seq)
  # 
  mgmt_zone_variables_TROLL[[length(mgmt_zone_variables_TROLL)+1]]<-mz_vars
}

mgmt_zone_variables_TROLL<-bind_rows(mgmt_zone_variables_TROLL)

mgmt_zone_variables_TROLL$predict_habitat_area<-predict(MHW_glmm, newdata=mgmt_zone_variables_TROLL)

mgmt_zone_variables_TROLL_split<-split(mgmt_zone_variables_TROLL, 
                                 f = mgmt_zone_variables_TROLL$mgmt_zone)

Tp1<-mgmt_zone_variables_TROLL_split$VN %>%
  ggplot() +
  geom_tile(aes(x = prop_MHW_cells, y = mean_SSTa, 
                fill = predict_habitat_area)) +
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_bw() +
  # cmocean::scale_fill_cmocean(name="delta", 
  #                             guide = guide_colorbar(title.position = "right"))+
  scale_fill_gradient2(low = "#400505FF",
                       mid = "#9A9A99FF",
                       high = "#DDAF19FF",
                       midpoint = 0,
                       guide = guide_colorbar(title.position = "right"))+
  labs(x = expression("MHW Size"), y = "MHW Intensity",
       fill = expression("% Change in Available Fishing Grounds")) +
  theme(legend.spacing.y = unit(0.2, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(5,5,-7,-7),
        legend.title = element_text(angle = 90, size = 7),
        legend.title.align = 0.5,
        strip.background = element_blank())


Tp2<- Tp1 %+% mgmt_zone_variables_TROLL_split$CL
Tp3<- Tp1 %+% mgmt_zone_variables_TROLL_split$EK
Tp4<- Tp1 %+% mgmt_zone_variables_TROLL_split$MT
#Tp5<- Tp1 %+% mgmt_zone_variables_TROLL_split$CP

(Tp1 + Tp2 + Tp3 + Tp4)

width <- max(mgmt_zone_variables_TROLL$prop_MHW_cells)-min(mgmt_zone_variables_TROLL$prop_MHW_cells)/length(mgmt_zone_variables_TROLL$prop_MHW_cells)

height <- max(mgmt_zone_variables_TROLL$mean_SSTa)-min(mgmt_zone_variables_TROLL$mean_SSTa)/length(mgmt_zone_variables_TROLL$mean_SSTa)

TROLL_plots<-mgmt_zone_variables_TROLL%>%
  mutate(mgmt_zone = factor(mgmt_zone,levels = c("VN","CL","EK",
                                                 "MT","CP"))) %>% 
  filter(mgmt_zone %in% c("VN","CL","EK","MT")) %>% 
  ggplot() +
  geom_tile(aes(x = prop_MHW_cells, y = mean_SSTa, 
                fill = predict_habitat_area, height = height, width = width)) +
  facet_wrap(~mgmt_zone, scales = "free", nrow = 4)+
  theme_bw() +
  # cmocean::scale_fill_cmocean(name="delta",
  #                             limits = c(-20,20),
  #                             guide = guide_colorbar(title.position = "right"))+
  scale_fill_gradient2(low = "#400505FF",
                       mid = "#9A9A99FF",
                       high = "#DDAF19FF",
                       midpoint = 0,
                       guide = guide_colorbar(title.position = "right"))+
  labs(x = expression("MHW Size"), y = "MHW Intensity",
       fill = expression("% Change in Fishing Grounds")) +
  theme(legend.spacing.y = unit(0.2, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(5,5,-7,-7),
        legend.title = element_text(angle = 90, size = 7),
        legend.title.align = 0.5,
        strip.background = element_blank())

#testing assumptions
m_TROLL<-lm(NEP_TROLL.area.anomaly ~ prop_MHW_cells + mean_SSTa , data = NEPTROLLa_MHW_total_scaled %>% filter(mgmt_zone == "MT"), na.action = na.omit)

plot(m_TROLL, which=1)#equal variances of residuals 
plot(m_TROLL, which=2)#normal distribution of residuals
hist(resid(m_TROLL))# look for normality again

summary(m_TROLL)$r.squared #r squared

#################
#both coasts plot
#################
Tp<-cowplot::plot_grid(Tp1,Tp2,Tp3,Tp4,align = "h",labels = c('(A)','(B)','(C)','(D)'), label_size = 12,nrow=4)

Lp<-cowplot::plot_grid(Lp1,Lp3,Lp6,Lp8,align = "h",labels = c('(E)','(F)','(G)','(H)'), label_size = 12,nrow=4)

cowplot::plot_grid(Tp,Lp, align = "v")











#############################
#both coasts at the same time
#############################

#load the data
NWAPLLa_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLLa_MHW_total.rds") %>%
  readRDS() %>% 
  dplyr::select(prop_MHW_cells,mean_SSTa,mgmt_zone,NWA_PLL.area.anomaly) %>% 
  rename("percent_change"="NWA_PLL.area.anomaly")

NEPTROLLa_MHW_total<-here("data","Mgmt_zone","NEP_TROLL","NEPTROLLa_MHW_total.rds") %>% readRDS() %>% 
  dplyr::select(prop_MHW_cells,mean_SSTa,mgmt_zone,NEP_TROLL.area.anomaly) %>% 
  rename("percent_change"="NEP_TROLL.area.anomaly") %>% 
  filter(mgmt_zone != "CP")

#bind together
all_MHW_total<-rbind(NWAPLLa_MHW_total, NEPTROLLa_MHW_total)

#lets normalize/scale the data so we can better interpret the LMM coeifficents
all_MHW_total<-all_MHW_total %>% 
  na.omit() %>% 
  mutate(prop_MHW_cells = scale(prop_MHW_cells),
         mean_SSTa = scale(mean_SSTa)) %>% 
  as.data.frame()

# # #lets normalize/scale the data so we can better interpret the reg coeifficents
# NWAPLLa_MHW_total_scaled<-NWAPLLa_MHW_total %>%
#   na.omit() %>%
#   mutate(prop_MHW_area = scale(prop_MHW_area), #center and scales by default
#          mean_SSTa = scale(mean_SSTa),
#          n.cell_MHW = scale(n.cell_MHW),
#          mean_SSTa = as.numeric(mean_SSTa)) %>% 
#   as.data.frame() %>% 
#   dplyr::select(prop_MHW_cells,mean_SSTa,mgmt_zone,NWA_PLL.area.anomaly) %>% 
#   rename("percent_change"="NWA_PLL.area.anomaly")
# 
# NEPTROLLa_MHW_total<-here("data","Mgmt_zone","NEP_TROLL","NEPTROLLa_MHW_total.rds") %>% readRDS()
# 
# # #lets normalize/scale the data so we can better interpret the reg coeifficents
# NEPTROLLa_MHW_total_scaled<-NEPTROLLa_MHW_total %>%
#   na.omit() %>%
#   mutate(prop_MHW_area = scale(prop_MHW_area), #center and scales by default
#          mean_SSTa = scale(mean_SSTa) %>% as.numeric(),
#          n.cell_MHW = scale(n.cell_MHW)) %>% 
#   as.data.frame() %>% 
#   dplyr::select(prop_MHW_cells,mean_SSTa,mgmt_zone,NEP_TROLL.area.anomaly) %>% 
#   rename("percent_change"="NEP_TROLL.area.anomaly") %>% 
#   filter(mgmt_zone != "CP")

#all_MHW_total<-rbind(NWAPLLa_MHW_total_scaled, NEPTROLLa_MHW_total_scaled)

MHW_glmm<-lmerTest::lmer(percent_change ~ prop_MHW_cells + mean_SSTa + 
                           (1|mgmt_zone)+
                           (0+prop_MHW_cells|mgmt_zone)+
                           (0+mean_SSTa|mgmt_zone), 
                         data = all_MHW_total)

#same as above

MHW_glmm<-lmerTest::lmer(percent_change ~ prop_MHW_cells + mean_SSTa +
                           (1+prop_MHW_cells+mean_SSTa|mgmt_zone),
                         data = all_MHW_total)


# MHW_glmm<-lmList(percent_change ~ prop_MHW_cells + mean_SSTa|mgmt_zone , data = all_MHW_total, na.action = na.omit)

summary(MHW_glmm)
coefficients(MHW_glmm)


mgmt_zones<-unique(all_MHW_total$mgmt_zone) %>% as.vector()


#need to make predictions at different combinations for mean_SSTa and prop_MHW_area
mgmt_zone_variables_BOTH<-list()

for(i in 1:length(mgmt_zones)){
  
  SSTa_seq<-seq(min(all_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(mean_SSTa), na.rm=TRUE),max(all_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(mean_SSTa), na.rm=TRUE),length = 50)
  
  area_seq<-seq(min(all_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(prop_MHW_cells), na.rm=TRUE),max(all_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(prop_MHW_cells), na.rm=TRUE),length = 50)
  
  mz_vars <- crossing(SSTa_seq,area_seq,mgmt_zones[i]) %>% 
    rename("mean_SSTa"="SSTa_seq",
           "prop_MHW_cells"="area_seq",
           "mgmt_zone"="mgmt_zones[i]")
  
  # mgmt_zone_seq<-rep(mgmt_zones[i],length = length(area_seq))
  # 
  # mz_vars<-data.frame(mean_SSTa = SSTa_seq, prop_MHW_area = area_seq, 
  #                    mgmt_zone = mgmt_zone_seq)
  # 
  mgmt_zone_variables_BOTH[[length(mgmt_zone_variables_BOTH)+1]]<-mz_vars
}

mgmt_zone_variables_BOTH<-bind_rows(mgmt_zone_variables_BOTH)

mgmt_zone_variables_BOTH$predict_habitat_area<-predict(MHW_glmm, newdata=mgmt_zone_variables_BOTH)

# mgmt_zone_variables_BOTH_split<-split(mgmt_zone_variables_BOTH, 
#                                        f = mgmt_zone_variables_BOTH$mgmt_zone)
# 
# Bp1<-mgmt_zone_variables_BOTH_split$VN %>%
#   ggplot() +
#   geom_tile(aes(x = prop_MHW_cells, y = mean_SSTa,
#                 fill = predict_habitat_area)) +
#   facet_wrap(~mgmt_zone, scales = "free")+
#   theme_bw() +
#   # cmocean::scale_fill_cmocean(name="delta",
#   #                             guide = guide_colorbar(title.position = "right"))+
#   # scale_fill_gradient2(low = "#400505FF",
#   #                      mid = "#9A9A99FF",
#   #                      high = "#DDAF19FF",
#   #                      midpoint = 0,
#   #                      guide = guide_colorbar(title.position = "right"))+
#   scale_fill_gradientn(colours = jet.colors(100), 
#                        limits = c(-50,50),
#                        oob = scales::squish,
#                        breaks = bzz,
#                        guide = guide_colorbar(title.position = "right"))+
#   labs(x = expression("MHW Size"), y = "MHW Intensity",
#        fill = expression("% Change in Available Fishing Grounds")) +
#   theme(legend.spacing.y = unit(0.2, 'cm'),
#         legend.spacing.x = unit(0.1, 'cm'),
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(5,5,-7,-7),
#         legend.title = element_text(angle = 90, size = 7),
#         legend.title.align = 0.5,
#         strip.background = element_blank())
# 
# 
# Bp2<- Bp1 %+% mgmt_zone_variables_BOTH_split$CL
# Bp3<- Bp1 %+% mgmt_zone_variables_BOTH_split$EK
# Bp4<- Bp1 %+% mgmt_zone_variables_BOTH_split$MT
# Bp5<- Bp1 %+% mgmt_zone_variables_BOTH_split$NED
# Bp6<- Bp1 %+% mgmt_zone_variables_BOTH_split$NEC
# Bp7<- Bp1 %+% mgmt_zone_variables_BOTH_split$MAB
# Bp8<- Bp1 %+% mgmt_zone_variables_BOTH_split$SAB
# Bp9<- Bp1 %+% mgmt_zone_variables_BOTH_split$SAR
# Bp10<- Bp1 %+% mgmt_zone_variables_BOTH_split$FEC
# Bp11<- Bp1 %+% mgmt_zone_variables_BOTH_split$GOM
# Bp12<- Bp1 %+% mgmt_zone_variables_BOTH_split$CAR
# 
# (Bp1 + Bp5) / (Bp2 + Bp7) / (Bp3 + Bp10) / (Bp4 + Bp12)
# 



# width <- max(mgmt_zone_variables_BOTH$prop_MHW_cells)-min(mgmt_zone_variables_BOTH$prop_MHW_cells)/length(mgmt_zone_variables_BOTH$prop_MHW_cells)
# 
# height <- max(mgmt_zone_variables_BOTH$mean_SSTa)-min(mgmt_zone_variables_BOTH$mean_SSTa)/length(mgmt_zone_variables_BOTH$mean_SSTa)


## general plot setup
jet.colors <- colorRampPalette(c("#400505FF", "#5D070CFF", "#7D050EFF",   
                                 "#9A9A99FF","#EAEA3BFF", "#E5CB26FF",
                                 "#DDAF19FF"))

bzz <- c(-50,-25,0,25,50)


# ts_labels_NEP<-data.frame(mgmt_zone = factor(c("VN","CL","EK",
#                                                "MT")),
#                           label = c("(A)","(B)","(C)","(D)"))

Tp<-mgmt_zone_variables_BOTH%>%
  mutate(mgmt_zone = factor(mgmt_zone,levels = c("VN","NED","CL", "NEC",
                                                 "EK","MAB","MT", "SAB",
                                                 "SAR","FEC","GOM","CAR"))) %>% 
  filter(mgmt_zone %in% c("VN","CL","EK","MT")) %>% 
  # mutate(mgmt_zone = factor(mgmt_zone,levels = c("VN","NED","CL","MAB","EK","FEC","MT","CAR"))) %>% 
  ggplot() +
  geom_raster(aes(x = prop_MHW_cells, y = mean_SSTa, 
                fill = predict_habitat_area)) +
  facet_wrap(~mgmt_zone, scales = "free", nrow = 6)+
  theme_bw() +
  # cmocean::scale_fill_cmocean(name="delta",
  #                             limits = c(-20,20),
  #                             guide = guide_colorbar(title.position = "right"))+
  # scale_fill_gradient2(low = "#400505FF",
  #                      mid = "#9A9A99FF",
  #                      high = "#DDAF19FF",
  #                      midpoint = 0,
  #                      guide = guide_colorbar(title.position = "right"))+
  scale_fill_gradientn(colours = jet.colors(100), 
                       limits = c(-50,50),
                       oob = scales::squish,
                       breaks = bzz,
                       guide = guide_colorbar(title.position = "right"))+
  labs(x = expression("MHW Size"), y = "MHW Intensity",
       fill = expression("% Change in Core Fishing Grounds")) +
  theme(legend.spacing.y = unit(0.2, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(5,5,-7,-7),
        legend.title = element_text(angle = 90, size = 7),
        legend.title.align = 0.5,
        strip.background = element_blank(),
        legend.position = "none")
  # geom_text(data=ts_labels_NEP, aes(label = label, x = -Inf, y = Inf),
  #           hjust = 0, vjust = 1, fontface="bold")


# ts_labels_NWA<-data.frame(mgmt_zone = factor(c("NED","NEC","MAB",
#                                                "SAB","SAR", "FEC",
#                                                "GOM", "CAR")),
#                           label = c("(E)","(F)","(G)","(H)",
#                                     "(I)","(J)","(K)","(L)"))
# 
# ts_labels_NWA<-data.frame(mgmt_zone = factor(c("NED","MAB",
#                                                "FEC","CAR")),
#                           label = c("(E)","(F)","(G)","(H)"))

Lp<-mgmt_zone_variables_BOTH%>%
  mutate(mgmt_zone = factor(mgmt_zone,levels = c("VN","NED","CL", "NEC",
                                                 "EK","MAB","MT", "SAB",
                                                 "SAR","FEC","GOM","CAR"))) %>% 
  filter(mgmt_zone %in% c("NED","NEC", "MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>%
  # filter(mgmt_zone %in% c("NED","MAB",
  #                         "FEC","CAR")) %>% 
  ggplot() +
  geom_raster(aes(x = prop_MHW_cells, y = mean_SSTa, 
                fill = predict_habitat_area)) +
  facet_wrap(~mgmt_zone, scales = "free", nrow = 8)+
  theme_bw() +
  # cmocean::scale_fill_cmocean(name="delta",
  #                             limits = c(-20,20),
  #                             guide = guide_colorbar(title.position = "right"))+
  # scale_fill_gradient2(low = "#400505FF",
  #                      mid = "#9A9A99FF",
  #                      high = "#DDAF19FF",
  #                      midpoint = 0,
  #                      guide = guide_colorbar(title.position = "right"))+
  scale_fill_gradientn(colours = jet.colors(100), 
                       limits = c(-50,50),
                       oob = scales::squish,
                       breaks = bzz,
                       guide = guide_colorbar(title.position = "right"))+
  labs(x = expression("MHW Size"), y = "MHW Intensity",
       fill = expression("% Change in Core Fishing Grounds")) +
  theme(legend.spacing.y = unit(0.2, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(5,5,-7,-7),
        legend.title = element_text(angle = 90, size = 7),
        legend.title.align = 0.5,
        strip.background = element_blank())
  # geom_text(data=ts_labels_NWA, aes(label = label, x = -Inf, y = Inf),
  #           hjust = 0, vjust = 1, fontface="bold")

cowplot::plot_grid(Tp,Lp,labels = c('A', 'B'), label_size = 12)

#Tp | Lp





######################
#both coasts coef plot
######################


#management zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_zones<-st_difference(NWA_PLL_zones, st_union(st_combine(land)))


NEP_TROLL_zones<-here("data","shapefiles","NEP_TROLL","Zones_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NEP_TROLL_zones<-st_difference(NEP_TROLL_zones, st_union(st_combine(land)))


all_zones<-rbind(NWA_PLL_zones,NEP_TROLL_zones)

#center coordinates for each mgmt_zone
mgmtzone_centroid<-all_zones %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtzone_centroid<-cbind(all_zones$ET_ID,mgmtzone_centroid)
colnames(mgmtzone_centroid)<-c("mgmt_zone","X","Y")


#combine both coasts coef df
mgmt_zone_coef<-coefficients(MHW_glmm)$mgmt_zone

mgmt_zone_coef<-mgmt_zone_coef %>%  
  tibble::rownames_to_column("mgmt_zone") %>% 
  mutate(Fishery = case_when(mgmt_zone %in% c("NED","NEC", "MAB",
                                              "SAB","SAR","FEC",
                                              "GOM","CAR") ~ "Longline",
         mgmt_zone %in% c("VN","CL","EK","MT") ~ "Troll")) %>% 
  rename("Intensity"="mean_SSTa",
         "Size"="prop_MHW_cells") %>% 
  left_join(., mgmtzone_centroid, by = c("mgmt_zone")) %>%
  gather("Parameters","Coefficients",-mgmt_zone,-X,-Y,-Fishery)
  

mgmt_zone_coef %>% filter(Parameters != "(Intercept)") %>% 
  ggplot() +
  geom_path(aes(x = Coefficients, y = Y, color = Fishery),
            size = 1) +
  geom_label(aes(x = Coefficients, y = Y,label = mgmt_zone))+
  facet_wrap(~Parameters, scales = "free_x") + theme_bw() + 
  labs(x = "% Change in Core Fishing Ground Area\nper 1 unit increase in MHW Intensity or Size", y = "Latitude", color = "Fishery")+
  scale_color_manual(values=c("orange","lightseagreen"))
