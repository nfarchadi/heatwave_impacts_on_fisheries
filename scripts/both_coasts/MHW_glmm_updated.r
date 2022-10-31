library(sf)
library(purrr)
library(lme4)
library(tidyverse)
library(plotly)
library(patchwork)
library(here)
sf_use_s2(FALSE)


#load both fishery data sets
NWAPLLa_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLLa_MHW_total.rds") %>%
  readRDS() %>% 
  dplyr::select(prop_MHW_cells,mean_SSTa,sequence_months,mgmt_zone,NWA_PLL.area.anomaly, MHW.x) %>% 
  rename("percent_change"="NWA_PLL.area.anomaly",
         "MHW"="MHW.x")

NEPTROLLa_MHW_total<-here("data","Mgmt_zone","NEP_TROLL","NEPTROLLa_MHW_total.rds") %>% readRDS() %>% 
  dplyr::select(prop_MHW_cells,mean_SSTa,mgmt_zone,sequence_months,NEP_TROLL.area.anomaly, MHW) %>% 
  rename("percent_change"="NEP_TROLL.area.anomaly") %>% 
  filter(mgmt_zone != "CP")


#bind together
all_MHW_total<-rbind(NWAPLLa_MHW_total, NEPTROLLa_MHW_total) %>% 
  mutate(sequence_months = as.numeric(sequence_months),
         sequence_months = if_else(MHW == 0, 0, sequence_months))

#lets normalize/scale the data so we can better interpret the LMM coefficients
all_MHW_total<-all_MHW_total %>% 
  na.omit() %>% 
  mutate(prop_MHW_cells = scale(prop_MHW_cells),
         mean_SSTa = scale(mean_SSTa),
         sequence_months = scale(sequence_months)) %>% 
  as.data.frame()

# removes correlation parameter between random intercepts and slops
# MHW_glmm<-lmerTest::lmer(percent_change ~ prop_MHW_cells + mean_SSTa + 
#                            (1|mgmt_zone)+
#                            (0+prop_MHW_cells|mgmt_zone)+
#                            (0+mean_SSTa|mgmt_zone), 
#                          data = all_MHW_total)

#Keeps correlation parameter
MHW_glmm<-lmerTest::lmer(percent_change ~ prop_MHW_cells + mean_SSTa +
                           sequence_months +
                           (1+prop_MHW_cells+mean_SSTa+sequence_months|mgmt_zone),
                         data = all_MHW_total)


summary(MHW_glmm)
coef(MHW_glmm)#estiamte coefficients in each mgmt zone
fixef(MHW_glmm)#average coefficients 
lme4::ranef(MHW_glmm)#group level errors 



mgmt_zones<-unique(all_MHW_total$mgmt_zone) %>% as.vector()

######################################################################
#predictions at different combinations for mean_SSTa and prop_MHW_area
######################################################################

mgmt_zone_variables_BOTH<-list()

for(i in 1:length(mgmt_zones)){
  
  SSTa_seq<-seq(min(all_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(mean_SSTa), na.rm=TRUE),max(all_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(mean_SSTa), na.rm=TRUE),length = 50)
  
  area_seq<-seq(min(all_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(prop_MHW_cells), na.rm=TRUE),max(all_MHW_total %>% filter(mgmt_zone==mgmt_zones[i]) %>% pull(prop_MHW_cells), na.rm=TRUE),length = 50)
  
  mz_vars <- crossing(SSTa_seq,area_seq,mgmt_zones[i]) %>% 
    rename("mean_SSTa"="SSTa_seq",
           "prop_MHW_cells"="area_seq",
           "mgmt_zone"="mgmt_zones[i]")
  mgmt_zone_variables_BOTH[[length(mgmt_zone_variables_BOTH)+1]]<-mz_vars
}

mgmt_zone_variables_BOTH<-bind_rows(mgmt_zone_variables_BOTH)

mgmt_zone_variables_BOTH$predict_habitat_area<-predict(MHW_glmm, newdata=mgmt_zone_variables_BOTH)


## general plot setup
jet.colors <- colorRampPalette(c("#400505FF", "#5D070CFF", "#7D050EFF",   
                                 "#9A9A99FF","#EAEA3BFF", "#E5CB26FF",
                                 "#DDAF19FF"))

bzz <- c(-50,-25,0,25,50)


Tp<-mgmt_zone_variables_BOTH%>%
  mutate(mgmt_zone = factor(mgmt_zone,levels = c("VN","NED","CL", "NEC",
                                                 "EK","MAB","MT", "SAB",
                                                 "SAR","FEC","GOM","CAR"))) %>% 
  filter(mgmt_zone %in% c("VN","CL","EK","MT")) %>% 
  ggplot() +
  geom_raster(aes(x = prop_MHW_cells, y = mean_SSTa, 
                fill = predict_habitat_area)) +
  facet_wrap(~mgmt_zone, scales = "free", nrow = 6)+
  theme_bw() +
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

Lp<-mgmt_zone_variables_BOTH%>%
  mutate(mgmt_zone = factor(mgmt_zone,levels = c("VN","NED","CL", "NEC",
                                                 "EK","MAB","MT", "SAB",
                                                 "SAR","FEC","GOM","CAR"))) %>% 
  filter(mgmt_zone %in% c("NED","NEC", "MAB",
                          "SAB","SAR","FEC",
                          "GOM","CAR")) %>%
  ggplot() +
  geom_raster(aes(x = prop_MHW_cells, y = mean_SSTa, 
                fill = predict_habitat_area)) +
  facet_wrap(~mgmt_zone, scales = "free", nrow = 8)+
  theme_bw() +
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

cowplot::plot_grid(Tp,Lp,labels = c('A', 'B'), label_size = 12)



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
         "Size"="prop_MHW_cells",
         "Duration"="sequence_months") %>%
  left_join(., mgmtzone_centroid, by = c("mgmt_zone")) %>%
  gather("Parameters","Coefficients",-mgmt_zone,-X,-Y,-Fishery) %>% 
  mutate(Parameters = factor(Parameters, levels = c("Intensity","Size",
                                                    "Duration","(Intercept)")))
  

mgmt_zone_coef %>% filter(Parameters != "(Intercept)") %>% 
  ggplot() +
  geom_path(aes(x = Coefficients, y = Y, color = Fishery),
            size = 1) +
  geom_label(aes(x = Coefficients, y = Y,label = mgmt_zone))+
  facet_wrap(~Parameters) + theme_bw() + 
  labs(x = "% Change in Core Fishing Ground Area\nper 1 unit increase in MHW Property", y = "Latitude", color = "Fishery")+
  scale_color_manual(values=c("orange","lightseagreen"))
