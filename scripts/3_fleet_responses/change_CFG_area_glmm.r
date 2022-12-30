# Change in Core Fishing Ground Area

library(sf)
library(purrr)
library(lme4)
library(tidyverse)
library(patchwork)
library(here)
sf_use_s2(FALSE)


#load both fishery data sets
NWAPLL_CFG_MHW_total<-here("data","mgmt_area_metrics","NWA_PLL",
                        "NWAPLL_CFG_MHW_total.rds") %>%
  readRDS() %>% 
  dplyr::select(prop_MHW_cells,mean_SSTa,sequence_months,mgmt_area,
                NWA_PLL.CFG.change, MHW) %>% 
  rename("percent_change"="NWA_PLL.CFG.change")

NEPTROLL_CFG_MHW_total<-here("data","mgmt_area_metrics","NEP_TROLL",
                             "NEPTROLL_CFG_MHW_total.rds") %>% 
  readRDS() %>% 
  dplyr::select(prop_MHW_cells,mean_SSTa,mgmt_area,
                sequence_months,NEP_TROLL.CFG.change, MHW) %>% 
  rename("percent_change"="NEP_TROLL.CFG.change") %>% 
  filter(mgmt_area != "CP") #removing CP from analysis due having historically low effort in this region


#bind together
all_CFG_MHW_total<-rbind(NWAPLL_CFG_MHW_total, NEPTROLL_CFG_MHW_total) %>% 
  mutate(sequence_months = as.numeric(sequence_months),
         sequence_months = if_else(MHW == 0, 0, sequence_months))

#lets normalize/scale the data so we can better interpret the LMM coefficients
all_CFG_MHW_total<-all_CFG_MHW_total %>% 
  na.omit() %>% 
  mutate(prop_MHW_cells = scale(prop_MHW_cells),
         mean_SSTa = scale(mean_SSTa),
         sequence_months = scale(sequence_months)) %>% 
  as.data.frame()

####################################################
# linear mixed model - varying intercepts and slopes
####################################################
MHW_glmm<-lmerTest::lmer(percent_change ~ prop_MHW_cells + mean_SSTa +
                           sequence_months +
                           (1+prop_MHW_cells+mean_SSTa+sequence_months|mgmt_area),
                         data = all_CFG_MHW_total)


summary(MHW_glmm)
coef(MHW_glmm)#estiamte coefficients in each mgmt area
fixef(MHW_glmm)#average coefficients 
lme4::ranef(MHW_glmm)#group level errors 



mgmt_areas<-unique(all_CFG_MHW_total$mgmt_area) %>% as.vector()


######################
#both coasts coef plot
######################


#management areas shapefile
NWA_PLL_areas<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

land<-here("data","shapefiles","Land","GSHHS_l_L1.shp") %>% st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_areas<-st_difference(NWA_PLL_areas, st_union(st_combine(land)))


NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NEP_TROLL_areas<-st_difference(NEP_TROLL_areas, st_union(st_combine(land)))


all_areas<-rbind(NWA_PLL_areas,NEP_TROLL_areas)

#center coordinates for each mgmt_area
mgmtarea_centroid<-all_areas %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
mgmtarea_centroid<-cbind(all_areas$ET_ID,mgmtarea_centroid)
colnames(mgmtarea_centroid)<-c("mgmt_area","X","Y")


#combine both coasts coef df
mgmt_area_coef<-coefficients(MHW_glmm)$mgmt_area

mgmt_area_coef<-mgmt_area_coef %>%  
  tibble::rownames_to_column("mgmt_area") %>% 
  mutate(Fleet = case_when(mgmt_area %in% c("NED","NEC", "MAB",
                                              "SAB","SAR","FEC",
                                              "GOM","CAR") ~ "Longline",
         mgmt_area %in% c("VN","CL","EK","MT") ~ "Troll")) %>% 
  rename("Intensity"="mean_SSTa",
         "Size"="prop_MHW_cells",
         "Duration"="sequence_months") %>%
  left_join(., mgmtarea_centroid, by = c("mgmt_area")) %>%
  gather("Parameters","Coefficients",-mgmt_area,-X,-Y,-Fleet) %>% 
  mutate(Parameters = factor(Parameters, levels = c("Intensity","Size",
                                                    "Duration","(Intercept)")))
  

mgmt_area_coef %>% filter(Parameters != "(Intercept)") %>% 
  ggplot() +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "gray", size = 1)+
  geom_path(aes(x = Coefficients, y = Y, color = Fleet),
            size = 1) +
  geom_label(aes(x = Coefficients, y = Y,label = mgmt_area))+
  facet_wrap(~Parameters) + theme_bw() + 
  labs(x = "% Change in Core Fishing Ground Area\nper 1 unit increase in MHW Property", y = "Latitude", color = "Fleet")+
  scale_color_manual(values=c("orange","lightseagreen"))+
  xlim(-10, 26)

ggsave(here("Plots","F4_GLMMcoefficents.png"),
       width = 10, height = 6, units = "in", dpi = 300)
ggsave(here("Plots","F4_GLMMcoefficents.svg"),
       width = 10, height = 6, units = "in", dpi = 300)
