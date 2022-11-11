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

####
#NWA
####

######################################
#shift / total area in mgmt zone
######################################
NWAPLL_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_total.rds") %>% 
  readRDS()

month_avgCOG<-NWAPLL_MHW_total %>% filter(MHW == 0) %>% 
  group_by(mgmt_zone,month) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG_NWA<-NWAPLL_MHW_total %>% 
  dplyr::select(MHW,month,event_index,duration_months, 
                mgmt_zone,COGx,COGy,prop_MHW_area,
                mean_SSTa,date,prop_habitat_cells, 
                prop_MHW_cells, sequence_months) %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326")


MHWCOG_NWA$distance <- map_dbl(1:nrow(MHWCOG_NWA), function(x){
  x <- MHWCOG_NWA[x,]
  y <- month_avgCOG[month_avgCOG$mgmt_zone == x$mgmt_zone & month_avgCOG$month == x$month,]
  st_distance(x, y)
})


MHWCOG_NWA<-MHWCOG_NWA %>% mutate(distance = distance * 0.001,
                                  lon = sf::st_coordinates(.)[,1],
                                  lat = sf::st_coordinates(.)[,2])

mgmt_zone_area<-st_area(NWA_PLL_zones)
units(mgmt_zone_area)<-NULL #remove the units

area_df<-data.frame(mgmt_zone = NWA_PLL_zones$ET_ID %>% unique(),area = mgmt_zone_area*0.000001) #converting to km^2

MHWCOG_NWA<-left_join(MHWCOG_NWA, area_df, by=c("mgmt_zone")) %>% 
  mutate(dis_per_area = distance / area)


#######
#sst_sd
#######
#function to calculate landscape metrics per mgmt zone
mgmt_zone_sstsd<-function(oisst,cog){
  
  mgmtzone_sst_sd_list<-list()
  
  # mgmtzone<-oisst %>% dplyr::select(ET_ID) %>% unique() %>% pull()
  udates<-cog %>% dplyr::select(date) %>% pull(date) %>% unique()
  
  for(i in 1:length(udates)){
    print(paste(udates[i]))
    
    oisst_date<-oisst %>% filter(yearmon == udates[i])
    cog_date<-cog %>% filter(date == udates[i])
    
    sst<-oisst_date %>% 
        dplyr::select(lon,lat,temp_monthly) %>%  
        rasterFromXYZ()
    
    sst_sd <- raster::focal(sst, w = matrix(1, nrow = 3, ncol = 3), 
                              fun = function(x) stats::sd(x, na.rm = TRUE))
    
    sst_gradient_mean<-raster::cellStats(sst_sd, mean, na.rm=TRUE)
    sst_gradient_sd<-raster::cellStats(sst_sd, sd, na.rm=TRUE)
    
    cog_sst_sd<-raster::extract(sst_sd,cog_date)
    
    sst_gradient_df<-data.frame(sst_gradient=cog_sst_sd,
                                lat = cog_date %>% as.data.frame() %>% 
                                  dplyr::select(lat),
                                sst_gradient_mean=sst_gradient_mean,
                                sst_gradient_sd=sst_gradient_sd,
                                lon = cog_date %>% as.data.frame() %>% 
                                  dplyr::select(lon),
                                mgmt_zone=cog_date %>% as.data.frame() %>% 
                                  dplyr::select(mgmt_zone),
                                date=cog_date %>% as.data.frame() %>% 
                                  dplyr::select(date))
    
    mgmtzone_sst_sd_list[[length(mgmtzone_sst_sd_list)+1]]=sst_gradient_df
      
  }
  return(mgmtzone_sst_sd_list)
}


north_oisst<-here("data","water_temp","NWA","oisst","MAB_NEC_NED_MHW.rds") %>% readRDS()

north_sst_gradient<-mgmt_zone_sstsd(north_oisst,MHWCOG_NWA)

north_sst_gradient<-bind_rows(north_sst_gradient) %>% na.omit()
rm(north_oisst)


south_oisst<-here("data","water_temp","NWA","oisst","GOM_CAR_FEC_SAR_SAB_MHW.rds") %>% readRDS()

south_sst_gradient<-mgmt_zone_sstsd(south_oisst,MHWCOG_NWA)

south_sst_gradient<-bind_rows(south_sst_gradient) %>% na.omit()
rm(south_oisst)

sst_gradient<-rbind(north_sst_gradient,south_sst_gradient)

rm(north_sst_gradient,south_sst_gradient)

MHWCOG_NWA<-left_join(MHWCOG_NWA, sst_gradient, by=c("mgmt_zone","date",
                                                     "lon","lat"))

# #area anomaly join
# NWAPLLa_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLLa_MHW_total.rds") %>%
#   readRDS() %>% dplyr::select("date","mgmt_zone","NWA_PLL.area.anomaly") %>% 
#   rename("percent_change"="NWA_PLL.area.anomaly")
# 
# MHWCOG_NWA<-left_join(MHWCOG_NWA, NWAPLLa_MHW_total, by=c("mgmt_zone","date"))


MHWCOG_NWA %>% #filter(mgmt_zone %in% c("NED","MAB","FEC","CAR")) %>%
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("NED","NEC","MAB",
                                                  "SAB","SAR","FEC",
                                                  "GOM","CAR"))) %>% 
  mutate(MHW = as.factor(MHW)) %>% 
  ggplot()+
  geom_point(aes(x=prop_MHW_cells,y=sst_gradient,
                 color=mgmt_zone),size = 2)+
  facet_wrap(~mgmt_zone, nrow=4)+
  geom_smooth(method = "lm", aes(x=prop_MHW_cells,y=sst_gradient))+
  theme_bw()




#####
#NEP
#####
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

######################################
#shift / total area in mgmt zone
######################################

NEPTROLL_MHW_total<-here("data","Mgmt_zone","NEP_TROLL",
                         "NEPTROLL_MHW_total.rds") %>% 
  readRDS()


month_avgCOG<-NEPTROLL_MHW_total %>% filter(MHW == 0) %>% 
  group_by(mgmt_zone,month) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG_NEP<-NEPTROLL_MHW_total %>% #filter(MHW == 1) %>% 
  dplyr::select(MHW,month,event_index,duration_months, 
                mgmt_zone,COGx,COGy,prop_MHW_area,
                mean_SSTa,date,prop_habitat_cells, 
                prop_MHW_cells, sequence_months) %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326")


MHWCOG_NEP$distance <- map_dbl(1:nrow(MHWCOG_NEP), function(x){
  x <- MHWCOG_NEP[x,]
  y <- month_avgCOG[month_avgCOG$mgmt_zone == x$mgmt_zone & month_avgCOG$month == x$month,]
  st_distance(x, y)
})


MHWCOG_NEP<-MHWCOG_NEP %>% mutate(distance = distance * 0.001,
                                  lon = sf::st_coordinates(.)[,1],
                                  lat = sf::st_coordinates(.)[,2])

mgmt_zone_area<-st_area(NEP_TROLL_zones)
units(mgmt_zone_area)<-NULL #remove the units

area_df<-data.frame(mgmt_zone = NEP_TROLL_zones$ET_ID %>% unique(),area = mgmt_zone_area*0.000001) #converting to km^2

MHWCOG_NEP<-left_join(MHWCOG_NEP, area_df, by=c("mgmt_zone")) %>% 
  mutate(dis_per_area = distance / area)


MHWCOG_NEP<-MHWCOG_NEP %>% 
  filter(mgmt_zone %in% c("VN","CL","EK", "MT")) %>% 
  mutate(mgmt_zone == factor(mgmt_zone,
                             levels = c("VN","CL","EK",
                                        "MT","CP")))



NEP_oisst<-here("data","water_temp","NEP","oisst","Conception_Monterey_Eureka_Columbia_Vancouver_MHW.rds") %>% readRDS() %>% 
  mutate(ET_ID = case_when(ET_ID == "Conception" ~ "CP",
                           ET_ID == "Monterey" ~ "MT",
                           ET_ID == "Eureka" ~ "EK",
                           ET_ID == "Columbia" ~ "CL",
                           ET_ID == "Vancouver" ~ "VN"))


NEP_sst_gradient<-mgmt_zone_sstsd(NEP_oisst,MHWCOG_NEP)

NEP_sst_gradient<-bind_rows(NEP_sst_gradient) %>% na.omit()
rm(NEP_oisst)

MHWCOG_NEP<-left_join(MHWCOG_NEP, NEP_sst_gradient, by=c("mgmt_zone","date",
                                                         "lon","lat"))

MHWCOG_NEP %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("VN","CL","EK","MT"))) %>% 
  mutate(MHW = as.factor(MHW)) %>% 
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
  ggplot()+
  geom_point(aes(x=sst_gradient,y=dis_per_area*1000000,
                 color=MHW, shape = MHW),size = 2)+
  facet_wrap(~mgmt_zone,nrow=4)+
  theme_bw() 


#####
MHWCOG_NWA_NEP<-rbind(MHWCOG_NWA %>% 
                        dplyr::select(dis_per_area,sst_gradient,
                                      sst_gradient_mean,sst_gradient_sd,
                                      MHW,mgmt_zone,prop_MHW_cells, 
                                      mean_SSTa, sequence_months),
                      MHWCOG_NEP%>% 
                        dplyr::select(dis_per_area,sst_gradient,
                                      sst_gradient_mean,sst_gradient_sd,
                                      MHW,mgmt_zone,prop_MHW_cells, 
                                      mean_SSTa, sequence_months))

MHWCOG_NWA_NEP %>%
  mutate(MHW = as.factor(MHW)) %>% 
  filter(mgmt_zone %in% c("VN","NED",
                         "CL","MAB",
                         "EK","FEC",
                         "MT","CAR")) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("VN","NED",
                                                  "CL","MAB",
                                                  "EK","FEC",
                                                  "MT","CAR"))) %>% 
  na.omit() %>% 
  ggplot()+
  geom_point(aes(x=sst_gradient,y=dis_per_area*1000000,
                 color=MHW),size = 2)+
  facet_wrap(~mgmt_zone, scales = "free_y", nrow=4)+
  # geom_smooth(method = lm, se = FALSE,aes(x=prop_MHW_cells,y=dis_per_area*1000000),size = 1)+
  theme_bw() +
  labs(x="mean sst gradient",y="Distance")

#############################
#distance vs distance to port
#############################
dis2port_NWA <- raster("E:/HYCOM/NWA_dis_port_masked_updatednobuffer.nc")
dis2port_NEP <- raster("E:/HYCOM_NEP/NEP_ALB_dis_port_masked.nc")

mgmt_zone_dis2port<-function(dis2port_nc,cog){
  
  mgmtzone_dis2port_list<-list()
  
  udates<-cog$date %>% unique()
  
  for (i in 1:length(udates)){
    
    print(udates[i])
    
    cog_date<-cog %>% filter(date == udates[i])
      
      cog_dis2port<-raster::extract(dis2port_nc,cog_date)
      
      dis2port_df<-data.frame(dis2port=cog_dis2port,
                                  lat = cog_date %>% as.data.frame() %>% 
                                dplyr::select(lat),
                                  lon = cog_date %>% as.data.frame() %>% 
                                dplyr::select(lon),
                                  mgmt_zone=cog_date %>% as.data.frame() %>% 
                                dplyr::select(mgmt_zone),
                                  date=cog_date %>% as.data.frame() %>% 
                                dplyr::select(date))
      
      
      mgmtzone_dis2port_list[[length(mgmtzone_dis2port_list)+1]]=dis2port_df
      
    }
  return(mgmtzone_dis2port_list)
}

NWA_dis2port<-mgmt_zone_dis2port(dis2port_NWA,MHWCOG_NWA)
NWA_dis2port<-bind_rows(NWA_dis2port)
MHWCOG_NWA<-left_join(MHWCOG_NWA, NWA_dis2port, by=c("mgmt_zone","date",
                                                         "lon","lat"))

NEP_dis2port<-mgmt_zone_dis2port(dis2port_NEP,MHWCOG_NEP)
NEP_dis2port<-bind_rows(NEP_dis2port)
MHWCOG_NEP<-left_join(MHWCOG_NEP, NEP_dis2port, by=c("mgmt_zone","date",
                                                     "lon","lat"))




MHWCOG_NWA_NEP<-rbind(MHWCOG_NWA %>% 
                        dplyr::select(dis_per_area, dis2port, sst_gradient,
                                      sst_gradient_mean,sst_gradient_sd,
                                      MHW,mgmt_zone,
                                      prop_MHW_cells, mean_SSTa,lat,lon,
                                      prop_MHW_area, prop_habitat_cells),
                      MHWCOG_NEP %>% 
                        dplyr::select(dis_per_area, dis2port,sst_gradient,
                                      sst_gradient_mean,sst_gradient_sd,
                                      MHW,mgmt_zone,
                                      prop_MHW_cells, mean_SSTa,lat,lon,
                                      prop_MHW_area, prop_habitat_cells))

#need to show this mechanistically
cor_gradient<-cor.test(MHWCOG_NWA_NEP$dis_per_area, MHWCOG_NWA_NEP$sst_gradient_mean,
                       method = 'spearman')
cor_size<-cor.test(MHWCOG_NWA_NEP$dis_per_area, MHWCOG_NWA_NEP$prop_MHW_cells,
                   method = 'spearman')
cor_hab<-cor.test(MHWCOG_NWA_NEP$dis_per_area, MHWCOG_NWA_NEP$prop_habitat_cells,
                  method = 'spearman')


sst_sd_plot<-MHWCOG_NWA_NEP %>%
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>%  
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("NED","NEC","MAB",
                                                  "SAB","SAR","FEC",
                                                  "GOM","CAR","VN",
                                                  "CL","EK","MT"))) %>%
  na.omit() %>% 
  ggplot()+
  geom_point(aes(x=sst_gradient,y=dis_per_area*1000000,
                 fill = mgmt_zone),shape = 21, size = 3)+
  stat_ellipse(aes(x=prop_habitat_cells,y=dis_per_area*1000000,
                   color=MHW),size = 1.5)+
  theme_bw()+theme()+
  # geom_text(aes(x=2,y=500,
  #               label = paste0("r = ",round(cor_gradient$estimate,3))))+
  # geom_text(aes(x=2,y=450,
  #               label = paste0("p = ",round(cor_gradient$p.value,3))))+
  labs(x="SST Gradient", y="Relative Distance x 10e5") +
  viridis::scale_fill_viridis(discrete = TRUE)


MHW_size_plot<-MHWCOG_NWA_NEP %>%
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("NED","NEC","MAB",
                                                  "SAB","SAR","FEC",
                                                  "GOM","CAR","VN",
                                                  "CL","EK","MT"))) %>% 
  mutate(MHW = as.factor(MHW),
         prop_MHW_cells = if_else(is.na(prop_MHW_cells), 0, prop_MHW_cells)) %>% 
  ggplot()+
  geom_point(aes(x=prop_MHW_cells,y=dis_per_area*1000000,
                 fill=mgmt_zone),shape = 21, size = 3)+
  stat_ellipse(aes(x=prop_habitat_cells,y=dis_per_area*1000000,
                   color=MHW),size = 1.5)+
  theme_bw()+ theme(legend.position = "none")+
  geom_text(aes(x=0.75,y=500,
                label = paste0("r = ",round(cor_size$estimate,3))))+
  geom_text(aes(x=0.75,y=450,
                label = paste0("p < 0.001")))+
  labs(x="MHW Size",y="",  fill = "Management\nArea")+
  viridis::scale_fill_viridis(discrete = TRUE)


prop_hab_plot<-MHWCOG_NWA_NEP %>%
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("NED","NEC","MAB",
                                                  "SAB","SAR","FEC",
                                                  "GOM","CAR","VN",
                                                  "CL","EK","MT"))) %>%
  na.omit() %>% 
  ggplot()+
  geom_point(aes(x=prop_habitat_cells,y=dis_per_area*1000000,
                 fill = mgmt_zone),shape = 21,size = 2)+
  #facet_wrap(~mgmt_zone,scales="free_y",nrow=4)+
  stat_ellipse(aes(x=prop_habitat_cells,y=dis_per_area*1000000,
                   color=MHW),size = 1.5)+
  theme_bw()+ 
  geom_text(aes(x=0.75,y=500,
                label = paste0("r = ",round(cor_hab$estimate,3))))+
  geom_text(aes(x=0.75,y=450,
                label = paste0("p < 0.001")))+
  labs(x="Core Fishing Ground Size", y="", fill = "Management\nArea")+
  viridis::scale_fill_viridis(discrete = TRUE)

sst_sd_plot + MHW_size_plot + prop_hab_plot



































MHWCOG_NWA_NEP %>%
  # mutate(MHW = as.factor(MHW)) 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("NED","NEC","MAB",
                                                  "SAB","SAR","FEC",
                                                  "GOM","CAR","VN",
                                                  "CL","EK","MT"))) %>%
  na.omit() %>% 
  ggplot()+
  geom_point(aes(x=prop_MHW_cells,y=sst_gradient_mean,
                 color = mgmt_zone),size = 2)+
  geom_smooth(method = "lm",aes(x=prop_MHW_cells,y=sst_gradient_mean,
                  color = mgmt_zone))+
  theme_bw()+
  labs(x="MHW Size", y="SST Gradient", color = "Management\nArea")

MHWCOG_NWA_NEP %>%
  mutate(MHW = as.factor(MHW)) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("NED","NEC","MAB",
                                                  "SAB","SAR","FEC",
                                                  "GOM","CAR","VN",
                                                  "CL","EK","MT"))) %>%
  na.omit() %>% 
  ggplot()+
  geom_point(aes(x=prop_habitat_cells,y=dis_per_area*1000000,
                 fill = mgmt_zone),shape = 21, size = 3)+
  #facet_wrap(~mgmt_zone,scales="free_y",nrow=4)+
  # stat_ellipse(aes(x=mean_SSTa,y=dis_per_area,
  #                  color=MHW),size = 1)+
  stat_ellipse(aes(x=prop_habitat_cells,y=dis_per_area*1000000,
                   color=MHW),size = 1)+
  theme_bw()+
  labs(x="Proportion of CFG", y="Relative Distance x 10e5", color = "Management\nArea")+
  viridis::scale_fill_viridis(discrete = TRUE)+
  scale_color_manual(values = c("blue","red"))
















MHWCOG_NWA_NEP %>%
  mutate(MHW = as.factor(MHW)) %>% 
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("NED","NEC","MAB",
                                                  "SAB","SAR","FEC",
                                                  "GOM","CAR","VN",
                                                  "CL","EK","MT"))) %>%
  mutate(prop_MHW_cells = if_else(is.na(prop_MHW_cells), 0, prop_MHW_cells)) %>% 
  ggplot()+
  geom_point(aes(x=sst_gradient,y=dis_per_area*1000000,
                 fill = prop_MHW_cells),shape = 21,size = 2,)+
  facet_wrap(~mgmt_zone, scale = "free_y")+
  stat_ellipse(aes(x=sst_gradient,y=dis_per_area*1000000,
                   color=MHW),size = 1)+
  cmocean::scale_fill_cmocean(name = "speed", direction = -1)+
  theme_bw()+
  labs(x="SST SD", y="Relative Distance x 10e5", fill = "MHW Size")




#lets normalize/scale the data so we can better interpret the LMM coefficients
all_COG_total<-MHWCOG_NWA_NEP %>% 
  na.omit() %>% 
  mutate(prop_MHW_cells = scale(prop_MHW_cells),
         sst_gradient = scale(sst_gradient),
         prop_habitat_cells = scale(prop_habitat_cells)) %>% 
  as.data.frame()

COG_glmm<-lmerTest::lmer(dis_per_area*1000000 ~ prop_MHW_cells + sst_gradient +
                           prop_habitat_cells +
                           (1|mgmt_zone),
                         data = all_COG_total)
summary(COG_glmm)
coefficients(COG_glmm)
  
  