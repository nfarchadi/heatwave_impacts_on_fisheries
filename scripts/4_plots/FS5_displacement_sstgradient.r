# Fig. S5. Spatial variability in fleet displacement in relation to SST gradient

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


####
#NWA
####

#############################
#load in mgmt area shapefiles
#############################

#mangement areas shapefile
NWA_PLL_areas<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

#trying to clip the management areas to the coast
land<-here("data","shapefiles","Land","GSHHS_l_L1.shp") %>% st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_areas<-st_difference(NWA_PLL_areas, st_union(st_combine(land)))


NWA_PLL_areas$ET_ID<-factor(NWA_PLL_areas$ET_ID,
                            levels = c("NED","NEC","MAB",
                                       "SAB","SAR","FEC",
                                       "GOM","CAR"))

####
#NWA
####

################################
#shift / total area in mgmt area
################################
NWAPLL_CFG_MHW_total<-here("data","mgmt_area_metrics","NWA_PLL",
                           "NWAPLL_CFG_MHW_total.rds") %>% 
  readRDS()

month_avgCOG<-NWAPLL_CFG_MHW_total %>% filter(MHW == 0) %>% 
  group_by(mgmt_area,month) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG_NWA<-NWAPLL_CFG_MHW_total %>% 
  dplyr::select(MHW,month,event_index,duration_months, 
                mgmt_area,COGx,COGy,prop_MHW_area,
                mean_SSTa,date,prop_habitat_cells, 
                prop_MHW_cells, sequence_months) %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326")


MHWCOG_NWA$distance <- purrr::map_dbl(1:nrow(MHWCOG_NWA), function(x){
  x <- MHWCOG_NWA[x,]
  y <- month_avgCOG[month_avgCOG$mgmt_area == x$mgmt_area & month_avgCOG$month == x$month,]
  st_distance(x, y)
})


MHWCOG_NWA<-MHWCOG_NWA %>% mutate(distance = distance * 0.001,
                                  lon = sf::st_coordinates(.)[,1],
                                  lat = sf::st_coordinates(.)[,2])

mgmt_area_area<-st_area(NWA_PLL_areas)
units(mgmt_area_area)<-NULL #remove the units

area_df<-data.frame(mgmt_area = NWA_PLL_areas$ET_ID %>% unique(),area = mgmt_area_area*0.000001) #converting to km^2

MHWCOG_NWA<-left_join(MHWCOG_NWA, area_df, by=c("mgmt_area")) %>% 
  mutate(dis_per_area = distance / area)


###########################
#sst_sd (i.e. sst gradient)
###########################

#function to calculate landscape metrics per mgmt area
mgmt_area_sstsd<-function(oisst,cog){
  
  mgmtarea_sst_sd_list<-list()
  
  # mgmtarea<-oisst %>% dplyr::select(ET_ID) %>% unique() %>% pull()
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
                                mgmt_area=cog_date %>% as.data.frame() %>% 
                                  dplyr::select(mgmt_area),
                                date=cog_date %>% as.data.frame() %>% 
                                  dplyr::select(date))
    
    mgmtarea_sst_sd_list[[length(mgmtarea_sst_sd_list)+1]]=sst_gradient_df
      
  }
  return(mgmtarea_sst_sd_list)
}


north_oisst<-here("data","oisst","NWA_PLL","MAB_NEC_NED_MHW.rds") %>% readRDS()

north_sst_gradient<-mgmt_area_sstsd(north_oisst,MHWCOG_NWA)

north_sst_gradient<-bind_rows(north_sst_gradient) %>% na.omit()
rm(north_oisst)


south_oisst<-here("data","oisst","NWA_PLL","GOM_CAR_FEC_SAR_SAB_MHW.rds") %>% readRDS()

south_sst_gradient<-mgmt_area_sstsd(south_oisst,MHWCOG_NWA)

south_sst_gradient<-bind_rows(south_sst_gradient) %>% na.omit()
rm(south_oisst)

sst_gradient<-rbind(north_sst_gradient,south_sst_gradient)

rm(north_sst_gradient,south_sst_gradient)

MHWCOG_NWA<-left_join(MHWCOG_NWA, sst_gradient, by=c("mgmt_area","date",
                                                     "lon","lat"))





#####
#NEP
#####



#############################
#load in mgmt area shapefiles
#############################

#mangement areas shapefile
NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

#trying to clip the management areas to the coast
land<-here("data","shapefiles","Land","GSHHS_l_L1.shp") %>% st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NEP_TROLL_areas<-st_difference(NEP_TROLL_areas, st_union(st_combine(land)))

NEP_TROLL_areas$ET_ID<-factor(NEP_TROLL_areas$ET_ID,
                              levels = c("VN","CL","EK",
                                         "MT","CP"))

#subsetting shapefile
NEP_TROLL_areas_subset<-NEP_TROLL_areas %>% 
  filter(ET_ID %in% c("VN","CL","EK","MT"))

################################
#shift / total area in mgmt area
################################

NEPTROLL_CFG_MHW_total<-here("data","mgmt_area_metrics","NEP_TROLL",
                         "NEPTROLL_CFG_MHW_total.rds") %>% 
  readRDS()


month_avgCOG<-NEPTROLL_CFG_MHW_total %>% filter(MHW == 0) %>% 
  group_by(mgmt_area,month) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG_NEP<-NEPTROLL_CFG_MHW_total %>% 
  dplyr::select(MHW,month,event_index,duration_months, 
                mgmt_area,COGx,COGy,prop_MHW_area,
                mean_SSTa,date,prop_habitat_cells, 
                prop_MHW_cells, sequence_months) %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326")


MHWCOG_NEP$distance <- map_dbl(1:nrow(MHWCOG_NEP), function(x){
  x <- MHWCOG_NEP[x,]
  y <- month_avgCOG[month_avgCOG$mgmt_area == x$mgmt_area & month_avgCOG$month == x$month,]
  st_distance(x, y)
})


MHWCOG_NEP<-MHWCOG_NEP %>% mutate(distance = distance * 0.001,
                                  lon = sf::st_coordinates(.)[,1],
                                  lat = sf::st_coordinates(.)[,2])

mgmt_area_area<-st_area(NEP_TROLL_areas)
units(mgmt_area_area)<-NULL #remove the units

area_df<-data.frame(mgmt_area = NEP_TROLL_areas$ET_ID %>% unique(),area = mgmt_area_area*0.000001) #converting to km^2

MHWCOG_NEP<-left_join(MHWCOG_NEP, area_df, by=c("mgmt_area")) %>% 
  mutate(dis_per_area = distance / area)


MHWCOG_NEP<-MHWCOG_NEP %>% 
  filter(mgmt_area %in% c("VN","CL","EK", "MT")) %>% 
  mutate(mgmt_area == factor(mgmt_area,
                             levels = c("VN","CL","EK",
                                        "MT","CP")))



NEP_oisst<-here("data","oisst","NEP_TROLL","CP_MT_EK_CL_VN_MHW.rds") %>% 
  readRDS() 


NEP_sst_gradient<-mgmt_area_sstsd(NEP_oisst,MHWCOG_NEP)

NEP_sst_gradient<-bind_rows(NEP_sst_gradient) %>% na.omit()
rm(NEP_oisst)

MHWCOG_NEP<-left_join(MHWCOG_NEP, NEP_sst_gradient, by=c("mgmt_area","date",
                                                         "lon","lat"))



#combine and plot
MHWCOG_NWA_NEP<-rbind(MHWCOG_NWA %>% 
                        dplyr::select(dis_per_area,sst_gradient,
                                      sst_gradient_mean,sst_gradient_sd,
                                      MHW,mgmt_area,
                                      prop_MHW_cells, mean_SSTa,lat,lon,
                                      prop_MHW_area, prop_habitat_cells),
                      MHWCOG_NEP %>% 
                        dplyr::select(dis_per_area,sst_gradient,
                                      sst_gradient_mean,sst_gradient_sd,
                                      MHW,mgmt_area,
                                      prop_MHW_cells, mean_SSTa,lat,lon,
                                      prop_MHW_area, prop_habitat_cells))

cols <- c("NED" = "darkorange3", "NEC" = "darkorange2", 
          "MAB" = "darkorange1", "SAB" = "lightgoldenrod3", 
          "SAR" = "lightgoldenrod2","FEC" = "lightgoldenrod1",
          "CAR" = "lightgoldenrod", "VN" = "seagreen4",
          "CL" = "seagreen3", "EK" = "seagreen2",
          "MT" = "seagreen1")


FS5_displacement_sstgradient<-MHWCOG_NWA_NEP %>%
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>%  
  mutate(mgmt_area = factor(mgmt_area, levels = c("NED","NEC","MAB",
                                                  "SAB","SAR","FEC",
                                                  "GOM","CAR","VN",
                                                  "CL","EK","MT"))) %>%
  na.omit() %>% 
  ggplot()+
  geom_point(aes(x=sst_gradient,y=dis_per_area*1000000,
                 fill = mgmt_area),shape = 21, size = 3)+
  stat_ellipse(aes(x=prop_habitat_cells,y=dis_per_area*1000000,
                   color=MHW),size = 1.5)+
  theme_bw()+theme()+
  labs(x="SST Gradient", y="Relative Distance x 10e5", fill = "Mangement Areas") +
  scale_fill_manual(values = cols)
  

ggsave(here("Plots","FS5_displacement_sstgradient.png"),
       width = 7, height = 6, units = "in", dpi = 300)
ggsave(here("Plots","FS5_displacement_sstgradient.svg"),
       width = 7, height = 6, units = "in", dpi = 300)

  