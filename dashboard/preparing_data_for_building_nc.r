library(raster)
library(tidyverse)
library(sf)
library(here)
sf_use_s2(FALSE)# need to do this to remove spherical geometry
# NWA PLL --------------------------------------------------------------

## Nima generated year-mon mean habitat suitability maps for each mgmt_species in the CCS
## mgmt zone names

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

zone_names<-c("Caribbean", "Florida East Coast", "Gulf of Mexico",
              "Mid-Atlantic Bight", "Northeast Coastal", "Northeast Distant",
              "South Atlantic Bight", "Sargasso")
NWA_PLL_zones$ET_ID<-zone_names

#ordering the NWA PLL zones latitudinally
xy = st_coordinates(st_centroid(NWA_PLL_zones))
NWA_PLL_zones<-NWA_PLL_zones[order(-xy[,"Y"], xy[,"X"]),]

mgmtzone_vec <- NWA_PLL_zones$ET_ID


## build a time vector to specify the time points of the grids
time_vec <- seq.POSIXt(as.POSIXct('2012-01-01', tz='UTC'), as.POSIXct('2020-12-01', tz='UTC'), by='month')

#VDM stack monthly predictions
VDM_stack<-stack("E:/VDM_results/NWA_PLL/monthly/predictions/NWA_PLL_monthly_predictions_2012to2020.nc")

hab_list <- list(VDM_stack)
for (i in 1:length(mgmtzone_vec)){
  
  print(mgmtzone_vec[i])
  
  mgmtzone_crop<-raster::mask(VDM_stack, NWA_PLL_zones[i,])
  
  # ## list the grid files and isolate just the .grd files for the GFDL model (as an example)
  # fList <- list.files(paste0('~/Downloads/Grid_habitat/', mgmtzone_vec[i], '/'), full.names = TRUE)
  # fList <- fList[grep('GFDL', fList)]
  # fList <- fList[grep('.grd', fList)]
  
  ## the habitat suitability grids as input to build_nc must be a list
  ## where each element of the list is a stack corresponding to a mgmt_zone
  hab_list[[i+1]] <- mgmtzone_crop
  names(hab_list[[i]]) <- time_vec
}

## we also need SSTa 
fList <- list.files("E:/OISST/SSTa", full.names = TRUE, pattern = ".nc")
#fList <- fList[grep('GFDL', fList)]
#fList <- fList[grep('.grd', fList)]
# idx <- c(stringr::str_locate_all(fList[1], '//')[[1]][1,2] + 1,
#          stringr::str_locate_all(fList[1], '//')[[1]][1,2] + 4)

#these next lines are making a large rasterstack for SSTa and then reampling it to the same resolution as the VDM predictions
fList_years <- c()
for (i in 1:length(fList)) fList_years[i] <- substr(fList[i], 26, 29) 

ssta_stack <- raster::stack(fList[which(fList_years %in% lubridate::year(time_vec))])
names(ssta_stack) <- time_vec
ssta_stack <- raster::resample(ssta_stack, hab_list[[1]])

#here we want to subset each mgmt zone just like we did with the VDMs
ssta_list <- list(ssta_stack)
for (i in 1:length(mgmtzone_vec)){
  
  print(mgmtzone_vec[i])
  
  mgmtzone_crop<-raster::mask(ssta_stack, NWA_PLL_zones[i,])
  
  ssta_list[[i+1]]<- mgmtzone_crop
  names(ssta_list[[i]])<- time_vec
}

## get annual COG for the same time period for each mgmt_zone
NWAPLL_MHW_summer<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_summer.rds") %>% readRDS()
NWAPLL_MHW_winter<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_winter.rds") %>% readRDS()
pts<-rbind(NWAPLL_MHW_summer,NWAPLL_MHW_winter) %>% ungroup()

#need to fix the acronym for the zones
pts<-pts %>% 
  mutate(mgmt_zone = case_when(mgmt_zone == "MAB" ~ "Mid-Atlantic Bight",
                               mgmt_zone == "NEC" ~ "Northeast Coastal",
                               mgmt_zone == "NED" ~ "Northeast Distant",
                               mgmt_zone == "CAR" ~ "Caribbean",
                               mgmt_zone == "FEC" ~ "Florida East Coast",
                               mgmt_zone == "GOM" ~ "Gulf of Mexico",
                               mgmt_zone == "SAB" ~ "South Atlantic Bight",
                               mgmt_zone == "SAR" ~ "Sargasso"))

#pts <- data.table::fread('~/Downloads/hms_cc(v2).csv')
pts$date <- lubridate::parse_date_time(pts$date, tz='UTC', orders='mY')
pts <- pts %>% dplyr::select(COGx,COGy,date,mgmt_zone)%>% 
  bind_rows(All_areas_pts) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("All Management Areas",NWA_PLL_zones$ET_ID)))

##get the anomaly change df
NWAPLLa_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLLa_MHW_total.rds") %>%
  readRDS()

NWAPLLa_MHW_total$date <- lubridate::parse_date_time(NWAPLLa_MHW_total$date, tz='UTC', orders='mY')

NWAPLLa_MHW_total<-NWAPLLa_MHW_total %>% 
  mutate(mgmt_zone = case_when(mgmt_zone == "MAB" ~ "Mid-Atlantic Bight",
                               mgmt_zone == "NEC" ~ "Northeast Coastal",
                               mgmt_zone == "NED" ~ "Northeast Distant",
                               mgmt_zone == "CAR" ~ "Caribbean",
                               mgmt_zone == "FEC" ~ "Florida East Coast",
                               mgmt_zone == "GOM" ~ "Gulf of Mexico",
                               mgmt_zone == "SAB" ~ "South Atlantic Bight",
                               mgmt_zone == "SAR" ~ "Sargasso"))



change_df<-NWAPLLa_MHW_total %>% dplyr::select(NWA_PLL.area.anomaly,date,mgmt_zone) %>% 
  rename("habitat_change" = "NWA_PLL.area.anomaly") %>% 
  bind_rows(All_areas_change_df) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("All Management Areas",NWA_PLL_zones$ET_ID)))
  

MHW_df<-NWAPLLa_MHW_total %>% dplyr::select(MHW.x,date,mgmt_zone) %>% 
  rename("MHW" = "MHW.x")%>% 
  bind_rows(All_areas_MHW_df) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("All Management Areas",NWA_PLL_zones$ET_ID)))

ssta_ts_df<-NWAPLLa_MHW_total %>% dplyr::select(detrend.x,date,mgmt_zone) %>% 
  rename("ssta" = "detrend.x")%>% 
  bind_rows(All_areas_ssta_ts_df) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("All Management Areas",NWA_PLL_zones$ET_ID)))

seasonal_thresh_df<-NWAPLLa_MHW_total %>% dplyr::select(seas.x,date,mgmt_zone) %>% rename("seasonal_thresh" = "seas.x")%>% 
  bind_rows(All_areas_seasonal_thresh_df) %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("All Management Areas",NWA_PLL_zones$ET_ID)))

mgmtzone_vec <- c("All Management Areas",NWA_PLL_zones$ET_ID)

#REMEMBER to change the date below when saving
build_mhw_nc(ssta_list=ssta_list, hab_list = hab_list, time_vec = time_vec, 
             mgmt_vec = mgmtzone_vec, cog_pts=pts, change_df=change_df,
             MHW_df = MHW_df, ssta_ts_df = ssta_ts_df, 
             seasonal_thresh_df = seasonal_thresh_df,
             nc_file=here("dashboard","MHW_and_Fisheries_20220828.nc")) 

