#enhancing AIS with environmental data 

#loading in packages
library(rgdal)
library(rgeos)
library(tidyverse)
library(raster)
library(sf)
library(stars)
library(here)

#load in the presence-absence data if its not already in your global environment 
NWAPLL<-here("data","AIS_processed","NWA_PLL",
               "Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_v2_1to3ratio_absenceconstrained_convexhull_v2.rds") %>% readRDS()


#changing the heading name
NWAPLL<-NWAPLL %>% rename(Pres_abs = type)

NWAPLL<-NWAPLL %>% dplyr::select(-unique) #remove this column



####----enhancing AIS with NEP env data----####

HYCOM_NWA_dir<-"E:/HYCOM/"


#function to enhance AIS data
enhance_AIS<-function(input_df, env_dir){
  dates <-unique(input_df$date) %>% as.Date() #get unique dates from df
  enhanced_df<-data.frame()#need a empty df
  
  #for loop that subsets input df daily 
  #then enhances that data specific to the same date of a raster file
  #rbinds it all at the end
  for (i in 1:length(dates)){
    print(dates[i])
  day_subset<- filter(input_df, 
                      input_df$date==dates[i])
  
  day_subset<-sf::st_as_sf(day_subset, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  #bring in the env by same date
  bathy_file_list <- grep(list.files(path = paste0(env_dir,"bathy/"), full.names = TRUE, pattern = ".nc"), 
                          pattern = "bathy", value = TRUE)[1] %>% raster()
  
  sst_file_list <- grep(list.files(path = paste0(env_dir,"water_temp/"), full.names = TRUE, pattern = ".nc"), 
                        pattern = dates[i], value = TRUE)[1]
  
  if(is.na(sst_file_list)){
    next
  } 
  sst_file_list <-sst_file_list %>% raster() %>% rotate() %>% suppressWarnings()
  
  raster_extent<-matrix(raster::extent(sst_file_list))
  if(raster_extent[4,1]==50.04) {
    sst_file_list
  } else {
    n_ext<-extent(bathy_file_list)
    sst_file_list<-raster::shift(sst_file_list,dy=-0.02)
    sst_file_list<-extend(sst_file_list,n_ext)
    sst_file_list<-resample(sst_file_list,bathy_file_list)
  }
  
  sst_SD_file_list <- grep(list.files(path = paste0(env_dir,"water_temp/"), full.names = TRUE, pattern = ".nc"), 
                           pattern = dates[i], value = TRUE)[2]
  
  if(is.na(sst_SD_file_list)){
    next
  } 
  sst_SD_file_list <-sst_SD_file_list %>% raster() %>% rotate() %>% suppressWarnings()
  
  raster_extent<-matrix(raster::extent(sst_SD_file_list))
  if(raster_extent[4,1]==50.04) {
    sst_SD_file_list
  } else {
    n_ext<-extent(bathy_file_list)
    sst_SD_file_list<-raster::shift(sst_SD_file_list,dy=-0.02)
    sst_SD_file_list<-extend(sst_SD_file_list,n_ext)
    sst_SD_file_list<-resample(sst_SD_file_list,bathy_file_list)
  }
  
  ssh_file_list <- grep(list.files(path = paste0(env_dir,"surf_el/"), full.names = TRUE, pattern = ".nc"), 
                        pattern = dates[i], value = TRUE)[1]
  
  if(is.na(ssh_file_list)){
    next
  } 
  ssh_file_list <-ssh_file_list %>% raster() %>% rotate() %>% suppressWarnings()
  
  raster_extent<-matrix(raster::extent(ssh_file_list))
  if(raster_extent[4,1]==50.04) {
    ssh_file_list
  } else {
    n_ext<-extent(bathy_file_list)
    ssh_file_list<-raster::shift(ssh_file_list,dy=-0.02)
    ssh_file_list<-extend(ssh_file_list,n_ext)
    ssh_file_list<-resample(ssh_file_list,bathy_file_list)
  }
  
  ssh_SD_file_list <- grep(list.files(path = paste0(env_dir,"surf_el/"), full.names = TRUE, pattern = ".nc"), 
                           pattern = dates[i], value = TRUE)[2]
  
  if(is.na(ssh_SD_file_list)){
    next
  } 
  ssh_SD_file_list <-ssh_SD_file_list %>% raster() %>% rotate() %>% suppressWarnings()
  
  raster_extent<-matrix(raster::extent(ssh_SD_file_list))
  if(raster_extent[4,1]==50.04) {
    ssh_SD_file_list
  } else {
    n_ext<-extent(bathy_file_list)
    ssh_SD_file_list<-raster::shift(ssh_SD_file_list,dy=-0.02)
    ssh_SD_file_list<-extend(ssh_SD_file_list,n_ext)
    ssh_SD_file_list<-resample(ssh_SD_file_list,bathy_file_list)
  }
  
  n2_file_list <- grep(list.files(path = paste0(env_dir,"n2/"), full.names = TRUE, pattern = ".nc"), 
                       pattern = dates[i], value = TRUE)[1]
  
  if(is.na(n2_file_list)){
    next
  } 
  
  n2_file_list <-n2_file_list %>% raster() %>% rotate() %>% suppressWarnings()
  
  raster_extent<-matrix(raster::extent(n2_file_list))
  if(raster_extent[4,1]==50.04) {
    n2_file_list
  } else {
    n_ext<-extent(bathy_file_list)
    n2_file_list<-raster::shift(n2_file_list,dy=-0.02)
    n2_file_list<-extend(n2_file_list,n_ext)
    n2_file_list<-resample(n2_file_list,bathy_file_list)
  }
  
  eke_file_list <- grep(list.files(path = paste0(env_dir,"eke/"), full.names = TRUE, pattern = ".nc"), 
                        pattern = dates[i], value = TRUE)[1]
  
  if(is.na(eke_file_list)){
    next
  } 
  eke_file_list <-eke_file_list %>% raster() %>% rotate() %>% suppressWarnings()
  
  raster_extent<-matrix(raster::extent(eke_file_list))
  if(raster_extent[4,1]==50.04) {
    eke_file_list
  } else {
    n_ext<-extent(bathy_file_list)
    eke_file_list<-raster::shift(eke_file_list,dy=-0.02)
    eke_file_list<-extend(eke_file_list,n_ext)
    eke_file_list<-resample(eke_file_list,bathy_file_list)
  }
  
  ild_file_list <- grep(list.files(path = paste0(env_dir,"ild5/"), full.names = TRUE, pattern = ".nc"), 
                        pattern = dates[i], value = TRUE)[1]
  
  if(is.na(ild_file_list)){
    next
  } 
  ild_file_list <-ild_file_list %>% raster() %>% rotate() %>% suppressWarnings()
  
  raster_extent<-matrix(raster::extent(ild_file_list))
  if(raster_extent[4,1]==50.04) {
    ild_file_list
  } else {
    n_ext<-extent(bathy_file_list)
    ild_file_list<-raster::shift(ild_file_list,dy=-0.02)
    ild_file_list<-extend(ild_file_list,n_ext)
    ild_file_list<-resample(ild_file_list,bathy_file_list)
  }
  
  
  bathy_sd_file_list <- grep(list.files(path = paste0(env_dir,"bathy/"), full.names = TRUE, pattern = ".nc"), pattern = "bathy", value = TRUE)[2]
  
  dis2port_file_list <- grep(list.files(path = env_dir, full.names = TRUE, pattern = ".nc"), pattern = "port", value = TRUE)
  
  dis2seamount_file_list <- grep(list.files(path = env_dir, full.names = TRUE, pattern = ".nc"), pattern = "seamount", value = TRUE)
  
  env_file_list<-c(sst_file_list,
                   sst_SD_file_list,
                   ssh_file_list,
                   ssh_SD_file_list,
                   n2_file_list,
                   eke_file_list,
                   ild_file_list,
                   bathy_file_list,
                   bathy_sd_file_list,
                   dis2port_file_list,
                   dis2seamount_file_list)

  
  if(length(env_file_list)!=11){
    next
  } 
  
  env_day_stack<-stack(env_file_list)
  
  names(env_day_stack)<-c("SST","SST_SD","SSH","SSH_SD",
          "n2","EKE","ILD","z","z_SD","dist_port","dis_seamount")
  
  pts.env<-raster::extract(env_day_stack,day_subset)

  day.pts.env<-cbind(sf::st_coordinates(day_subset),day_subset,pts.env)%>% dplyr::select(-c("geometry","z")) %>% mutate(id=day_subset$id)
 
  enhanced_df<-rbind(enhanced_df,day.pts.env)
  }
  return(enhanced_df) #returns the fully enhanced df
}


NWAPLL2<-enhance_AIS(NWAPLL,HYCOM_NWA_dir) #run it!


#save it!
saveRDS(NWAPLL2,here("data","AIS_processed","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_v2_1to3ratio_absenceconstrained_convexhull_v2_enhanced.rds"))

NWAPLL2<-readRDS(here("data","AIS_processed","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_v2_1to3ratio_absenceconstrained_convexhull_v2_enhanced.rds"))



###clean up the data set
NWAPLL3<-na.omit(NWAPLL2)
#getting a 1:1 ratio
udates <-unique(NWAPLL3$date)

absences<-data.frame()
for (i in 1:length(udates)){
  subdate_presence<-filter(NWAPLL3, 
                           NWAPLL3$date== udates[i] & NWAPLL3$Pres_abs == 1)
  
  subdate_absences= NWAPLL3 %>%
    filter((NWAPLL3$Pres_abs == 0 & NWAPLL3$date == udates[i])) %>%
    .[sample(nrow(.),nrow(subdate_presence)),]
  
  absences<-rbind(absences,subdate_absences)
}

#subsetting the presences 
presence<-filter(NWAPLL3, NWAPLL3$Pres_abs == 1)    


table(presence$year,presence$Pres_abs)
table(absences$year,absences$Pres_abs) 

NWAPLL3_1to1ratio_w_env<-rbind(presence,absences)   

saveRDS(NWAPLL3_1to1ratio_w_env, here("data","AIS_processed","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull_v2_enhanced.rds"))
