#combining all the environmentale hycom data in 1 file per day

library(raster)
library(here)
library(tidyverse)

###############################################
# combine all env variables into 1 file per day
###############################################

## load in bathhymetry and bathymetry SD (rugosity) raster
bathy <- here("data","static","NEP_TROLL","NEP_bathy.nc")
rugosity <- here("data","static","NEP_TROLL","NEP_bathysd.nc")

# start and end dates at daily resolution
st_date<-as.Date("2012-01-02")
en_date<-as.Date("2012-01-02")
udates <-seq(st_date,en_date, by = "day")

# For each day combining all env variables into 1 file
for (i in 1:length(udates)) {
  ## water u
  water_u <- list.files(path = here("data","hycom","NEP_TROLL"),
                        full.names=TRUE) %>% 
    grep(pattern = paste0("u_",udates[i]),value = TRUE) %>% raster()
  
  ## water v
  water_v <- list.files(path = here("data","hycom","NEP_TROLL"),
                        full.names=TRUE) %>% 
    grep(pattern = paste0("v_",udates[i]),value = TRUE) %>% raster()
  
  ## ild
  ild <- list.files(path = here("data","hycom","NEP_TROLL"),
                    full.names=TRUE) %>% 
    grep(pattern = paste0("ild5_",udates[i]),value = TRUE) %>% raster()
  
  ## n2
  n2 <- list.files(path = here("data","hycom","NEP_TROLL"),
                   full.names=TRUE) %>% 
    grep(pattern = paste0("n2_",udates[i]),value = TRUE) %>% raster()
  
  ## sst_sd
  sst_sd <- list.files(path = here("data","hycom","NEP_TROLL"),
                       full.names=TRUE) %>% 
    grep(pattern = paste0("SST_SD_",udates[i]),value = TRUE) %>% raster()
  
  ## sst
  sst <- list.files(path = here("data","hycom","NEP_TROLL"),
                        full.names=TRUE) %>% 
    grep(pattern = paste0("temp_",udates[i]),value = TRUE) %>% raster()
  
  ## ssh_sd
  ssh_sd <- list.files(path = here("data","hycom","NEP_TROLL"),
                       full.names=TRUE) %>% 
    grep(pattern = paste0("SSH_SD_",udates[i]),value = TRUE) %>% raster()
  
  ## ssh
  ssh <- list.files(path = here("data","hycom","NEP_TROLL"),
                       full.names=TRUE) %>% 
    grep(pattern = paste0("ssh_",udates[i]),value = TRUE) %>% raster()
  
  ## eke
  eke <- list.files(path = here("data","hycom","NEP_TROLL"),
                       full.names=TRUE) %>% 
    grep(pattern = paste0("EKE_",udates[i]),value = TRUE) %>% raster()
  
  ## salinity
  salinity <- list.files(path = here("data","hycom","NEP_TROLL"),
                       full.names=TRUE) %>% 
    grep(pattern = paste0("sal_",udates[i]),value = TRUE) %>% raster()
  
  
  combine<-stack(water_u, water_v, ild, n2, sst_sd, sst, ssh_sd, ssh, eke, 
  salinity, bathy, rugosity)
  
  names(combine)<-c("water_u", "water_v"," ild", "n2", "sst_sd", "sst", 
                    "ssh_sd", "ssh", "eke", "salinity", "bathy", "rugosity")
  
  #file name
  fname<-paste0("hycom_combine_",udates[i],".grd")
  
  raster::writeRaster(combine, filename = here("data","hycom","NEP_TROLL", 
                                               "hycom_combine", fname), 
                      overwrite=T)
  
  
}
