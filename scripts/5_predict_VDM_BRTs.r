####------load packages------####
library(tidyverse)
library(raster)
library(foreach)
library(doParallel)
library(lunar)
library(ncdf4)

####------predicting BRTs------####

#1) load in the static variables 

z<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/ETOPO1/NWA_bathy_largerextent.nc")

z_sd <- raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/ETOPO1/NWA_bathysd_largerextent.nc")

dis_seamount<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance_from_seamount/dis_from_seamount_NWA_largerextent.nc")

dis_port<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NWA_dis_port_masked.nc")

#2)load in the dynamic variable file names

#list of the desire env files
#not including eke or ild5 because they were not used in the model building (i just commented them out)
home_dir<-"E:/HYCOM"

sst_files<-grep(list.files(path = paste(home_dir, '/water_temp', sep='')), pattern = "_sd", inv=TRUE, value = TRUE)

sst_sd_files<-list.files(path = paste(home_dir, '/water_temp', sep=''),pattern = "_sd")

#ild.5_files<-list.files(path = paste(home_dir, '/ild5', sep=''))

n2_files<-list.files(path = paste(home_dir, '/n2', sep=''))

ssh_files<-grep(list.files(path = paste(home_dir, '/surf_el', sep='')), pattern = "_sd", inv=TRUE, value = TRUE)

ssh_sd_files<-list.files(path = paste(home_dir, '/surf_el', sep=''),pattern = "_sd")

#eke_files<-list.files(path = paste(home_dir, '/eke', sep=''))


#3)load in the model
brt<-readRDS("E:/VDM_results/NWA_gbm_convexhull/brt_v2.rds")


#4) specify the dates to predict
udates <- as.Date(unlist(lapply(sst_files, FUN=function(x) substr(x, 12, 21))))
udates <- as.Date(grep(udates, pattern = "2012", inv = TRUE, value = TRUE)) #I don't want 2012 anymore

#5) predicting each day can be a lot so we are going to do this in parallel
cl<-makeCluster(4)
registerDoParallel(cl)

clusterCall(cl, function(x) .libPaths(x), .libPaths())

#foreach(i=udates, .packages = 'raster') %dopar% { only if you want to do it in parallel
for (i in 1:length(udates)) { 
  #SST
  setwd(paste(home_dir, '/water_temp', sep=''))
  SST <- raster::raster(sst_files[grep(paste('temp_', udates[i], sep=''), sst_files)])
  raster_extent<-matrix(raster::extent(SST))
  if(raster_extent[1,1]==-100.04) {
    SST
  } else {
    suppressWarnings(SST<-raster::rotate(SST))
  }
  if(raster_extent[4,1]==50.04) {
    SST
  } else {
    n_ext<-extent(z)
    SST<-raster::shift(SST,dy=-0.02)
    SST<-extend(SST,n_ext)
    SST<-resample(SST,z)
  }

  #SST_SD
  SST_SD <- raster::raster(sst_sd_files[grep(paste('temp_sd_',udates[i], sep=''), sst_sd_files)])
  raster_extent<-matrix(raster::extent(SST_SD))
  if(raster_extent[1,1]==-100.04) {
    SST
  } else {
    suppressWarnings(SST_SD<-raster::rotate(SST_SD))
  }
  if(raster_extent[4,1]==50.04) {
    SST_SD
  } else {
    n_ext<-extent(z)
    SST_SD<-shift(SST_SD,dy=-0.02)
    SST_SD<-extend(SST_SD,n_ext)
    SST_SD<-resample(SST_SD,z)
  }
  
  #SSH
  setwd( paste(home_dir, '/surf_el', sep=''))
  SSH <- raster::raster(ssh_files[grep(paste('ssh_',udates[i], sep=''), ssh_files)])
  raster_extent<-matrix(raster::extent(SSH))
  if(raster_extent[1,1]==-100.04) {
    SST
  } else {
    suppressWarnings(SSH<-raster::rotate(SSH))
  }
  if(raster_extent[4,1]==50.04) {
    SSH
  } else {
    n_ext<-extent(z)
    SSH<-shift(SSH,dy=-0.02)
    SSH<-extend(SSH,n_ext)
    SSH<-resample(SSH,z)
  }
  
  
  #SSH_SD
  SSH_SD <- raster::raster(ssh_sd_files[grep(paste('ssh_sd_',udates[i], sep=''), ssh_sd_files)])
  raster_extent<-matrix(raster::extent(SSH_SD)) 
  if(raster_extent[1,1]==-100.04) {
    SST
  } else {
    suppressWarnings(SSH_SD<-raster::rotate(SSH_SD))
  }
  if(raster_extent[4,1]==50.04) {
    SSH_SD
  } else {
    n_ext<-extent(z)
    SSH_SD<-shift(SSH_SD,dy=-0.02)
    SSH_SD<-extend(SSH_SD,n_ext)
    SSH_SD<-resample(SSH_SD,z)
  }
  
  #n2
  setwd(paste(home_dir, '/n2', sep=''))
  n2 <- raster::raster(n2_files[grep(paste('n2_',udates[i], sep=''), n2_files)])
  raster_extent<-matrix(raster::extent(n2))
  if(raster_extent[1,1]==-100.04) {
    SST
  } else {
    suppressWarnings(n2<-raster::rotate(n2))
  }
  if(raster_extent[4,1]==50.04) {
    n2
  } else {
    n_ext<-extent(z)
    n2<-shift(n2,dy=-0.02)
    n2<-extend(n2,n_ext)
    n2<-resample(n2,z)
  }
  
  
  #lunar
  lunar <- SST
  lunar[] <- lunar::lunar.illumination(udates[i])

  s<-stack(z,z_sd,dis_port,dis_seamount,SST,SSH,SST_SD,SSH_SD, n2, lunar)


  names(s)<-c("z","z_sd","dis_port", "dis_seamount","SST","SSH","SST_SD","SSH_SD", "n2", "lunar")

  p <- raster::predict(s, brt, n.trees = brt$gbm.call$best.trees, type = "response") #this part is finicky when trying to do it in parallel. if not working try switching to gbm:predict.gbm, run it again, and then switch back
 
  writeRaster(p, paste("E:/VDM_results/NWA_gbm_convexhull/Spatial_Predictions_v2/",udates[i], sep=''), format='CDF', overwrite=TRUE)
}



####------plotting daily predicted rasters------####

#make a for loop to graph the results and making a gif
results_r<-list.files("E:/VDM_results/NWA_gbm_convexhull/Spatial_Predictions_v2", pattern = ".nc")

library(PNWColors)
library(raster)
library(RStoolbox)
library(ggplot2)
library(sf) #for mapping
library(rnaturalearth)#package that provides data of the world. Used for mapping`

world <- ne_countries(scale = "medium", returnclass = "sf")

## general plot setup

par(mar=c(4, 4, 1, 1))
pal<-pnw_palette("Bay",n=3)

for (i in 1:length(results_r)){
  setwd("E:/VDM_results/NWA_gbm_convexhull/Spatial_Predictions_v2/")
  r<-raster(results_r[i])
  #r<-rotate(r)
  #r_Stack<-addLayer(r_Stack, r)
  #animate(r_Stack, pause=0.25) r_Stack
  plot_raster<-ggR(r, geom_raster = TRUE)+ 
    theme_classic()+ggtitle(paste(substr(results_r[i], 1, 10)))+
    xlab("Lon")+ylab("Lat")+ labs(fill = "Habitat \nSuitability") + geom_sf(data = world, color= "black", fill = "grey")+
    coord_sf(xlim = c(-100,-30), ylim = c(10,50), expand = FALSE)+
    scale_fill_gradientn(colors = pal)
  
  fname <- paste(substr(results_r[i], 1, 3), substr(results_r[i], 4, 10),".png", sep='')
  
  ggsave(plot_raster,
         path = "E:/VDM_results/NWA_gbm_convexhull/Plots_v2/"
         , filename = fname,
         width = 9, height = 6, units = 'in')
  
} 


#-----------now to make a animated gif from the pngs-----------#
library(purrr)
library(magick)


VDM_image_read<- grep(list.files(path = "E:/VDM_results/NWA_gbm_convexhull/Plots_v2/mp4", pattern = "*.png", full.names = T), pattern = "2013", value = TRUE) 

VDM_image_read<-VDM_image_read[91:303]

VDM_image_read %>% 
  map(image_read) %>%  # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write_gif("E:/VDM_results/NWA_gbm_convexhull/Plots_v2/mp4/VDM_2013_animation3.gif") # write to current dir





#####---------calculating the mean_centre------------########
library(aspace)
library(raster)

results_files<-list.files("D:/Env_rasters/HYCOM/VDM_results/BRT_VDM_LR0_08_results",pattern = ".nc")
udates <- as.Date(unlist(lapply(results_files, FUN=function(x) substr(x, 4, 13))))

VDM_points<-data.frame()

for (i in 1:length(udates)){
  #SST
  setwd("D:/Env_rasters/HYCOM/VDM_results/BRT_VDM_LR0_08_results")
  Daily_VDM <- raster::raster(results_files[grep(paste('VDM', udates[i], sep=''), results_files)])
  Daily_VDM <-rasterToPoints(Daily_VDM)
  Daily_VDM <-as.data.frame(Daily_VDM)
  date<-rep(udates[i], length(Daily_VDM$layer))
  Daily_VDM<-cbind(Daily_VDM,date)
  Daily_VDM<-subset(Daily_VDM, layer >=0.5)#only want to get the suitability points above 0.5 ------ Try >=0.75 (white et al. used this thresold)
  VDM_points<-rbind(VDM_points,Daily_VDM)
}



setwd("C:/Users/nfarc/Desktop")#setting the wd to desktop so ill be reminded to delete the txt file the mean_centre function produces


## set up the function to calculate mean, weighed center for each year
mean_cen_fun <- function(x, print=TRUE) {
  w <- x$layer # with your suitability value
  m <- mean_centre(id=1, filename="mean_centre_Output.txt",weighted=TRUE, weights=w, points=x[,c(1:2)])  ### points will be long, lat in utm
  longlat <- m[,c(2:3)] ## pull out just the long and lat
  data.frame(longlat)
}

### run the function for each day separately
library(plyr)
Dailycenters <- dlply(VDM_points, .(VDM_points$date), mean_cen_fun)

#rbind the list of dataframes
Dailycenters_2 <- do.call(rbind, llply(Dailycenters))
Dailycenters_2
Dailycenters_2$date <- rownames(Dailycenters_2)

Dailycenters_2$date<-as.Date(Dailycenters_2$date)
Dailycenters_2$month<-lubridate::month(Dailycenters_2$date)
ggplot(Dailycenters_2, aes(date,CENTRE.y, 
                           color=as.factor(month)))+geom_point()+
         theme_classic()+xlab("Date")+ylab("Lat")+labs(color="Month")
