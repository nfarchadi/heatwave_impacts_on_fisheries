#Daily predictions for VDMs

library(tidyverse)
library(raster)
library(foreach)
library(doParallel)
library(lunar)
library(ncdf4)
library(here)

#days from 2012 - 2020
udates<-seq(as.Date("2012-01-01"),as.Date("2020-12-31"),by="day")

#load in the model
brt<-here("data","VDM_and_eval","NWA_PLL","brt_hoursabove1_VDM_gbm_step.rds") %>% readRDS()


#parent directory of where all the env data are
HYCOM_NWA_dir<-"E:/HYCOM/"

#output directory
output_dir<-"E:/VDM_results/NWA_PLL/spatial_predictions/"

#function to predict VDMs daily
predict_NWA_PLL_VDM<-function(dates, model, env_dir, output_dir){
  
  #for loop to do this whole thing
  for (i in 1:length(dates)){
    
    #bring in the env by same date

   #bathy 
   bathy_file_list <- grep(list.files(path = paste0(env_dir,"bathy/"), full.names = TRUE, pattern = ".nc"), 
                            pattern = "bathy", value = TRUE)[1] %>% raster()
   #SST 
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
   
   #SST SD 
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
   
   #SSH 
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
    
    #SSH_SD
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
    
    #n2
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
    
    
    #z_SD
    bathy_sd_file_list <- grep(list.files(path = paste0(env_dir,"bathy/"), full.names = TRUE, pattern = ".nc"), pattern = "bathy", value = TRUE)[2]
    
    #dist_port
    dis2port_file_list <- grep(list.files(path = env_dir, full.names = TRUE, pattern = ".nc"), pattern = "port", value = TRUE)
    
    #dis_seamount
    dis2seamount_file_list <- grep(list.files(path = env_dir, full.names = TRUE, pattern = ".nc"), pattern = "seamount", value = TRUE)
    
    #lunar
    lunar <- sst_file_list
    lunar[] <- lunar::lunar.illumination(udates[i])
    
    env_file_list<-c(sst_file_list,
                     sst_SD_file_list,
                     ssh_file_list,
                     ssh_SD_file_list,
                     n2_file_list,
                     bathy_file_list,
                     bathy_sd_file_list,
                     dis2port_file_list,
                     dis2seamount_file_list,
                     lunar)
    
    
    if(length(env_file_list)!=10){
      next
    } 
    
    env_day_stack<-stack(env_file_list)
    
    print(dates[i]) #tells me it predicted this date
    
    names(env_day_stack)<-c("SST","SST_SD",
                            "SSH","SSH_SD",
                            "n2","z.1","z_SD",
                            "dist_port",
                            "dis_seamount",
                            "lunar")
    
    p <- raster::predict(env_day_stack, brt, n.trees = brt$gbm.call$best.trees, type = "response")
    
    writeRaster(p, paste0(output_dir,udates[i]), format='CDF', overwrite=TRUE)
  
  }
}


#lets predict!
predict_NWA_PLL_VDM(udates, brt, HYCOM_NWA_dir, output_dir)
  
  

  


####------plotting daily predicted rasters------####

#make a for loop to graph the results and making a gif
results_r<-list.files("E:/VDM_results/NWA_PLL/spatial_predictions/", pattern = ".nc")

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
  setwd("E:/VDM_results/NWA_PLL/spatial_predictions/")
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
         path = "E:/VDM_results/NWA_PLL/plots"
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
