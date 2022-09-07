#Daily predictions for VDMs

library(tidyverse)
library(raster)
library(lunar)
library(ncdf4)
library(here)
library(PNWColors)
library(RStoolbox)
library(sf) #for mapping
library(rnaturalearth)#package that provides data of the world. Used for mapping`
world <- ne_countries(scale = "medium", returnclass = "sf")

#days from 2012 - 2020 only during fishing season Jun - Oct
udates<-seq(as.Date("2019-09-03"),as.Date("2020-11-30"),by="day") %>% subset(format.Date(., "%m") %in% c("05", "06","07","08","09","10", "11"))

#load in the model
brt<-here("data","VDM_and_eval","NEP_TROLL","brt_hoursabove1_VDM_gbm_step CCS_filtered_w_bathyrugosity_lr0.03_treecomp7.rds") %>% readRDS()


#parent directory of where all the env data are
HYCOM_NEP_dir<-"E:/HYCOM_NEP/"

#output directory
output_dir<-"E:/VDM_results/NEP_TROLL/spatial_predictions/"

#function to predict VDMs daily
predict_NEP_TROLL_VDM<-function(dates, model, env_dir, output_dir){
  
  #for loop to do this whole thing
  for (i in 1:length(dates)){
    
    #bring in the env by same date
    env<-grep(list.files(path = env_dir, full.names = TRUE, pattern = ".grd"),
         pattern = udates[i], value = TRUE)
    
    if(length(env)==0){
      next
    }
    
    env<-stack(env)
    
    #only want n2, sst_sd, sst, ssh_sd, ssh, bathy, and rugosity
    env<-env[[c(4:8,11:12)]] 
   
    #dist_port
    dis2port_file_list <- grep(list.files(path = env_dir, full.names = TRUE, pattern = ".nc"), pattern = "port", value = TRUE)
    
    #dis_seamount
    dis2seamount_file_list <- grep(list.files(path = env_dir, full.names = TRUE, pattern = ".nc"), pattern = "seamount", value = TRUE)
    
    #lunar
    lunar <- env[[1]]
    lunar[] <- lunar::lunar.illumination(udates[i])
    
    env_day_stack<-stack(env,
                         dis2port_file_list,
                         dis2seamount_file_list,
                         lunar)
    
    
    if(length(names(env_day_stack))!=10){
      next
    } 
    
    print(dates[i]) #tells me it predicted this date
    
    names(env_day_stack)<-c("n2","sst_sd",
                            "sst","ssh_sd",
                            "ssh","bathy","rugosity",
                            "dis_port",
                            "dis_seamount",
                            "lunar")
    
    p <- raster::predict(env_day_stack, brt, n.trees = brt$gbm.call$best.trees, type = "response")
    
    writeRaster(p, paste0(output_dir,udates[i]), format='CDF', overwrite=TRUE)
  
  }
}


#lets predict!
predict_NEP_TROLL_VDM(udates, brt, HYCOM_NEP_dir, output_dir)
  
  

  


####------plotting daily predicted rasters------####

# #get logbook data to compare
# Processed_ALBTrollLogbooks_2009_2019 <- readRDS("C:/Users/nfarc/Desktop/Grad Work SDSU/Chapter1_VDMheatwaves/heatwave_impacts_on_fisheries/data/AIS_processed/NEP_TROLL/ALB_Logbook/Processed_ALBTrollLogbooks_2009_2019.rds")
# 
# #remove land from logbook data
# bathy_file<-"E:/HYCOM_NEP/hycom_combine_2012-01-01.grd"
# 
# remove_land<- function(input_df, bathy_file){
#   AIS_df<-input_df
#   AIS_df_2<-AIS_df
#   AIS_df_2<-sf::st_as_sf(AIS_df_2, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
# 
#   bathy<-raster::raster(bathy_file, band = 11)
# 
#   pts.bathy<-raster::extract(bathy,AIS_df_2)
#   fleet.bathy<-data.frame(cbind(sf::st_coordinates(AIS_df_2),pts.bathy))
# 
#   variable<-"z"
#   AIS_df[,variable]<-fleet.bathy[,c(3)]
#   AIS_df <- AIS_df[which(AIS_df$z < 0),]#only bathy less than
# 
#   return(AIS_df)
# }
# 
# Processed_ALBTrollLogbooks_2009_2019 <- remove_land(Processed_ALBTrollLogbooks_2009_2019, bathy_file = bathy_file)
# 
# Processed_ALBTrollLogbooks_2009_2019<-Processed_ALBTrollLogbooks_2009_2019 %>%
#   mutate(yearmon = zoo::as.yearmon(date)) %>%
#   group_by(lon,lat,yearmon) %>%
#   summarise(count = n(), .groups = "drop")

#compare with AIS data
NEP_TROLL<-here("data","AIS_processed","NEP_TROLL","Pres_Abs_2013to2020_NWA_USA_TROLL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull_v2_enhanced.rds") %>% readRDS() %>% as.data.frame() %>% na.omit()


NEP_TROLL_pres<-NEP_TROLL %>% filter(Pres_abs == 1) %>%
  mutate(yearmon = zoo::as.yearmon(date)) %>%
  group_by(X,Y,yearmon) %>%
  summarise(count = n(), .groups = "drop")


#make a for loop to graph the results and making a gif
results_r<-list.files("E:/VDM_results/NEP_TROLL/spatial_predictions/", pattern = ".nc", full.names = TRUE)


## general plot setup

par(mar=c(4, 4, 1, 1))
pal<-pnw_palette("Bay",n=3)

for (i in 1:length(results_r)){
  r<-raster(results_r[i])
  # logbook_subset<-Processed_ALBTrollLogbooks_2009_2019 %>%
  #   filter(yearmon == zoo::as.yearmon(substr(results_r[i], 59, 68)))
  AIS_subset<-NEP_TROLL_pres %>%
    filter(yearmon == zoo::as.yearmon(substr(results_r[i], 46, 55)))
  plot_raster<-ggR(r, geom_raster = TRUE)+ 
    theme_classic()+ggtitle(paste(substr(results_r[i], 46, 55)))+
    geom_point(data = AIS_subset,
               aes(x = X, y = Y, size = count), pch = 21, color = "darkmagenta")+
    scale_size_continuous(range = c(1,5)) +
    xlab("Lon")+ylab("Lat")+ labs(fill = "Habitat \nSuitability",
                                  size = "Monthly AIS \nOccurance") + geom_sf(data = world, color= "black", fill = "grey")+
    coord_sf(xlim = c(-135,-117), ylim = c(32.5,53.5), expand = TRUE)+
    scale_fill_gradientn(colors = pal)
  plot_raster
  fname <- paste(substr(results_r[i], 46, 55),".png", sep='')
  
  ggsave(plot_raster,
         path = "E:/VDM_results/NEP_TROLL/plots"
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
  #setwd("D:/Env_rasters/HYCOM/VDM_results/BRT_VDM_LR0_08_results")
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
