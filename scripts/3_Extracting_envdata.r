#extracting (enhancing) environmental data for AIS data

#loading in packages
library(rgdal)
#Extracting the static variables (e.g. bathymetry, bathymetry SD, distance to ports, distance to seamounts) for each location
library(rgeos)
library(sp)
library(tidyverse)
library(dplyr)
library(raster)
library(spdplyr)
library(sf)

#load in the presence-absence data if its not already in your global environment 
Pres_Abs_2013to2020<-readRDS("C:/Users/nfarc/Desktop/NASA_FaCeT/Data/1_Processed_AIS/Pres_Abs_2013to2020_NWA_USA_LL_onlyfishing_v2.rds")

#check and cleaning the data
head(Pres_Abs_2013to2020)
#changing the heading name
Pres_Abs_2013to2020<-Pres_Abs_2013to2020 %>% rename(Pres_abs = type)
#looking to the ratio between 1 and 0 for each year
table(Pres_Abs_2013to2020$year, Pres_Abs_2013to2020$Pres_abs)
Pres_Abs_2013to2020<-Pres_Abs_2013to2020[,-11]
Pres_Abs_2013to2020<-Pres_Abs_2013to2020[,c(12,1,2,3,4,5,6,7,8,9,11,10)]

#these dates the env were corrupt so I am taking them out now
st_date<-as.Date("2018-11-21")
en_date<-as.Date("2018-12-04")
removedates <-seq(st_date,en_date, by = "day")
removedates<- append(removedates, as.Date(c("2013-08-19","2014-04-05",
                                            "2014-04-06", "2016-03-22",
                                            "2016-04-10", "2016-04-24",
                                            "2018-11-03","2019-09-04",
                                            "2019-09-05","2019-09-07",
                                            "2019-09-09","2019-09-10",
                                            "2019-09-13","2019-09-14",
                                            "2020-11-12","2020-11-13",
                                            "2020-11-14","2020-11-15",
                                            "2020-11-16","2020-11-17",
                                            "2020-11-18","2020-11-24",
                                            "2020-12-08","2020-12-10",
                                            "2017-09-06", "2017-12-16",
                                            "2019-08-09")))


Pres_Abs_2013to2020<-Pres_Abs_2013to2020[!Pres_Abs_2013to2020$date %in% removedates,]

#saving this in case for later
saveRDS(Pres_Abs_2013to2020,"C:/Users/nfarc/Desktop/NASA_FaCeT/Data/1_Processed_AIS/Pres_Abs_2013to2020_NWA_USA_LL_onlyfishing_v2.rds")



## Extracting static variables


#------extracting bathymetry data------#

#want to stack each of these two rasters
bathy_NWA_extent<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/ETOPO1/NWA_bathy_largerextent.nc") 

bathySD_NWA_extent<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/ETOPO1/NWA_bathysd_largerextent.nc")

bathy.stack<-raster::stack(bathy_NWA_extent,bathySD_NWA_extent)

#need to define dataframe to be a spatial class
Pres_Abs_2013to2020_ex<-Pres_Abs_2013to2020
coordinates(Pres_Abs_2013to2020_ex)<-c("lon", "lat")
#have to make a Pres_Abs_2013to2020_ex the same projection as bathy
projection(Pres_Abs_2013to2020_ex)<-"+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

pts.bathy<-raster::extract(bathy.stack,Pres_Abs_2013to2020_ex)
NWA_longline.bathy<-data.frame(cbind(coordinates(Pres_Abs_2013to2020_ex),pts.bathy))


Pres_Abs_2013to2020[,c(length(Pres_Abs_2013to2020),length(Pres_Abs_2013to2020)+1)]<-NWA_longline.bathy[,c(3:4)]
colnames(Pres_Abs_2013to2020)[(length(Pres_Abs_2013to2020)-1):(length(Pres_Abs_2013to2020))]<-c("z", "z_sd")
Pres_Abs_2013to2020 <- Pres_Abs_2013to2020[which(Pres_Abs_2013to2020$z < 0),]#only bathy less than 0



#------extracting distance from port variables------#

#There are 3 type of distance to port rasters. 
#1) normal distance to port within the study extent
#2) distance to port raster that has a threshold of the max distance (2190 km)
#3) distance to port raster that has a threshold of the mean distance (403 km)

dis_port<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NWA_dis_port_masked.nc")

#not adding the max or mean right now because previous analyses did not show that is contributed much
#dis_port_max<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NWA_dis_port_thresholdMax.nc")

#dis_port_mean<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NWA_dis_port_thresholdMean.nc")

#dis_port.stack<-stack(dis_port,dis_port_max,dis_port_mean)

Pres_Abs_2013to2020_ex<-Pres_Abs_2013to2020
coordinates(Pres_Abs_2013to2020_ex)<-c("lon", "lat")
#have to make a NWA_XXX_ex the same projection as bathy
projection(Pres_Abs_2013to2020_ex)<-"+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0"

#pts.ports<-raster::extract(dis_port.stack,Pres_Abs_2013to2020_ex)
#NWA_longline.ports<-data.frame(cbind(coordinates(Pres_Abs_2013to2020_ex),pts.ports))
#Pres_Abs_2013to2020[,c(14:16)]<-NWA_longline.ports[,c(3:5)]
#colnames(Pres_Abs_2013to2020)[14:16]<-c("dis_port","dis_port_max", "dis_port_mean")

pts.ports<-raster::extract(dis_port,Pres_Abs_2013to2020_ex)
NWA_longline.ports<-data.frame(cbind(coordinates(Pres_Abs_2013to2020_ex),pts.ports))
Pres_Abs_2013to2020[,length(Pres_Abs_2013to2020)+1]<-NWA_longline.ports[,c(3)]
colnames(Pres_Abs_2013to2020)[length(Pres_Abs_2013to2020)]<-"dis_port"



#------extracting distance from seamounts------#

seamount_dis<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance_from_seamount/dis_from_seamount_NWA_largerextent.nc")

Pres_Abs_2013to2020_ex<-Pres_Abs_2013to2020 #making a copy
#making the copy a spatial dataframe
coordinates(Pres_Abs_2013to2020_ex)<-c("lon", "lat")
#have to make a NWA_XXX_ex the same projection as seamount dis
projection(Pres_Abs_2013to2020_ex)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

pts.seamount_dis<-raster::extract(seamount_dis,Pres_Abs_2013to2020_ex)
NWA_longline.seamount_dis<-data.frame(cbind(coordinates(Pres_Abs_2013to2020_ex),pts.seamount_dis))
Pres_Abs_2013to2020[,length(Pres_Abs_2013to2020)+1]<-NWA_longline.seamount_dis[,c(3)]
colnames(Pres_Abs_2013to2020)[length(Pres_Abs_2013to2020)]<-"dis_seamount"



## Extracting dynamic variables


####-------extracting SST------####
home_dir<-"E:/HYCOM"
sst_dir<-'E:/HYCOM/water_temp/'


#unique dates
udates <-unique(Pres_Abs_2013to2020$date)
udates<-as.Date(udates)


#need blank dataframe
sst_df<-data.frame()

for (i in 1:length(udates)){
  
  day_subset<- filter(Pres_Abs_2013to2020, 
                      Pres_Abs_2013to2020$date==udates[i])
  
  
  
  #make into spatial dataframe
  coordinates(day_subset)<-c("lon", "lat")
  #have to make a NWA_XXX_ex the same projection as bathy
  projection(day_subset)<-"+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0"
  
  #bring in the same sst date
  sstList <- grep(list.files(path = paste(home_dir, '/water_temp', sep='')), pattern = udates[i], value = TRUE)
  sstList<-sstList[-2]# dont want the sd file
  setwd(sst_dir)
  sst_day_raster<-raster(sstList)
  raster_extent<-matrix(extent(sst_day_raster))
  #if then statement because we want the raster to be -180 to 180 format. Some of the hycom rasters are in 0 to 360
  if(raster_extent[1,1]==-100.04) {
    sst_day_raster
  } else {
    suppressWarnings(sst_day_raster<-rotate(sst_day_raster))
  }
  
  pts.sst<-raster::extract(sst_day_raster,day_subset)
  day.pts.sst<-data.frame(cbind(coordinates(day_subset),pts.sst))
  day.pts.sst<-mutate(day.pts.sst, id=day_subset$id)
  colnames(day.pts.sst)<-c("lon","lat","SST","id")
  sst_df<-rbind(sst_df,day.pts.sst)
}

#combining the two data sets by the id #
sst_df<-sst_df[-c(1:2)]#don't need these
Pres_Abs_2013to2020<-merge(Pres_Abs_2013to2020,sst_df, by="id")
#changing the column name
Pres_Abs_2013to2020<- Pres_Abs_2013to2020 %>% rename(SST = pts.sst) #there is an error message but it works so IDK

rm(sstList,sst_df) #relieving memory

####-------extracting SSH------####


#need blank dataframe
ssh_df<-data.frame()

for (i in 1:length(udates)){
  day_subset<- subset(Pres_Abs_2013to2020, 
                      Pres_Abs_2013to2020$date==udates[i])
  
  
  #make into spatial dataframe
  coordinates(day_subset)<-c("lon", "lat")
  #have to make a NWA_XXX_ex the same projection as bathy
  projection(day_subset)<-"+proj=longlat +ellps=WGS84
  +towgs84=0,0,0,0,0,0,0"
  
  #bring in the same ssh date
  sshList <- grep(list.files(path = paste(home_dir, '/surf_el', sep='')), pattern = udates[i], value = TRUE)
  sshList<-sshList[-2]# dont want the sd file
  setwd('E:/HYCOM/surf_el/')
  ssh_day_raster<-raster(sshList)
  raster_extent<-matrix(extent(ssh_day_raster))
  if(raster_extent[1,1]==-100.04) {
    ssh_day_raster
  } else {
    suppressWarnings(ssh_day_raster<-rotate(ssh_day_raster))
  }
  
  pts.ssh<-raster::extract(ssh_day_raster,day_subset)
  day.pts.ssh<-data.frame(cbind(coordinates(day_subset),pts.ssh))
  day.pts.ssh<-mutate(day.pts.ssh, id=day_subset$id)
  ssh_df<-rbind(ssh_df,day.pts.ssh)
}

#combining the two data sets by the id #
ssh_df<-ssh_df[-c(1:2)]#don't need these
Pres_Abs_2013to2020<-merge(Pres_Abs_2013to2020,ssh_df, by="id")
#changing the column name
Pres_Abs_2013to2020<- Pres_Abs_2013to2020 %>% rename(SSH = pts.ssh)

rm(sshList,ssh_df)


####-------extracting SST SD-------####
#need blank dataframe
sst_sd_df<-data.frame()

for (i in 1:length(udates)){
  day_subset<- subset(Pres_Abs_2013to2020, 
                      Pres_Abs_2013to2020$date==udates[i])
  
  
  #make into spatial dataframe
  coordinates(day_subset)<-c("lon", "lat")
  #have to make a NWA_XXX_ex the same projection as bathy
  projection(day_subset)<-"+proj=longlat +ellps=WGS84
  +towgs84=0,0,0,0,0,0,0"
  
  #bring in the same sst_sd date
  sst_sdList <- grep(list.files(path = paste(home_dir, '/water_temp', sep='')), pattern = udates[i], value = TRUE)
  sst_sdList<-sst_sdList[-1]# I want the sd file this time
  setwd('E:/HYCOM/water_temp/')
  sst_sd_day_raster<-raster(sst_sdList)
  raster_extent<-matrix(extent(sst_sd_day_raster))
  if(raster_extent[1,1]==-100.04) {
    sst_sd_day_raster
  } else {
    suppressWarnings(sst_sd_day_raster<-rotate(sst_sd_day_raster))
  }
  
  pts.sst_sd<-raster::extract(sst_sd_day_raster,day_subset)
  day.pts.sst_sd<-data.frame(cbind(coordinates(day_subset),pts.sst_sd))
  day.pts.sst_sd<-mutate(day.pts.sst_sd, id=day_subset$id)
  sst_sd_df<-rbind(sst_sd_df,day.pts.sst_sd)
}

#combining the two data sets by the id #
sst_sd_df<-sst_sd_df[-c(1:2)]#don't need these
Pres_Abs_2013to2020<-merge(Pres_Abs_2013to2020,sst_sd_df, by="id")
#changing the column name
Pres_Abs_2013to2020<- Pres_Abs_2013to2020 %>% rename(SST_SD = pts.sst_sd)

rm(sst_sdList,sst_sd_df)


####-------extracting SSH SD-------####
#need blank dataframe
ssh_sd_df<-data.frame()

for (i in 1:length(udates)){
  day_subset<- subset(Pres_Abs_2013to2020, 
                      Pres_Abs_2013to2020$date==udates[i])
  
  
  #make into spatial dataframe
  coordinates(day_subset)<-c("lon", "lat")
  #have to make a NWA_XXX_ex the same projection as bathy
  projection(day_subset)<-"+proj=longlat +ellps=WGS84
  +towgs84=0,0,0,0,0,0,0"
  
  #bring in the same ssh_sd date
  ssh_sdList <- grep(list.files(path = paste(home_dir, '/surf_el', sep='')), pattern = udates[i], value = TRUE)
  ssh_sdList<-ssh_sdList[-1]# dont want the non-sd file
  setwd('E:/HYCOM/surf_el/')
  ssh_sd_day_raster<-raster(ssh_sdList)
  raster_extent<-matrix(extent(ssh_sd_day_raster))
  if(raster_extent[1,1]==-100.04) {
    ssh_sd_day_raster
  } else {
    suppressWarnings(ssh_sd_day_raster<-rotate(ssh_sd_day_raster))
  }
  
  pts.ssh_sd<-raster::extract(ssh_sd_day_raster,day_subset)
  day.pts.ssh_sd<-data.frame(cbind(coordinates(day_subset),pts.ssh_sd))
  day.pts.ssh_sd<-mutate(day.pts.ssh_sd, id=day_subset$id)
  ssh_sd_df<-rbind(ssh_sd_df,day.pts.ssh_sd)
}

#combining the two datastes by the id #
ssh_sd_df<-ssh_sd_df[-c(1:2)]#don't need these
Pres_Abs_2013to2020<-merge(Pres_Abs_2013to2020,ssh_sd_df, by="id")
#changing the column name
Pres_Abs_2013to2020<- Pres_Abs_2013to2020 %>% rename(SSH_SD = pts.ssh_sd)

rm(ssh_sdList,ssh_sd_df)



####-------extracting EKE-------####
#need blank dataframe
eke_df<-data.frame()

for (i in 1:length(udates)){
  day_subset<- subset(Pres_Abs_2013to2020, 
                      Pres_Abs_2013to2020$date==udates[i])
  
  
  #make into spatial dataframe
  coordinates(day_subset)<-c("lon", "lat")
  #have to make a NWA_XXX_ex the same projection as bathy
  projection(day_subset)<-"+proj=longlat +ellps=WGS84
  +towgs84=0,0,0,0,0,0,0"
  
  #bring in the same eke date
  ekeList <- grep(list.files(path = paste(home_dir, '/eke', sep='')), pattern = udates[i], value = TRUE)
  setwd('E:/HYCOM/eke/')
  eke_day_raster<-raster(ekeList)
  raster_extent<-matrix(extent(eke_day_raster))
  if(raster_extent[1,1]==-100.04) {
    eke_day_raster
  } else {
    suppressWarnings(eke_day_raster<-rotate(eke_day_raster))
  }
  
  pts.eke<-raster::extract(eke_day_raster,day_subset)
  day.pts.eke<-data.frame(cbind(coordinates(day_subset),pts.eke))
  day.pts.eke<-mutate(day.pts.eke, id=day_subset$id)
  eke_df<-rbind(eke_df,day.pts.eke)
}

#combining the two datastes by the id #
eke_df<-eke_df[-c(1:2)]#don't need these
Pres_Abs_2013to2020<-merge(Pres_Abs_2013to2020,eke_df, by="id")
#changing the column name
Pres_Abs_2013to2020<- Pres_Abs_2013to2020 %>% rename(EKE = pts.eke)

rm(ekeList,eke_df)


####-------extracting ILD------####
#need blank dataframe
ild5_df<-data.frame()

for (i in 1:length(udates)){
  day_subset<- subset(Pres_Abs_2013to2020, 
                      Pres_Abs_2013to2020$date==udates[i])
  
  
  #make into spatial dataframe
  coordinates(day_subset)<-c("lon", "lat")
  #have to make a NWA_XXX_ex the same projection as bathy
  projection(day_subset)<-"+proj=longlat +ellps=WGS84
  +towgs84=0,0,0,0,0,0,0"
  
  #bring in the same ild5 date
  ild5List <- grep(list.files(path = paste(home_dir, '/ild5', sep='')), pattern = udates[i], value = TRUE)
  setwd('E:/HYCOM/ild5/')
  ild5_day_raster<-raster(ild5List)
  raster_extent<-matrix(extent(ild5_day_raster))
  if(raster_extent[1,1]==-100.04) {
    ild5_day_raster
  } else {
    suppressWarnings(ild5_day_raster<-rotate(ild5_day_raster))
  }
  
  pts.ild5<-raster::extract(ild5_day_raster,day_subset)
  day.pts.ild5<-data.frame(cbind(coordinates(day_subset),pts.ild5))
  day.pts.ild5<-mutate(day.pts.ild5, id=day_subset$id)
  ild5_df<-rbind(ild5_df,day.pts.ild5)
}

#combining the two datastes by the id #
ild5_df<-ild5_df[-c(1:2)]#don't need these
Pres_Abs_2013to2020<-merge(Pres_Abs_2013to2020,ild5_df, by="id")
#changing the column name
Pres_Abs_2013to2020<- Pres_Abs_2013to2020 %>% rename(ild5 = pts.ild5)

rm(ild5List,ild5_df)


####-------extracting n2------####
#need blank dataframe
n2_df<-data.frame()

for (i in 1:length(udates)){
  day_subset<- subset(Pres_Abs_2013to2020, 
                      Pres_Abs_2013to2020$date==udates[i])
  
  
  #make into spatial dataframe
  coordinates(day_subset)<-c("lon", "lat")
  #have to make a NWA_XXX_ex the same projection as bathy
  projection(day_subset)<-"+proj=longlat +ellps=WGS84
  +towgs84=0,0,0,0,0,0,0"
  
  #bring in the same n2 date
  n2List <- grep(list.files(path = paste(home_dir, '/n2', sep='')), pattern = udates[i], value = TRUE)
  setwd('E:/HYCOM/n2/')
  n2_day_raster<-raster(n2List)
  raster_extent<-matrix(extent(n2_day_raster))
  if(raster_extent[1,1]==-100.04) {
    n2_day_raster
  } else {
    suppressWarnings(n2_day_raster<-rotate(n2_day_raster))
  }
  
  pts.n2<-raster::extract(n2_day_raster,day_subset)
  day.pts.n2<-data.frame(cbind(coordinates(day_subset),pts.n2))
  day.pts.n2<-mutate(day.pts.n2, id=day_subset$id)
  n2_df<-rbind(n2_df,day.pts.n2)
}

#combining the two datastes by the id #
n2_df<-n2_df[-c(1:2)]#don't need these
Pres_Abs_2013to2020<-merge(Pres_Abs_2013to2020,n2_df, by="id")
#changing the column name
Pres_Abs_2013to2020<- Pres_Abs_2013to2020 %>% rename(n2 = pts.n2)

rm(n2List,n2_df)


#lets save it!
saveRDS(Pres_Abs_2013to2020, "C:/Users/nfarc/Desktop/NASA_FaCeT/Data/2_AIS_wENV/Pres_Abs_2013to2020_NWA_USA_LL_onlyfishing_v2_absencesconstrained_convexhull.rds")





###clean up the data set

#need to change the hours column for absences to 0 - currently it is NA
Pres_Abs_2013to2020<- Pres_Abs_2013to2020 %>% mutate(hours = ifelse(Pres_abs == 0, 0, hours))
Pres_Abs_2013to2020<-na.omit(Pres_Abs_2013to2020)

#getting a 1:1 ratio
udates <-unique(Pres_Abs_2013to2020$date)

absences<-data.frame()
for (i in 1:length(udates)){
  subdate_presence<-filter(Pres_Abs_2013to2020, 
                           Pres_Abs_2013to2020$date== udates[i] & Pres_Abs_2013to2020$Pres_abs == 1)
  
  subdate_absences= Pres_Abs_2013to2020 %>%
    filter((Pres_Abs_2013to2020$Pres_abs == 0 & Pres_Abs_2013to2020$date == udates[i])) %>%
    .[sample(nrow(.),nrow(subdate_presence)),]
  
  absences<-rbind(absences,subdate_absences)
}

#subsetting the presences 
presence<-filter(Pres_Abs_2013to2020, Pres_Abs_2013to2020$Pres_abs == 1)    


table(presence$year,presence$Pres_abs)
table(absences$year,absences$Pres_abs) 

Pres_Abs_2013to2020_1to1ratio_w_env<-rbind(presence,absences)   

saveRDS(Pres_Abs_2013to2020_1to1ratio_w_env, "C:/Users/nfarc/Desktop/NASA_FaCeT/Data/2_AIS_wENV/processed/Pres_Abs_2013to2020_NWA_USA_LL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull.rds")
