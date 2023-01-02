#enhancing AIS with environmental data 

#loading in packages
library(rgdal)
library(rgeos)
library(tidyverse)
library(raster)
library(sf)
library(here)

source(here("scripts","2_VDM","functions","enhance_AIS.r"))

#load in the presence-absence data if its not already in your global environment 
NEPTROLL<-here("data","AIS","NEP_TROLL",
               "Pres_Abs_2013to2020_NEP_USA_TROLL_onlyfishing_1to3ratio_absenceconstrained_convexhull.rds") %>% readRDS()


#changing the heading name
NEPTROLL<-NEPTROLL %>% rename(Pres_abs = type)

NEPTROLL<-NEPTROLL %>% dplyr::select(-unique) #remove this column


###################################################
# enhancing AIS with hycom & bathy data for the NEP
###################################################

HYCOM_NEP_dir<-here("data","hycom","NEP_TROLL","hycom_combine")


# function to enhance AIS data
NEPTROLL2<-enhance_AIS(NEPTROLL,HYCOM_NEP_dir) #run it!


# save it!
saveRDS(NEPTROLL2,here("data","AIS","NEP_TROLL","Pres_Abs_2013to2020_NEP_USA_TROLL_onlyfishing_1to3ratio_absenceconstrained_convexhull_enhanced.rds"))


##############################
# enhancing distance from port
##############################

dis_port<-raster(here("data","static","NEP_TROLL","NEP_dis_port.nc"))


NEPTROLL2<-sf::st_as_sf(NEPTROLL2, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


pts.ports<-raster::extract(dis_port,NEPTROLL2)
fleet.ports<-data.frame(cbind(sf::st_coordinates(NEPTROLL2),pts.ports))
variable<-"dis_port"
NEPTROLL2[,variable]<-fleet.ports[,3]


###################################
# enhancing distance from seamounts
###################################

dis_seamount<-raster(here("data","static","NEP_TROLL","NEP_dis_seamount.nc"))


NEPTROLL2<-sf::st_as_sf(NEPTROLL2, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


pts.seamounts<-raster::extract(dis_seamount,NEPTROLL2)
fleet.seamounts<-data.frame(cbind(sf::st_coordinates(NEPTROLL2),pts.seamounts))
variable<-"dis_seamount"
NEPTROLL2[,variable]<-fleet.seamounts[,3]


# save data again just in case
saveRDS(NEPTROLL2,here("data","AIS","NEP_TROLL","Pres_Abs_2013to2020_NEP_USA_TROLL_onlyfishing_1to3ratio_absenceconstrained_convexhull_enhanced.rds"))



#######################
# clean up the data set
#######################

NEPTROLL3<-na.omit(NEPTROLL2)

# getting the data in a 1:1 ratio
udates <-unique(NEPTROLL3$date)

absences<-data.frame()
for (i in 1:length(udates)){
  subdate_presence<-filter(NEPTROLL3, 
                           NEPTROLL3$date== udates[i] & NEPTROLL3$Pres_abs == 1)
  
  subdate_absences= NEPTROLL3 %>%
    filter((NEPTROLL3$Pres_abs == 0 & NEPTROLL3$date == udates[i])) %>%
    .[sample(nrow(.),nrow(subdate_presence)),]
  
  absences<-rbind(absences,subdate_absences)
}

#subsetting the presences 
presence<-filter(NEPTROLL3, NEPTROLL3$Pres_abs == 1)    

#making sure the number of presences and absences are the same
table(presence$year,presence$Pres_abs)
table(absences$year,absences$Pres_abs) 

NEPTROLL3_1to1ratio_w_env<-rbind(presence,absences)   

saveRDS(NEPTROLL3_1to1ratio_w_env, here("data","AIS_processed","NEP_TROLL","Pres_Abs_2013to2020_NEP_USA_TROLL_onlyfishing_1to1ratio_absenceconstrained_convexhull_enhanced.rds"))
