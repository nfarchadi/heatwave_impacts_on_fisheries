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
NWAPLL<-here("data","AIS","NWA_PLL",
               "Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_1to3ratio_absenceconstrained_convexhull.rds") %>% readRDS()


#changing the heading name
NWAPLL<-NWAPLL %>% rename(Pres_abs = type)

NWAPLL<-NWAPLL %>% dplyr::select(-unique) #remove this column


###################################################
# enhancing AIS with hycom & bathy data for the NWA
###################################################

HYCOM_NWA_dir<-here("data","hycom","NWA_PLL","hycom_combine")


# function to enhance AIS data
NWAPLL2<-enhance_AIS(NWAPLL,HYCOM_NWA_dir) #run it!


# save it!
saveRDS(NWAPLL2,here("data","AIS","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_1to3ratio_absenceconstrained_convexhull_enhanced.rds"))


##############################
# enhancing distance from port
##############################

dis_port<-raster(here("data","static","NWA_PLL","NWA_dis_port.nc"))


NWAPLL2<-sf::st_as_sf(NWAPLL2, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


pts.ports<-raster::extract(dis_port,NWAPLL2)
fleet.ports<-data.frame(cbind(sf::st_coordinates(NWAPLL2),pts.ports))
variable<-"dis_port"
NWAPLL2[,variable]<-fleet.ports[,3]


###################################
# enhancing distance from seamounts
###################################

dis_seamount<-raster(here("data","static","NWA_PLL","NWA_dis_seamount.nc"))


NWAPLL2<-sf::st_as_sf(NWAPLL2, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


pts.seamounts<-raster::extract(dis_seamount,NWAPLL2)
fleet.seamounts<-data.frame(cbind(sf::st_coordinates(NWAPLL2),pts.seamounts))
variable<-"dis_seamount"
NWAPLL2[,variable]<-fleet.seamounts[,3]


# save data again just in case
saveRDS(NWAPLL2,here("data","AIS","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_1to3ratio_absenceconstrained_convexhull_enhanced.rds"))



#######################
# clean up the data set
#######################

NWAPLL3<-na.omit(NWAPLL2)

# getting the data in a 1:1 ratio
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

#making sure the number of presences and absences are the same
table(presence$year,presence$Pres_abs)
table(absences$year,absences$Pres_abs) 

NWAPLL3_1to1ratio_w_env<-rbind(presence,absences)   

saveRDS(NWAPLL3_1to1ratio_w_env, here("data","AIS_processed","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_1to1ratio_absenceconstrained_convexhull_enhanced.rds"))
