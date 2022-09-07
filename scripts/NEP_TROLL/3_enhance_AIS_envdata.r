#enhancing AIS with environmental data 

#loading in packages
library(rgdal)
library(rgeos)
library(tidyverse)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(here)
sf_use_s2(FALSE)

#load in the presence-absence data if its not already in your global environment 
NEPtroll<-here("data","AIS_processed","NEP_TROLL","AIS_USA_troll",
               "Pres_Abs_2012to2020_NEP_USA_TROLL_onlyfishing_v2_1to3ratio_absenceconstrained_convexhull_v2.rds") %>% readRDS()


#changing the heading name
NEPtroll<-NEPtroll %>% rename(Pres_abs = type)

NEPtroll<-NEPtroll %>% dplyr::select(-unique) #remove this column



####----enhancing AIS with NEP env data----####

HYCOM_NEP_dir<-"E:/HYCOM_NEP/"


#function to enhance AIS data
enhance_AIS<-function(input_df, env_dir){
  dates <-unique(input_df$date) %>% as.Date() #get unique dates from df
  enhanced_df<-data.frame()#need a empty df
  
  #for loop that subsets input df daily 
  #then enhances that data specific to the same date of a raster file
  #rbinds it all at the end
  for (i in 1:length(dates)){ 
  day_subset<- filter(input_df, 
                      input_df$date==dates[i])
  
  day_subset<-sf::st_as_sf(day_subset, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  #bring in the same env date
  env_file_list <- grep(list.files(path = env_dir, full.names = TRUE, pattern = ".grd"), pattern = dates[i], value = TRUE)
  
  if(length(env_file_list)==0){
    next
  } 
 
  env_day_stack<-stack(env_file_list)
  
  pts.env<-raster::extract(env_day_stack,day_subset)

  day.pts.env<-cbind(sf::st_coordinates(day_subset),day_subset,pts.env)%>% dplyr::select(-c("geometry","z")) %>% mutate(id=day_subset$id)
 
  enhanced_df<-rbind(enhanced_df,day.pts.env)
  }
  return(enhanced_df) #returns the fully enhanced df
}


NEPtroll2<-enhance_AIS(NEPtroll,HYCOM_NEP_dir) #run it!

#save it!
saveRDS(NEPtroll2,here("data","AIS_processed","NEP_TROLL","AIS_USA_troll","Pres_Abs_2012to2020_NEP_USA_TROLL_onlyfishing_v2_1to3ratio_absenceconstrained_convexhull_v2_enhanced.rds"))

NEPtroll<-readRDS(here("data","AIS_processed","NEP_TROLL","AIS_USA_troll","Pres_Abs_2012to2020_NEP_USA_TROLL_onlyfishing_v2_1to3ratio_absenceconstrained_convexhull_v2_enhanced.rds"))

####----enhancing distance from port----####

dis_port<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NEP_ALB_dis_port_masked.nc")


NEPtroll<-sf::st_as_sf(NEPtroll, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


pts.ports<-raster::extract(dis_port,NEPtroll)
fleet.ports<-data.frame(cbind(sf::st_coordinates(NEPtroll),pts.ports))
variable<-"dis_port"
NEPtroll[,variable]<-fleet.ports[,3]




####----enhancing distance from seamounts----####

#need to first reate distance from seamount raster for NEP
#first load modelled seamount locations
# seamounts<-sf::st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/ModelledSeamounts2011_v1/DownloadPack-14_001_ZSL002_ModelledSeamounts2011_v1/01_Data/Seamounts/Seamounts.shp")

# template<-raster("E:/HYCOM_NEP/hycom_combine_2012-01-01.grd") ##need to template raster
# 
# seamounts<-sf::st_crop(seamounts,sf::st_bbox(template))
# 
# 
# dist_seamount<-distanceFromPoints(template,seamounts)
# dist_seamount_masked<-mask(dist_seamount,template)
# dist_seamount_masked<-dist_seamount_masked*0.001 #converting to km
# 
# #save as raster
# writeRaster(dist_seamount_masked, "C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance_from_seamount/dis_from_seamount_NEP.nc", format='CDF', overwrite=TRUE)


dis_seamount<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance_from_seamount/dis_from_seamount_NEP.nc")


NEPtroll<-sf::st_as_sf(NEPtroll, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


pts.seamounts<-raster::extract(dis_seamount,NEPtroll)
fleet.seamounts<-data.frame(cbind(sf::st_coordinates(NEPtroll),pts.seamounts))
variable<-"dis_seamount"
NEPtroll[,variable]<-fleet.seamounts[,3]

saveRDS(NEPtroll,here("data","AIS_processed","NEP_TROLL","AIS_USA_troll","Pres_Abs_2012to2020_NEP_USA_TROLL_onlyfishing_v2_1to3ratio_absenceconstrained_convexhull_v2_enhanced.rds"))

NEPtroll<-here("data","AIS_processed","NEP_TROLL","AIS_USA_troll","Pres_Abs_2012to2020_NEP_USA_TROLL_onlyfishing_v2_1to3ratio_absenceconstrained_convexhull_v2_enhanced.rds") %>% readRDS()




###clean up the data set
NEPtroll2<-na.omit(NEPtroll)
#getting a 1:1 ratio
udates <-unique(NEPtroll2$date)

absences<-data.frame()
for (i in 1:length(udates)){
  subdate_presence<-filter(NEPtroll2, 
                           NEPtroll2$date== udates[i] & NEPtroll2$Pres_abs == 1)
  
  subdate_absences= NEPtroll2 %>%
    filter((NEPtroll2$Pres_abs == 0 & NEPtroll2$date == udates[i])) %>%
    .[sample(nrow(.),nrow(subdate_presence)),]
  
  absences<-rbind(absences,subdate_absences)
}

#subsetting the presences 
presence<-filter(NEPtroll2, NEPtroll2$Pres_abs == 1)    


table(presence$year,presence$Pres_abs)
table(absences$year,absences$Pres_abs) 

NEPtroll2_1to1ratio_w_env<-rbind(presence,absences)   

saveRDS(NEPtroll2_1to1ratio_w_env, here("data","AIS_processed","NEP_TROLL","Pres_Abs_2013to2020_NWA_USA_TROLL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull_v2_enhanced.rds"))


#Constructing the map
#retreiving that data for the continents 
world <- ne_countries(scale = "medium", returnclass = "sf")



NEPtroll2_1to1ratio_w_env %>% filter(Pres_abs == 1 ) %>% 
  ggplot() +
  geom_sf(data = world, color= "black", fill = "grey" ) + #bring in world data
  coord_sf(xlim = c(-135,-117), ylim = c(32,54)) + #changing the extent
  ylab("Latitude") + xlab("Longitude") + 
  facet_wrap(~month)+
  geom_point(aes(x=X,y=Y))+
  theme_bw(base_size = 10) + theme(panel.grid=element_blank())+
  theme(axis.text=element_text(size=20))+
  theme(legend.title=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  #theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"))+
  theme(strip.text = element_text(size = 20))+
  theme(axis.title = element_text(size = 20))



