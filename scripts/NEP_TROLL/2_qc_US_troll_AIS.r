#generating pseudo-absences for the US pacific troll fleet AIS data

library(rgdal)
library(rgeos)
library(tidyverse)
library(raster)
library(rnaturalearth)
library(here)
library(sf)
sf_use_s2(FALSE)


#load in the AIS data
NEP_troll<-here("data","AIS_processed","NEP_TROLL","AIS_USA_troll",
                "AIS_USA_NEP_TROLL_2012_2020.rds") %>% readRDS()


#need to reduce the resolution to 0.08 degrees to match ALB
res <- 0.08

NEPtroll<-NEP_troll %>% 
  mutate(lat = floor(lat/res) * res + 0.5 * res,
         lon = floor(lon/res) * res + 0.5 * res) %>% 
  group_by(date, lat, lon) %>% 
  summarise(fishing_hours = sum(fishing_hours, na.rm = T), .groups = "drop") %>% filter(fishing_hours >= 1) %>% #here is where filter above 1 hour
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))


####-----removing points on land-----####
#Since I reduced the resolution some points are now on land so we want to remove those. I made a function that uses bathymetry raster to determine if points are on land or not. 


bathy_file<-"E:/HYCOM_NEP/hycom_combine_2012-01-01.grd"

remove_land<- function(input_df, bathy_file){
  AIS_df<-input_df
  AIS_df_2<-AIS_df
  AIS_df_2<-sf::st_as_sf(AIS_df_2, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  bathy<-raster::raster(bathy_file, band = 11)
  
  pts.bathy<-raster::extract(bathy,AIS_df_2)
  fleet.bathy<-data.frame(cbind(sf::st_coordinates(AIS_df_2),pts.bathy))
  
  variable<-"z"
  AIS_df[,variable]<-fleet.bathy[,c(3)]
  AIS_df <- AIS_df[which(AIS_df$z < 0),]#only bathy less than 
  
  return(AIS_df)
}

NEPtroll<- remove_land(NEPtroll, bathy_file = bathy_file)

#remove anything north 54 N or south of 32 (CCS filtering). Also spatially filtering to where ALB vessels will most likely be (-1000 m bathy) and only during the ALB fishing season (Jun - Oct). 
NEPtroll<-NEPtroll %>% filter(z <= -1000 & (month >= 5 & month <= 11) & (lon >= -135 & lat >= 32 & lat <= 54))


####------obtaining background points------####

## Use one of your environmental rasters as a template. Be sure to have it as the same resolution as what your predicted outputs will be. Most likely this will be the raster with the lowest common resolution. 
template<-raster("E:/HYCOM_NEP/hycom_combine_2012-01-01.grd") 


#convex hull polygon which will be used to select background points from
hull<-terra::convHull(terra::vect(NEPtroll))
hull<-as(hull, "Spatial")


#res(template)=5 ## change to fit your data
string=seq(1:ncell(template))
template[]=string #adds values to the raster
#need to use rast for template because 
template<-mask(template,hull)#only points within the convexhull

# extract unique values to presences
unique_pres<-raster::extract(template,
                             sf::st_as_sf(NEPtroll, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
NEPtroll<-NEPtroll %>% mutate(unique=unique_pres,type=1)


## filter dates based on presence availability
udates <-unique(NEPtroll$date)
udates<-as.Date(udates)

#function to generate absences daily. this will match the same number as your presences or you can make it sample more 
generate_abs<-function(input_df, udates,template){    
  subdate_presence<-filter(NEPtroll, 
                           NEPtroll$date==udates)
  
  subdate_absences=rasterToPoints(template) %>% as.data.frame()  %>% rename("unique"="water_u") %>% as.data.frame() %>%
    filter(!(unique %in%subdate_presence$unique)) %>% mutate(type=0) %>%  ## only select pixels that don't have presences
    .[sample(nrow(.),nrow(subdate_presence)*3),] %>% ## create 1:3 ratio. want more absences in case for later 
    mutate(date=udates,geartype="background",flag="No_flag",
           hours=NA,fishing_hours=0,mmsi_present=0,name=NA) %>% 
    rename("lon"="x", "lat"="y")
  
  subdate_absences$month<-lubridate::month(subdate_absences$date)
  subdate_absences$year<-lubridate::year(subdate_absences$date)
  subdate_absences<-subdate_absences[,c(5,1,2,7,6,8,9,10,11,12,13,3,4)]
  
  return(subdate_absences)
}





## now we have dates, get absences from areas that there isn't a presence point for each day
absences<-data.frame()
for (i in 1:length(udates)) {
  ud<-udates[i]
  ab<-generate_abs(NEPtroll, ud, template)
  absences<-rbind(absences,ab)
}


# combining presence and background dfs
absences<-absences[,-c(4,5,6,8,9)]
#need a z column to match with presence df
absences<- remove_land(absences, bathy_file = bathy_file)
NEPtroll$date<-as.Date(NEPtroll$date)
NEPtroll<-NEPtroll[,c(1,3,2,4,6,5,8,9,7)]
#now we combine
Pres_Abs_NEPtroll_2012to2020<-rbind(NEPtroll,absences)


#make an id column. this will help when cbinding dataframes later
Pres_Abs_NEPtroll_2012to2020<-mutate(Pres_Abs_NEPtroll_2012to2020, id = row_number())


saveRDS(Pres_Abs_NEPtroll_2012to2020,here("data","AIS_processed","NEP_TROLL","AIS_USA_troll","Pres_Abs_2012to2020_NEP_USA_TROLL_onlyfishing_v2_1to3ratio_absenceconstrained_convexhull_v2.rds"))





####----Constructing a map to look at the data------####

#retrieving that data for the continents 
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, color= "black", fill = "grey" ) + #bring in world data
  #geom_sf(data = eezs, color = "blue") + #bring in EEZ data
  geom_point(data = Pres_Abs_NEPtroll_2012to2020 %>% filter(type == 1), aes(x=lon, y=lat, color = as.factor(month)))+
  #geom_point(data = absences, aes(x=cell_ll_lon, y=cell_ll_lat, color = "black")) +
  coord_sf(xlim = c(-135.5,-117), ylim = c(32,54.5), expand = FALSE) + #changing the extent
  ylab("Latitude") + xlab("Longitude") + theme_bw()




