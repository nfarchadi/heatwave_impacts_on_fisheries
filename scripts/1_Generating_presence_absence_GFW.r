#processing presence and absence data from the GFW data. This code will subset USA longline fisheries that are actively fishing in a certain extent and then randomly select absences (background points where no US longline fishing occurred) on any given day. The ratio of presence to absences can be changed so that it is not just 1:1
library(rgdal)
library(rgeos)
library(sp)
library(tidyverse)
library(dplyr)
library(data.table)
library(raster)
library(terra)
library(rnaturalearth)


#daily AIS data 
setwd("E:/VDMs/GFW_fishing_effort_v2")

####----filter the AIS data--------####
filenames<-grep(list.files(path = "E:/VDMs/GFW_fishing_effort_v2"), 
                pattern = ".csv", value = TRUE)

Presences_all_onlyfishing<-data.frame() #need a empty df

years<- c("2013","2014","2015","2016","2017","2018","2019","2020")#for some reason there isn't any USA drifting long lines in the atlantic in 2012 so didn't include

for (i in 1:length(years)){
  fn<-grep(filenames, pattern = years[i], value = TRUE)
  #generate_pres is defined in another r script
  Presences_ls<-lapply(lapply(fn, fread),generate_pres)
  Presences_year<-rbindlist(Presences_ls)
  Presences_all_onlyfishing<-rbind(Presences_all_onlyfishing,Presences_year)
}

#lets save this just in case
write.csv(Presences_all_onlyfishing, 
          file = "E:/VDMs/Presences_all_USA_LL_onlyfishing_v2_2013_2020_worldwide.csv")

Presences_all_onlyfishing<-read.csv("E:/VDMs/Presences_all_USA_LL_onlyfishing_v2_2013_2020_worldwide.csv")
Presences_all_onlyfishing<-Presences_all_onlyfishing[,-1]





####-----removing points on land-----####
#Since I reduced the resolution some points are now on land so we want to remove those
names(Presences_all_onlyfishing)[2:3]<-c("lon","lat")
Presences_all_onlyfishing$year<-lubridate::year(Presences_all_onlyfishing$date)
bathy_file<-"C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/ETOPO1/global_bathy_180.nc"

Presences_noland<- remove_land(Presences_all_onlyfishing, 
                               bathy_file = bathy_file)





####------obtaining background points------####

## Use one of your environmental rasters as a template. Be sure to have it as the same resolution as what your predicted outputs will be. Most likely this will be the raster with the lowest common resolution. 
template<-raster("E:/HYCOM/water_temp/hycom_temp_2012-01-01.nc") 

#need a mask for only points in the Atlantic ocean
atl_ocean<-sf::st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/Atlantic_Ocean_shp/iho.shp") 

#want to get background points from places around the each vessel location
Presences_all_contrained<-filter(Presences_noland,lat <= 50 & lat >= 10,lon >= -100 & lon <= -30)
Presences_all_contrained<-data.frame(Presences_all_contrained)


#convex hull polygon which will be used to select background points from
hull<-terra::convHull(vect(Presences_all_contrained)) 
hull<-as(hull, "Spatial")
plot(hull)

#res(template)=5 ## change to fit your data
string=seq(1:ncell(template))
template[]=string #adds values to the raster
template<-mask(template,atl_ocean)#take out pacific ocean points
#need to use rast for template because 
template<-mask(template,hull)#only points within the convexhull

# extract unique values to presences
unique_pres<-raster::extract(template,
                            Presences_all_contrained[,2:3])
Presences_all_contrained<-Presences_all_contrained %>% mutate(unique=unique_pres,type=1)


## filter dates based on presence availability
udates <-unique(Presences_all_contrained$date)
udates<-as.Date(udates)

## now we have dates, get absences from areas that there isn't a presence point for each day
absences<-data.frame()
for (i in 1:length(udates)) {
  ud<-udates[i]
  ab<-generate_abs(Presences_all_contrained, ud)
  absences<-rbind(absences,ab)
}


# combining presence and background dfs
absences<-absences[,-c(9:10)]
#need a z column to match with presence df
absences<- remove_land(absences, bathy_file = bathy_file)
Presences_all_contrained$date<-as.Date(Presences_all_contrained$date)
#now we combine
Pres_Abs_2013to2020<-rbind(Presences_all_contrained,absences)


#make an id column. this will help when cbinding dataframes later
Pres_Abs_2013to2020<-mutate(Pres_Abs_2013to2020, id = row_number())

saveRDS(Pres_Abs_2013to2020,"E:/VDMs/Pres_Abs_2013to2020_NWA_USA_LL_onlyfishing_v2.rds")





####----Constructing a map to look at the data------####

#retrieving that data for the continents 
world <- ne_countries(scale = "medium", returnclass = "sf")


#inputting EEZ data
eezs <- sf::read_sf('E:/VDMs/World_EEZ_v9_20161021_LR/eez_lr.shp') %>% 
  filter(Territory1== "United States") # select the 200 nautical mile polygon layer


ggplot() +
  geom_sf(data = world, color= "black", fill = "grey" ) + #bring in world data
  geom_sf(data = eezs, color = "blue") + #bring in EEZ data
  geom_point(data = Pres_Abs_2013to2020, aes(x=lon, y=lat))+
  #geom_point(data = absences, aes(x=cell_ll_lon, y=cell_ll_lat, color = "black")) +
  coord_sf(xlim = c(-100,10), ylim = c(10,50), expand = FALSE) + #changing the extent
  ylab("Latitude") + xlab("Longitude") + theme_bw()




