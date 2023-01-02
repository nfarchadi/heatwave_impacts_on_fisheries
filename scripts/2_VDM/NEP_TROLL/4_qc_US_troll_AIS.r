#QCing the AIS for the U.S. pacific troll fleet

library(rgdal)
library(rgeos)
library(tidyverse)
library(raster)
library(rnaturalearth)
library(here)
library(sf)

source(here("scripts","2_VDM","functions","remove_land.r"))
source(here("scripts","2_VDM","functions","generate_abs.r"))

# load in the AIS data
NEP_TROLL<-here("data","AIS","NEP_TROLL","AIS_USA_NEP_TROLL_2013_2020.rds") %>%
  readRDS()

# need to reduce the resolution to 0.08 degrees to match ALB
res <- 0.08

NEPTROLL<-NEP_TROLL %>% 
  mutate(lat = floor(lat/res) * res + 0.5 * res,
         lon = floor(lon/res) * res + 0.5 * res) %>% 
  group_by(date, lat, lon) %>% 
  summarise(fishing_hours = sum(fishing_hours, na.rm = T), .groups = "drop") %>%
  filter(fishing_hours >= 1) %>% #here is where filter above 1 hour
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))


#########################
# removing points on land
#########################

# Since I reduced the resolution some points are now on land so we want to remove those. I made a function that uses bathymetry raster to determine if points are on land or not. 

bathy <- here("data","static","NEP_TROLL","NEP_bathy.nc")

NEPTROLL<- remove_land(NEPTROLL, bathy_file = bathy)

#################################################
# filter to match troll fleets targeting albacore
#################################################

# remove anything north 54 N or south of 32. Also spatially filtering to where albacore vessels will most likely be (-1000 m bathy) and only during the ALB fishing season (Jun - Oct). 
NEPtroll<-NEPtroll %>% filter(z <= -1000 & (month >= 5 & month <= 11) & (lon >= -135 & lat >= 32 & lat <= 54))


##############################
# generating background points
##############################

# Use one of your environmental rasters as a template. Be sure to have it as the same resolution as what your predicted outputs will be. Most likely this will be the raster with the lowest common resolution.

template<-raster(here("data","static","NEP_TROLL","NEP_bathy.nc")) 


#convex hull polygon which will be used to select background points from
hull<-terra::convHull(terra::vect(NEPTROLL))
hull<-as(hull, "Spatial")


string=seq(1:ncell(template))
template[]=string #adds values to the raster

# need to use rast for template because 
template<-mask(template,hull) # only points within the convexhull

# extract unique values to presences
unique_pres<-raster::extract(template,
                             sf::st_as_sf(NEPTROLL, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
NEPTROLL<-NEPTROLL %>% mutate(unique=unique_pres,type=1)


# filter dates based on presence availability
udates <-unique(NEPTROLL$date)
udates<-as.Date(udates)

# for loop below uses the generate_abs function that will generate the same number of pseudo-absences as the presences for each day from areas that there isn't a presence point

# ratio argument allows you to increase the number of pseudo-absences daily by a factor of whatever value you give it.

# Just in case I like to have more pseudo-absences so I sent the ratio argument to 3 to have a 1:3 presence:pseudo-absence ratio

absences<-data.frame()
for (i in 1:length(udates)) {
  ud<-udates[i]
  ab<-generate_abs(NEPTROLL, ud, template, ratio = 3)
  absences<-rbind(absences,ab)
}


# combining presence and background dataframes
absences<-absences[,-c(4,5,6,8,9)]

# need a z column to match with presence df
absences<- remove_land(absences, bathy_file = bathy)
NEPTROLL$date<-as.Date(NEPTROLL$date)
NEPTROLL<-NEPTROLL[,c(1,3,2,4,6,5,8,9,7)]
#now we combine
Pres_Abs_NEPTROLL_2013to2020<-rbind(NEPTROLL,absences)


#make an id column. this will help when cbinding dataframes later
Pres_Abs_NEPTROLL_2013to2020<-mutate(Pres_Abs_NEPTROLL_2013to2020, id = row_number())


saveRDS(Pres_Abs_NEPTROLL_2013to2020,here("data","AIS","NEP_TROLL","Pres_Abs_2013to2020_NEP_USA_TROLL_onlyfishing_1to3ratio_absenceconstrained_convexhull.rds"))





# Constructing a map to look at the data

#retrieving that data for the continents 
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, color= "black", fill = "grey" ) + #bring in world data
  #geom_sf(data = eezs, color = "blue") + #bring in EEZ data
  geom_point(data = Pres_Abs_NEPTROLL_2013to2020, aes(x=lon, y=lat))+
  #geom_point(data = absences, aes(x=cell_ll_lon, y=cell_ll_lat, color = "black")) +
  coord_sf(xlim = c(-100,-30), ylim = c(10,50), expand = FALSE) + #changing the extent
  ylab("Latitude") + xlab("Longitude") + theme_bw()




