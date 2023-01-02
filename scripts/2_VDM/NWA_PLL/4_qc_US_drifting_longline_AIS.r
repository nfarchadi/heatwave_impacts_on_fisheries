#QCing the data and  generating pseudo-absences for the US atlantic longline fleet AIS data

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
NWA_PLL<-here("data","AIS","NWA_PLL","AIS_USA_NWA_PLL_2013_2020.rds") %>% readRDS()

# need to reduce the resolution to 0.08 degrees to match ALB
res <- 0.08

NWAPLL<-NWA_PLL %>% 
  mutate(lat = floor(lat/res) * res + 0.5 * res,
         lon = floor(lon/res) * res + 0.5 * res) %>% 
  group_by(date, lat, lon) %>% 
  summarise(fishing_hours = sum(fishing_hours, na.rm = T), .groups = "drop") %>% filter(fishing_hours >= 1) %>% #here is where filter above 1 hour
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))


#########################
# removing points on land
#########################

# Since I reduced the resolution some points are now on land so we want to remove those. I made a function that uses bathymetry raster to determine if points are on land or not. 

bathy <- here("data","static","NWA_PLL","NWA_bathy.nc")

NWAPLL<- remove_land(NWAPLL, bathy_file = bathy)


##############################
# generating background points
##############################

# Use one of your environmental rasters as a template. Be sure to have it as the same resolution as what your predicted outputs will be. Most likely this will be the raster with the lowest common resolution.

template<-raster(here("data","static","NWA_PLL","NWA_bathy.nc")) 


#convex hull polygon which will be used to select background points from
hull<-terra::convHull(terra::vect(NWAPLL))
hull<-as(hull, "Spatial")


string=seq(1:ncell(template))
template[]=string #adds values to the raster

# need to use rast for template because 
template<-mask(template,hull) # only points within the convexhull

# extract unique values to presences
unique_pres<-raster::extract(template,
                             sf::st_as_sf(NWAPLL, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
NWAPLL<-NWAPLL %>% mutate(unique=unique_pres,type=1)


# filter dates based on presence availability
udates <-unique(NWAPLL$date)
udates<-as.Date(udates)

# for loop below uses the generate_abs function that will generate the same number of pseudo-absences as the presences for each day from areas that there isn't a presence point

# ratio argument allows you to increase the number of pseudo-absences daily by a factor of whatever value you give it.

# Just in case I like to have more pseudo-absences so I sent the ratio argument to 3 to have a 1:3 presence:pseudo-absence ratio

absences<-data.frame()
for (i in 1:length(udates)) {
  ud<-udates[i]
  ab<-generate_abs(NWAPLL, ud, template, ratio = 3)
  absences<-rbind(absences,ab)
}


# combining presence and background dataframes
absences<-absences[,-c(4,5,6,8,9)]

# need a z column to match with presence df
absences<- remove_land(absences, bathy_file = bathy)
NWAPLL$date<-as.Date(NWAPLL$date)
NWAPLL<-NWAPLL[,c(1,3,2,4,6,5,8,9,7)]
#now we combine
Pres_Abs_NWAPLL_2013to2020<-rbind(NWAPLL,absences)


#make an id column. this will help when cbinding dataframes later
Pres_Abs_NWAPLL_2013to2020<-mutate(Pres_Abs_NWAPLL_2013to2020, id = row_number())


saveRDS(Pres_Abs_NWAPLL_2013to2020,here("data","AIS","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_1to3ratio_absenceconstrained_convexhull.rds"))





# Constructing a map to look at the data

#retrieving that data for the continents 
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, color= "black", fill = "grey" ) + #bring in world data
  #geom_sf(data = eezs, color = "blue") + #bring in EEZ data
  geom_point(data = Pres_Abs_NWAPLL_2013to2020, aes(x=lon, y=lat))+
  #geom_point(data = absences, aes(x=cell_ll_lon, y=cell_ll_lat, color = "black")) +
  coord_sf(xlim = c(-100,-30), ylim = c(10,50), expand = FALSE) + #changing the extent
  ylab("Latitude") + xlab("Longitude") + theme_bw()




