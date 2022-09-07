#downloading OISST data
#this is *largerly* taken from the heatwaveR vignettes

library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(sf)
library(here)
library(beepr)

sf_use_s2(FALSE)# need to do this to remove spherical geometry

#mangement zones shapefile
NEP_TROLL_zones<-here("data","shapefiles","NEP_TROLL","Zones_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NEP_TROLL_zones<-st_transform(NEP_TROLL_zones)

####----Conception----####
Conception<-NEP_TROLL_zones %>% filter(ET_ID == "Conception") %>% st_bbox()
Conceptioncrop_x <- st_bbox(Conception)[c(1,3)]
Conceptioncrop_y <- st_bbox(Conception)[c(2,4)]

#Before we begin downloading the subsetted data for our study area we need to make sure that they are currently available on an ERDDAP server. The location of the NOAA OISST data has changed in the past so it should not be assumed that the current location will exist in perpetuity.

# The information for the NOAA OISST data
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")


#Download function 
# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = Conceptioncrop_y,
                       longitude = Conceptioncrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    dplyr::select(lon, lat, t, temp) %>% 
    na.omit()
}

#Date range
#With our wrapper function written we would now need to run it several times in order to grab all of the OISST data from 1982-01-01 to 2019-12-31....the server does not like it when more than 9 years of consecutive data are requested. The server will also end a users connection after ~17 individual files have been requested. Because we can’t download all of the data in one request, and we can’t download the data one year at a time, we will need to make requests for multiple batches of data. To accomplish this we will create a dataframe of start and end dates that will allow us to automate the entire download while meeting the aforementioned criteria.

# Date download range by start and end dates per year
dl_years <- data.frame(date_index = 1:6,
                       start = as.Date(c("1982-01-01", "1990-01-01", 
                                         "1998-01-01", "2006-01-01", 
                                         "2014-01-01", "2020-01-01")),
                       end = as.Date(c("1989-12-31", "1997-12-31", 
                                       "2005-12-31", "2013-12-31", 
                                       "2019-12-31", "2020-12-31")))


# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed

Conception_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  dplyr::select(lon, lat, t, temp)


saveRDS(Conception_OISST_data, here("data","water_temp","NEP","oisst",
                             "Conception_OISST.rds"))

rm(Conception_OISST_data, Conception, Conceptioncrop_x, Conceptioncrop_y)


####----Monterey----####
Monterey<-NEP_TROLL_zones %>% filter(ET_ID == "Monterey") %>% st_bbox()
Montereycrop_x <- st_bbox(Monterey)[c(1,3)]
Montereycrop_y <- st_bbox(Monterey)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = Montereycrop_y,
                       longitude = Montereycrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    dplyr::select(lon, lat, t, temp) %>% 
    na.omit()
}


Monterey_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  dplyr::select(lon, lat, t, temp)


saveRDS(Monterey_OISST_data, here("data","water_temp","NEP","oisst",
                             "Monterey_OISST.rds"))

rm(Monterey_OISST_data, Monterey, Montereycrop_x, Montereycrop_y)

####----Eureka----#### 
Eureka<-NEP_TROLL_zones %>% filter(ET_ID == "Eureka") %>% st_bbox()
Eurekacrop_x <- st_bbox(Eureka)[c(1,3)]
Eurekacrop_y <- st_bbox(Eureka)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = Eurekacrop_y,
                       longitude = Eurekacrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    dplyr::select(lon, lat, t, temp) %>% 
    na.omit()
}


Eureka_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  dplyr::select(lon, lat, t, temp)

beep(2)

saveRDS(Eureka_OISST_data, here("data","water_temp","NEP","oisst",
                             "Eureka_OISST.rds"))

rm(Eureka_OISST_data, Eureka, Eurekacrop_x, Eurekacrop_y)

####----Columbia----####
Columbia<-NEP_TROLL_zones %>% filter(ET_ID == "Columbia") %>% st_bbox()
Columbiacrop_x <- st_bbox(Columbia)[c(1,3)]
Columbiacrop_y <- st_bbox(Columbia)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = Columbiacrop_y,
                       longitude = Columbiacrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    dplyr::select(lon, lat, t, temp) %>% 
    na.omit()
}


Columbia_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  dplyr::select(lon, lat, t, temp)

beep(2)

saveRDS(Columbia_OISST_data, here("data","water_temp","NEP","oisst",
                             "Columbia_OISST.rds"))

rm(Columbia_OISST_data, Columbia, Columbiacrop_x, Columbiacrop_y)

####----Vancouver----####
Vancouver<-NEP_TROLL_zones %>% filter(ET_ID == "Vancouver") %>% st_bbox()
Vancouvercrop_x <- st_bbox(Vancouver)[c(1,3)]
Vancouvercrop_y <- st_bbox(Vancouver)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = Vancouvercrop_y,
                       longitude = Vancouvercrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    dplyr::select(lon, lat, t, temp) %>% 
    na.omit()
}


Vancouver_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  dplyr::select(lon, lat, t, temp)

beep(2)

saveRDS(Vancouver_OISST_data, here("data","water_temp","NEP","oisst",
                             "Vancouver_OISST.rds"))

rm(Vancouver_OISST_data, Vancouver, Vancouvercrop_x, Vancouvercrop_y)
