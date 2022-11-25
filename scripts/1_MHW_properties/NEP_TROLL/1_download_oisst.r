#downloading OISST data per management area. 

#this is *largerly* leveraged from the heatwaveR vignettes and we would like to credit the developers of the package; Robert W. Schlegel & Albertus J. Smit

library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(sf)
library(here)

sf_use_s2(FALSE)# need to do this to remove spherical geometry

# mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_zones<-st_transform(NWA_PLL_zones)

#####
# CAR
#####

CAR<-NWA_PLL_zones %>% filter(ET_ID == "CAR") %>% st_bbox()
CARcrop_x <- st_bbox(CAR)[c(1,3)]
CARcrop_y <- st_bbox(CAR)[c(2,4)]


# The information for the NOAA OISST data. Making sure the location of NOAA OISST data is is currently available in ERDDAP server
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")


# Download function 
# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = CARcrop_y,
                       longitude = CARcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}

# Date range
# With our wrapper function written we would now need to run it several times in order to grab all of the OISST data from 1982-01-01 to 2019-12-31....the server does not like it when more than 9 years of consecutive data are requested. The server will also end a users connection after ~17 individual files have been requested. Because we can’t download all of the data in one request, and we can’t download the data one year at a time, we will need to make requests for multiple batches of data. To accomplish this we will create a dataframe of start and end dates that will allow us to automate the entire download while meeting the aforementioned criteria.

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

CAR_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(CAR_OISST_data, here("data","oisst","NWA_PLL","CAR_OISST.rds"))

rm(CAR_OISST_data, CAR, CARcrop_x, CARcrop_y)

#####
# FEC
#####

FEC<-NWA_PLL_zones %>% filter(ET_ID == "FEC") %>% st_bbox()
FECcrop_x <- st_bbox(FEC)[c(1,3)]
FECcrop_y <- st_bbox(FEC)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = FECcrop_y,
                       longitude = FECcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


FEC_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(FEC_OISST_data, here("data","oisst","NWA_PLL","FEC_OISST.rds"))

rm(FEC_OISST_data, FEC, FECcrop_x, FECcrop_y)

#####
# GOM
#####

GOM<-NWA_PLL_zones %>% filter(ET_ID == "GOM") %>% st_bbox()
GOMcrop_x <- st_bbox(GOM)[c(1,3)]
GOMcrop_y <- st_bbox(GOM)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = GOMcrop_y,
                       longitude = GOMcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


GOM_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(GOM_OISST_data, here("data","oisst","NWA_PLL","GOM_OISST.rds"))

rm(GOM_OISST_data, GOM, GOMcrop_x, GOMcrop_y)

#####
# MAB
#####

MAB<-NWA_PLL_zones %>% filter(ET_ID == "MAB") %>% st_bbox()
MABcrop_x <- st_bbox(MAB)[c(1,3)]
MABcrop_y <- st_bbox(MAB)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = MABcrop_y,
                       longitude = MABcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


MAB_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(MAB_OISST_data, here("data","oisst","NWA_PLL","MAB_OISST.rds"))

rm(MAB_OISST_data, MAB, MABcrop_x, MABcrop_y)

#####
# NEC
#####

NEC<-NWA_PLL_zones %>% filter(ET_ID == "NEC") %>% st_bbox()
NECcrop_x <- st_bbox(NEC)[c(1,3)]
NECcrop_y <- st_bbox(NEC)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = NECcrop_y,
                       longitude = NECcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


NEC_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(NEC_OISST_data, here("data","oisst", "NWA_PLL","NEC_OISST.rds"))

rm(NEC_OISST_data, NEC, NECcrop_x, NECcrop_y)

#####
# NED
#####

NED<-NWA_PLL_zones %>% filter(ET_ID == "NED") %>% st_bbox()
NEDcrop_x <- st_bbox(NED)[c(1,3)]
NEDcrop_y <- st_bbox(NED)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = NEDcrop_y,
                       longitude = NEDcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


NED_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(NED_OISST_data, here("data","oisst", "NWA_PLL","NED_OISST.rds"))

rm(NED_OISST_data, NED, NEDcrop_x, NEDcrop_y)

#####
# SAB
#####

SAB<-NWA_PLL_zones %>% filter(ET_ID == "SAB") %>% st_bbox()
SABcrop_x <- st_bbox(SAB)[c(1,3)]
SABcrop_y <- st_bbox(SAB)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = SABcrop_y,
                       longitude = SABcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


SAB_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(SAB_OISST_data, here("data","oisst","NWA_PLL","SAB_OISST.rds"))

rm(SAB_OISST_data, SAB, SABcrop_x, SABcrop_y)

#####
# SAR
#####

SAR<-NWA_PLL_zones %>% filter(ET_ID == "SAR") %>% st_bbox()
SARcrop_x <- st_bbox(SAR)[c(1,3)]
SARcrop_y <- st_bbox(SAR)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = SARcrop_y,
                       longitude = SARcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


SAR_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(SAR_OISST_data, here("data","oisst","NWA_PLL","SAR_OISST.rds"))

rm(SAR_OISST_data, SAR, SARcrop_x, SARcrop_y)
