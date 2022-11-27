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

# mangement areas shapefile
NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

#####
# VN
#####

VN<-NEP_TROLL_areas %>% filter(ET_ID == "VN") %>% st_bbox()
VNcrop_x <- st_bbox(VN)[c(1,3)]
VNcrop_y <- st_bbox(VN)[c(2,4)]


# The information for the NOAA OISST data. Making sure the location of NOAA OISST data is is currently available in ERDDAP server
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")


# Download function 
# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = VNcrop_y,
                       longitude = VNcrop_x,
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

VN_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(VN_OISST_data, here("data","oisst","NEP_TROLL","VN_OISST.rds"))

rm(VN_OISST_data, VN, VNcrop_x, VNcrop_y)

#####
# CL
#####

CL<-NEP_TROLL_areas %>% filter(ET_ID == "CL") %>% st_bbox()
CLcrop_x <- st_bbox(CL)[c(1,3)]
CLcrop_y <- st_bbox(CL)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = CLcrop_y,
                       longitude = CLcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


CL_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(CL_OISST_data, here("data","oisst","NEP_TROLL","CL_OISST.rds"))

rm(CL_OISST_data, CL, CLcrop_x, CLcrop_y)

#####
# EK
#####

EK<-NEP_TROLL_areas %>% filter(ET_ID == "EK") %>% st_bbox()
EKcrop_x <- st_bbox(EK)[c(1,3)]
EKcrop_y <- st_bbox(EK)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = EKcrop_y,
                       longitude = EKcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


EK_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(EK_OISST_data, here("data","oisst","NEP_TROLL","EK_OISST.rds"))

rm(EK_OISST_data, EK, EKcrop_x, EKcrop_y)

#####
# MT
#####

MT<-NEP_TROLL_areas %>% filter(ET_ID == "MT") %>% st_bbox()
MTcrop_x <- st_bbox(MT)[c(1,3)]
MTcrop_y <- st_bbox(MT)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = MTcrop_y,
                       longitude = MTcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


MT_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(MT_OISST_data, here("data","oisst","NEP_TROLL","MT_OISST.rds"))

rm(MT_OISST_data, MT, MTcrop_x, MTcrop_y)

#####
# CP
#####

CP<-NEP_TROLL_areas %>% filter(ET_ID == "CP") %>% st_bbox()
CPcrop_x <- st_bbox(CP)[c(1,3)]
CPcrop_y <- st_bbox(CP)[c(2,4)]

OISST_sub_dl <- function(time_df, lat, lon){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = CPcrop_y,
                       longitude = CPcrop_x,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}


CP_OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)


saveRDS(CP_OISST_data, here("data","oisst", "NEP_TROLL","CP_OISST.rds"))

rm(CP_OISST_data, CP, CPcrop_x, CPcrop_y)
