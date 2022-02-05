library(raster)
library(rasterVis)
library(tidyverse)
library(zoo)
library(lubridate)
library(maps)
library(here)
#Heatwave maps 

##load in SST anoms
NWA_SST_anom<-here("data","water_temp_month","NWA","SST_month_anom_stack_2012to2020.nc") %>% stack()


SSTharddrivepath<-"E:/HYCOM/sst_updated/"

SST_rasters<-list.files(SSTharddrivepath) %>%
  grep(pattern = ".nc", value = TRUE) %>%
  grep(pattern = "", value = TRUE)

tt<-as.yearmon(unique(format(as.Date(substr(SST_rasters, 12,21)), format = "%Y-%m")))
NWA_SST_anom<-setZ(NWA_SST_anom,tt)
idx<-unique(format(as.Date(substr(SST_rasters, 12,21)),format = "%Y-%m"))
names(NWA_SST_anom)<-idx

# terra::time(NWA_SST_anom)<-tt
# names(NWA_SST_anom)<-idx
# varnames(NWA_SST_anom)<-"Monthly SST Anomaly"


#mean of SST during peak fishing season (July - October)
# time
tm <- seq(as.Date('2012-01-01'), as.Date('2020-12-01'), by = "months")
tm <-tm[month(tm)%in% c(7,8,9,10)]
tm<-as.yearmon(tm)

NWA_SST_anom_peakfishing<-subset(NWA_SST_anom, which(getZ(NWA_SST_anom) %in% tm))

NWA_SST_anom_peakfishing <- zApply(NWA_SST_anom_peakfishing, by=year, fun=mean, name='PeakFishing')

#writeRaster(NWA_SST_anom_peakfishing, paste("E:/HYCOM/water_temp_month/NWA_SST_anom_peakfishingseason_stack_2012_2020.nc"), format='CDF', overwrite=TRUE)

#I want to create kernal density plots for AIS data and compare it to the SST anomaly

#Load in raw AIS data
NWA_PLL<-here("data","AIS_processed", "NWA_PLL") %>% #locate PLL AIS data 
  list.files(full.names = TRUE) %>% 
  readRDS() %>% #load data
  filter(Pres_abs == 1) #only want presences



