# Classifying monthly core fishing grounds

library(tidyverse)
library(raster)
library(zoo)
library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

source(here("scripts","2_VDM","functions","threshold_finder.r"))
source(here("scripts","2_VDM","functions","mgmt_area_CFGmetrics.r"))

#####################################################################
#  calculating monthly fishing ground suitability spatial predictions
#####################################################################

VDM_prediction_dir<-here("data","spatial_predictions","NEP_TROLL")


VDM_predictions<-list.files(VDM_prediction_dir, full.names = TRUE) %>%
  grep(pattern = ".nc", value = TRUE)

ras_stack_VDM <- VDM_predictions %>% stack()
idx<-format(as.Date(substr(VDM_predictions, 44,53)), format = "%Y-%m")
names(ras_stack_VDM)<-idx

# calcualting monthly means
VDM_month<-stackApply(ras_stack_VDM,idx, fun = mean)

# adding in a time variable
tt<-as.yearmon(unique(format(as.Date(substr(VDM_predictions, 44,53)), format = "%Y-%m")))
VDM_month<-setZ(VDM_month,tt)
names(VDM_month)<-as.character(tt)

# save these monthly predictions
writeRaster(VDM_month, here("data","spatial_predictions","NEP_TROLL", 
                            "NEP_TROLL_monthly_predictions_2012to2020"), 
            format='CDF', overwrite=TRUE)





# load in mgmt area shapefiles
NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


################################################################
# Identifying core fishing grounds & other metrics per mgmt area
################################################################

# first need to find threshold the upper 25% threshold to convert spatial predictions into binary maps

# daily VDM prediction raster path
VDMdaily<-here("data","spatial_predictions","NEP_TROLL")

# run the threshold finder function on NEP_TROLL predictions
NEP_TROLL_VDMthres<-threshold_finder(VDMdaily)

# mean 75% quantile raster
thresh<-NEP_TROLL_VDMthres %>% summarise(threshold = mean(quantile75, na.rm=T))

# now run the function that will calculate core fishing ground area and other metrics per mgmt area

NEP_TROLL_CFGmetrics_monthly<-mgmt_area_CFGmetrics(VDM_stack, NEP_TROLL_areas, thresh)

NEP_TROLL_CFGmetrics_monthly<-bind_rows(NEP_TROLL_CFGmetrics_monthly) %>% dplyr::select(1,2,3,39:51) %>%
  mutate(total.area= total.area/1000000)%>% #m2 o km2
  group_by(date,mgmt_area) %>% 
  mutate(total.area.mgmt.area = sum(total.area, na.rm = TRUE)) %>% 
  filter(class == 1)

saveRDS(NEP_TROLL_CFGmetrics_monthly, here("data","mgmt_area_metrics","NEP_TROLL","NEP_TROLL_CFGmetrics_monthly.rds"))
