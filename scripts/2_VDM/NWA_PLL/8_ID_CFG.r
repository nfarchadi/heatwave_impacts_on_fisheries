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

VDM_prediction_dir<-here("data","spatial_predictions","NWA_PLL")


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
writeRaster(VDM_month, here("data","spatial_predictions","NWA_PLL", 
                            "NWA_PLL_monthly_predictions_2012to2020"), 
            format='CDF', overwrite=TRUE)





# load in mgmt area shapefiles
NWA_PLL_areas<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


################################################################
# Identifying core fishing grounds & other metrics per mgmt area
################################################################

# first need to find threshold the upper 25% threshold to convert spatial predictions into binary maps

# daily VDM prediction raster path
VDMdaily<-here("data","spatial_predictions","NWA_PLL")

# run the threshold finder function on NWA_PLL predictions
NWA_PLL_VDMthres<-threshold_finder(VDMdaily)

# mean 75% quantile raster
thresh<-NWA_PLL_VDMthres %>% summarise(threshold = mean(quantile75, na.rm=T))

# now run the function that will calculate core fishing ground area and other metrics per mgmt area

NWA_PLL_CFGmetrics_monthly<-mgmt_area_CFGmetrics(VDM_stack, NWA_PLL_areas, thresh)

NWA_PLL_CFGmetrics_monthly<-bind_rows(NWA_PLL_CFGmetrics_monthly) %>% dplyr::select(1,2,3,39:51) %>%
  mutate(total.area= total.area/1000000)%>% #m2 o km2
  group_by(date,mgmt_area) %>% 
  mutate(total.area.mgmt.area = sum(total.area, na.rm = TRUE)) %>% 
  filter(class == 1)

saveRDS(NWA_PLL_CFGmetrics_monthly, here("data","mgmt_area_metrics","NWA_PLL","NWA_PLL_CFGmetrics_monthly.rds"))
