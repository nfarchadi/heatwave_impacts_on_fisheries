#Identifying MHWs in a mangement zone

library(sf)
library(raster)
library(tidyverse)
library(glue)
library(zoo)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_zones<-st_transform(NWA_PLL_zones)


#############################################
#load in VDM and VDM anomaly raster stacks
#############################################

#VDM stack
VDM_stack<-here("data","Monthly_Spatial_Predictions","NWA_PLL",
                "NWA_PLL_monthly_predictions_2012to2020.nc") %>% stack()

tt<-as.yearmon(seq(as.Date("2012-01-01"),as.Date("2020-12-01"),by="month"))

VDM_stack<-setZ(VDM_stack,tt)

#VDM anomaly stack
VDM_anom_stack<-here("data","Monthly_Spatial_Predictions","NWA_PLL",
                     "NWA_PLL_monthly_anom_predictions_2012to2020.nc") %>% stack()

tt<-as.yearmon(seq(as.Date("2012-01-01"),as.Date("2020-12-01"),by="month"))

VDM_anom_stack<-setZ(VDM_anom_stack,tt)


#############################################
#VDM landscape metrics per mgmt zone function
#############################################

#function to calculate landscape metrics per mgmt zone
mgmt_zone_landscapemetrics<-function(stack,mgmt_zone,thresh){
  
  mgmt_metrics_list<-list()
  
  for(i in 1:nrow(mgmt_zone)){
    VDM_crop<-raster::crop(stack, mgmt_zone[i,])
    
    for (j in 1:nlayers(VDM_crop)){
      
      print(paste(getZ(VDM_crop)[[j]],mgmt_zone[i,]$ET_ID))
      ras_date <- VDM_crop[[j]]
      
      udate<-getZ(VDM_crop)[[j]]
      
      #reclassify into 1s and 0s based on the threshold
      ras_date[values(ras_date)>=thresh$threshold]=1 
      ras_date[values(ras_date)<thresh$threshold]=0
      
      #want to know the total amount of cells for entire zone and each class
      n.cell_habitat<-ncell(ras_date[values(ras_date)==1])
      n.cell_nohabitat<-ncell(ras_date[values(ras_date)==0])
      n.cell_total<-ncell(ras_date)
      
      
      #this section is because the centroid analysis needs data to be in point format, not raster
      ras_date2=ras_date
      ras_date2[values(ras_date2)==0]=NA
      
      
      raspnt=rasterToPoints(ras_date2) %>% as.data.frame()
      
      
      datt=data.frame(X25=quantile(raspnt$x)[2],
                      X75=quantile(raspnt$x)[4],
                      Y25=quantile(raspnt$y)[2],
                      Y75=quantile(raspnt$y)[4])
      
      
      stat=SDMTools::ClassStat(ras_date,cellsize = 0.08,bkgd=NA,latlon = TRUE) #%>% filter(class==1) #calculate classStats from the SDMTools package
      grav=SDMTools::COGravity(ras_date2) %>% as.data.frame() %>% t() %>% as.data.frame() # calculate centroids
      stat2=cbind(stat,grav) %>% mutate(date=udate)
      stat3=cbind(stat2,datt)
      stat3[,48]<-mgmt_zone[i,]$ET_ID
      stat3[,ncol(stat3)+1]<-n.cell_habitat
      stat3[,ncol(stat3)+1]<-n.cell_nohabitat
      stat3[,ncol(stat3)+1]<-n.cell_total
      stat3<-stat3 %>% rename("mgmt_zone" = "V48",
                              "n.cell_habitat" = "V49",
                              "n.cell_nohabitat" = "V50",
                              "n.cell_total" = "V51")
      
      
      
      mgmt_metrics_list[[length(mgmt_metrics_list)+1]]=stat3
      
    }
  }
  return(mgmt_metrics_list)
}

#########################################
#function to find the upper 25% threshold
#########################3###############

threshold_finder<-function(VDM_path){
  VDMdaily_ras<-list.files(VDM_path, full.names = TRUE) %>%
    grep(pattern = ".nc", value = TRUE) 
  
  thresh_df<-list()
  
  for(i in 1:length(VDMdaily_ras)){
    print(VDMdaily_ras[i])
    
    ras<-raster(VDMdaily_ras[i])
    quant75<-quantile(ras$layer)[4]
    
    daily_thresh<-data.frame(quantile75=quant75,
                             date=as.Date(substr(VDMdaily_ras[i], 58,67)),format = "%Y-%m-%d")
    
    thresh_df[[length(thresh_df)+1]]<-daily_thresh
  }
  thresh_df<-bind_rows(thresh_df)
  return(thresh_df)
  
}

# Daily VDM prediction raster path
VDMdaily<-"E:/VDM_results/NWA_PLL/spatial_predictions"

#run the threshold finder function on NWA_PLL predictions
NWA_PLL_VDMthres<-threshold_finder(VDMdaily)

#mean 75% quantile raster
thresh<-NWA_PLL_VDMthres %>% summarise(threshold = mean(quantile75, na.rm=T))
 
 
# #now use the mean 75% quantile threshold to reclassify 
# VDMdaily_ras<-list.files(VDMdaily, full.names = TRUE) %>%
#   grep(pattern = ".nc", value = TRUE)
# 
# VDMdaily_ras_stack<- VDMdaily_ras %>% stack()
# idx<-as.Date(substr(VDMdaily_ras, 58,67))
# names(VDMdaily_ras_stack)<-idx
# 
# #adding in a time variable
# tt<-idx
# VDMdaily_ras_stack<-setZ(VDMdaily_ras_stack,tt)



##############################
#Monthly VDM landscape metrics
##############################

NWA_PLL_landscapemetrics_monthly<-mgmt_zone_landscapemetrics(VDM_stack,
                                                     NWA_PLL_zones, thresh)

NWA_PLL_landscapemetrics_monthly<-bind_rows(NWA_PLL_landscapemetrics_monthly) %>% dplyr::select(1,2,3,39:51) %>%
  mutate(total.area= total.area/1000000)%>% #m2 o km2
  group_by(date,mgmt_zone) %>% 
  mutate(total.area.mgmt.zone = sum(total.area, na.rm = TRUE)) %>% 
  filter(class == 1)

saveRDS(NWA_PLL_landscapemetrics_monthly, here("data","Mgmt_zone","NWA_PLL","NWA_PLL_landscapemetrics_monthly.rds"))


######################################
#Monthly anomaly VDM landscape metrics
######################################

thresh1<-data.frame(threshold = 0)

NWA_PLL_landscapemetrics_monthly_anomaly<-mgmt_zone_landscapemetrics(VDM_anom_stack,NWA_PLL_zones,thresh1)

#SDMTools units are in m^2. I want to convert it to km^2
#Need to divide by 1000000 (at least for the area metrics)
NWA_PLL_landscapemetrics_monthly_anomaly<-bind_rows(NWA_PLL_landscapemetrics_monthly_anomaly) %>%
  dplyr::select(1,2,3,39:51) %>%
  mutate(total.area= total.area/1000000)%>% #m2 o km2
  group_by(date,mgmt_zone) %>%
  mutate(total.area.mgmt.zone = sum(total.area, na.rm = TRUE)) %>%
  filter(class == 1)


saveRDS(NWA_PLL_landscapemetrics_monthly_anomaly, here("data","Mgmt_zone","NWA_PLL", "NWA_PLL_landscapemetrics_monthly_anomaly.rds"))

