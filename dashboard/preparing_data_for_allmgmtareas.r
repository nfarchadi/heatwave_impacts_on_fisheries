library(here)
library(sf)
library(tidyverse)
library(raster)
sf_use_s2(FALSE)# need to do this to remove spherical geometry

############################
#bring in OISST data for NWA
############################
GOM_CAR_FEC_SAR_SAB_MHW <- here("data","water_temp","NWA","oisst","GOM_CAR_FEC_SAR_SAB_MHW.rds") %>% readRDS() 


 MAB_NEC_NED_MHW <- here("data","water_temp","NWA","oisst","MAB_NEC_NED_MHW.rds") %>% readRDS() 
##############################################
#remove variables and combine the two datasets
##############################################
MAB_NEC_NED_MHW <- MAB_NEC_NED_MHW %>% 
  dplyr::select(lon,lat,yearmon,temp_monthly)


GOM_CAR_FEC_SAR_SAB_MHW <- GOM_CAR_FEC_SAR_SAB_MHW %>% 
  dplyr::select(lon,lat,yearmon,temp_monthly)

All_areas<-rbind(MAB_NEC_NED_MHW,GOM_CAR_FEC_SAR_SAB_MHW)

rm(GOM_CAR_FEC_SAR_SAB_MHW,MAB_NEC_NED_MHW)

########################################
#function to find the seasonal threshold
########################################
find_seasonal_threshold<-function(data,thresh = .90){
  
  seas_list<-list()
  
  for (i in 1:12){
    if(i == 1){
      sea_month_df<-data %>%
        group_by(lat,lon) %>%
        filter(year <= 2011 & (month == 12 | month <=2)) %>% 
        summarise(seas = quantile(detrend, thresh, na.rm=TRUE),.groups = "drop") %>% 
        mutate(month = 1)
      
    } else if(i == 12){
      sea_month_df<-data %>%
        group_by(lat,lon) %>%
        filter(year <= 2011 & (month == 1 | month >= 11)) %>% 
        summarise(seas = quantile(detrend, thresh, na.rm=TRUE),.groups = "drop") %>% 
        mutate(month = i)
    } else{
      
      sea_month_df<-data %>%
        group_by(lat,lon) %>%
        filter(year <= 2011 & month >=i-1 & month <= i+1) %>% 
        summarise(seas = quantile(detrend, thresh, na.rm=TRUE),.groups = "drop") %>% 
        mutate(month = i)
    }
    
    seas_list[[length(seas_list)+1]]=sea_month_df
  }
  return(seas_list)
}

############################################################
#now lets calculate MHW timeseries for the entire NWA region
############################################################

#climatology
All_areas_clim<-All_areas %>% 
  mutate(month = lubridate::month(yearmon),
         year = lubridate::year(yearmon)) %>% 
  filter(year <= 2011) %>% 
  group_by(lon,lat,month) %>% 
  summarise(temp_clim = mean(temp_monthly, na.rm=TRUE), .groups = "drop")

#monthly anomalies
All_areas_anomalies<-All_areas %>% 
  mutate(month = lubridate::month(yearmon)) %>% 
  left_join(.,All_areas_clim, by=c("lon","lat","month")) %>%
  mutate(temp_anomaly = temp_monthly - temp_clim)


All_areas_anomalies_detrend<-All_areas_anomalies %>% 
  group_by(lon,lat) %>% 
  #mutate(detrend = detrend(temp_anomaly)) %>% ungroup()
  mutate(detrend = temp_anomaly) %>% ungroup()


#seasonal varying 90 percentile
All_areas_anomalies_detrend<-All_areas_anomalies_detrend %>%
  mutate(year = lubridate::year(yearmon)) 

All_areas_seasonal<-find_seasonal_threshold(All_areas_anomalies_detrend)

#define heatwave periods
All_areas_MHW<-bind_rows(All_areas_seasonal) %>% 
  right_join(.,All_areas_anomalies_detrend,by=c("lon","lat","month")) %>% 
  arrange(lat,lon,yearmon) %>% 
  mutate(MHW = if_else(detrend >= seas, 1, 0))

rm(All_areas, All_areas_anomalies, All_areas_anomalies_detrend, All_areas_clim,All_areas_seasonal)
###################################################
#need time series for MHW, SSTa, and seasonal_thres
###################################################

All_areas_MHW_ts<-All_areas_MHW %>% 
  filter(yearmon >= 2012) %>%
  group_by(yearmon) %>% 
  summarise(detrend = mean(detrend, na.rm=TRUE),
            seas = mean(seas, na.rm=TRUE), .groups = "drop") %>%
  mutate(MHW = ifelse(detrend >= seas, 1, 0),
         mgmt_zone = "All Management Areas")

rm(All_areas_MHW)

All_areas_MHW_ts$mgmt_zone<-as.factor(All_areas_MHW_ts$mgmt_zone)

All_areas_MHW_ts$date <- lubridate::parse_date_time(All_areas_MHW_ts$yearmon, tz='UTC', orders='mY')

All_areas_MHW_df<-All_areas_MHW_ts %>% dplyr::select(MHW,date,mgmt_zone) %>% 
  rename("MHW" = "MHW")

All_areas_ssta_ts_df<-All_areas_MHW_ts %>% dplyr::select(detrend,date,mgmt_zone) %>% 
  rename("ssta" = "detrend")

All_areas_seasonal_thresh_df<-All_areas_MHW_ts %>% dplyr::select(seas,date,mgmt_zone) %>% rename("seasonal_thresh" = "seas")


################################################################################
##################All management areas COG and habitat change###################
################################################################################

#############################################
#load in VDM and VDM anomaly raster stacks
#############################################
#VDM stack
VDM_stack<-here("data","Monthly_Spatial_Predictions","NWA_PLL",
                "NWA_PLL_monthly_predictions_2012to2020.nc") %>% stack()

tt<-zoo::as.yearmon(seq(as.Date("2012-01-01"),as.Date("2020-12-01"),by="month"))

VDM_stack<-setZ(VDM_stack,tt)

#############################################
#VDM landscape metrics per mgmt zone function
#############################################

#function to calculate landscape metrics for entire extent
mgmt_zone_landscapemetrics<-function(stack,thresh){
  
  mgmt_metrics_list<-list()
  
  for (j in 1:nlayers(stack)){
      
      print(paste(getZ(stack)[[j]]))
      ras_date <- stack[[j]]
      
      udate<-getZ(stack)[[j]]
      
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
      stat3[,48]<-"All Management Areas"
      stat3[,ncol(stat3)+1]<-n.cell_habitat
      stat3[,ncol(stat3)+1]<-n.cell_nohabitat
      stat3[,ncol(stat3)+1]<-n.cell_total
      stat3<-stat3 %>% rename("mgmt_zone" = "V48",
                              "n.cell_habitat" = "V49",
                              "n.cell_nohabitat" = "V50",
                              "n.cell_total" = "V51")
      
      
      
      mgmt_metrics_list[[length(mgmt_metrics_list)+1]]=stat3
      
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

##############################
#Monthly VDM landscape metrics
##############################

All_areas_landscapemetrics_monthly<-mgmt_zone_landscapemetrics(VDM_stack, thresh)

All_areas_landscapemetrics_monthly<-bind_rows(All_areas_landscapemetrics_monthly) %>% dplyr::select(1,2,3,39:51) %>%
  mutate(total.area= total.area/1000000)%>% #m2 o km2
  group_by(date) %>% 
  mutate(total.area.mgmt.zone = sum(total.area, na.rm = TRUE)) %>% 
  filter(class == 1)

#suitable fishing habitat area ~ area of anomalously warmer water
All_areas_lsm<-All_areas_landscapemetrics_monthly %>% 
  dplyr::select(date,total.area,mgmt_zone,total.area.mgmt.zone, 
                COGx, COGy, COGx.sd, COGy.sd,X25,X75,Y25,Y75,n.cell_habitat,
                n.cell_nohabitat,n.cell_total) %>% 
  rename("NWA_PLL.total.area"="total.area") %>% 
  mutate(prop_habitat_area = NWA_PLL.total.area/total.area.mgmt.zone,
         prop_habitat_cells = n.cell_habitat / n.cell_total)

All_areas_lsm<-All_areas_lsm %>% mutate(year = lubridate::year(date),
                                        month = lubridate::month(date))

All_areas_lsm$mgmt_zone<-as.factor(All_areas_lsm$mgmt_zone)

#calculate monthly anomaly
All_areas_change_df<-All_areas_lsm %>% 
  group_by(mgmt_zone,month) %>% 
  mutate(year = lubridate::year(date),
         month_mean = mean(n.cell_habitat, na.rm=TRUE),
         month_SD = mean(n.cell_total, na.rm=TRUE),
         NWA_PLL.area.anomaly =
           ((n.cell_habitat - month_mean)/month_mean)*100) %>% 
  ungroup() %>% 
  mutate(change_direction = if_else(NWA_PLL.area.anomaly <= 0, 1,0)) 

All_areas_change_df$mgmt_zone<-as.factor(All_areas_change_df$mgmt_zone)

All_areas_change_df$change_direction<-as.factor(All_areas_change_df$change_direction)

All_areas_change_df$date <- lubridate::parse_date_time(All_areas_change_df$date, tz='UTC', orders='mY')

All_areas_change_df<-All_areas_change_df %>% dplyr::select(NWA_PLL.area.anomaly,date,mgmt_zone) %>% 
  rename("habitat_change" = "NWA_PLL.area.anomaly")


#COG
All_areas_pts<-All_areas_lsm %>% 
  mutate(date = lubridate::parse_date_time(date, tz='UTC', orders='mY')) %>% 
  dplyr::select(COGx,COGy,date,mgmt_zone)


