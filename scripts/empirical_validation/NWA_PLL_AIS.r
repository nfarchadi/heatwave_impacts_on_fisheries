#validating the metrics empirically using the read data
library(tidyverse)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(zoo)
library(here)
library(sf)
library(tidyverse)

world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

################################
#load aggregate NWA PLL AIS data
################################

NWA_PLL<-here("data","AIS_processed","NWA_PLL","AIS_USA_NWA_PLL_2013_2020.rds") %>% 
  readRDS()

#need to reduce the resolution to 0.08 degrees to match ALB
res <- 0.08

NWA_PLL<-NWA_PLL %>% 
  mutate(lat = floor(lat/res) * res + 0.5 * res,
         lon = floor(lon/res) * res + 0.5 * res) %>%
  mutate(date = zoo::as.yearmon(date)) %>%  #date is now year-month
  group_by(date, lat, lon) %>%
  summarise(fishing_hours = sum(fishing_hours, na.rm = T), .groups = "drop") %>%
  filter(fishing_hours >= 1) %>% #here is where filter above 1 hour
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))


########################
#removing points on land
########################

#Since I reduced the resolution some points are now on land so we want to remove those. I made a function that uses bathymetry raster to determine if points are on land or not. 


bathy_file<-"C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/ETOPO1/NWA_bathy_largerextent.nc"

remove_land<- function(input_df, bathy_file){
  AIS_df<-input_df
  AIS_df_2<-AIS_df
  AIS_df_2<-sf::st_as_sf(AIS_df_2, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
  bathy<-raster::raster(bathy_file)
  
  pts.bathy<-raster::extract(bathy,AIS_df_2)
  fleet.bathy<-data.frame(cbind(sf::st_coordinates(AIS_df_2),pts.bathy))
  
  variable<-"z"
  AIS_df[,variable]<-fleet.bathy[,c(3)]
  AIS_df <- AIS_df[which(AIS_df$z < 0),]#only bathy less than 
  
  return(AIS_df)
}


NWA_PLL<- remove_land(NWA_PLL, bathy_file = bathy_file)

#####################
#rasterize each month
#####################

udates<-NWA_PLL %>% pull(date) %>% unique()

NWA_PLL_stack<-raster::stack()

bathy<-raster(bathy_file)
# bathy <- disaggregate(bathy,fact=8,method='bilinear')

for(i in 1:length(udates)){
  xy<-NWA_PLL %>% 
    filter(date == udates[i]) %>% 
    dplyr::select(lon,lat) %>% 
    rename("x"="lon",
           "y"="lat")
  
  r<-raster::rasterize(xy,raster(bathy_file), field = 1)
  NWA_PLL_stack<-raster::addLayer(NWA_PLL_stack,r)
}


names(NWA_PLL_stack)<-as.character(udates)
NWA_PLL_stack<-setZ(NWA_PLL_stack,udates)


#############################
#AIS management zone analysis
#############################

NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","Zones_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#trying to clip the management zones to the coast

land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_zones<-st_difference(NWA_PLL_zones, st_union(st_combine(land)))



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
      ras_date[values(ras_date)>=thresh]=1 
      ras_date[values(ras_date)<thresh]=0
      
      
      #want to know the total amount of cells for entire zone and each class
      n.cell_habitat<-ncell(ras_date[values(ras_date)==1])
      n.cell_nohabitat<-ncell(ras_date[values(ras_date)==0])
      n.cell_total<-ncell(ras_date)
      
      if(n.cell_habitat == 0){
        next
      }
      
      
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
      stat3[,ncol(stat3)+1]<-mgmt_zone[i,]$ET_ID
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


##############################
#Monthly VDM landscape metrics
##############################
thresh <- 1

NWA_PLL_AIS_landscapemetrics_monthly<-mgmt_zone_landscapemetrics(NWA_PLL_stack,
                                                             NWA_PLL_zones, thresh)

NWA_PLL_AIS_landscapemetrics_monthly<-bind_rows(NWA_PLL_AIS_landscapemetrics_monthly) %>% dplyr::select(1,2,3,39:51) %>%
  mutate(total.area= total.area/1000000)%>% #m2 o km2
  group_by(date,mgmt_zone) %>% 
  mutate(total.area.mgmt.zone = sum(total.area, na.rm = TRUE)) %>% 
  filter(class == 1)

#calculate percent change in fishing grounds
NWAPLLa_AIS_total<-NWA_PLL_AIS_landscapemetrics_monthly %>%
  mutate(month = lubridate::month(date)) %>% 
  group_by(mgmt_zone,month) %>% 
  mutate(year = lubridate::year(date),
         month_mean = mean(n.cell_habitat, na.rm=TRUE),
         month_SD = mean(n.cell_total, na.rm=TRUE),
         percent_change =
           ((n.cell_habitat - month_mean)/month_mean)*100) %>% 
  ungroup() %>% 
  mutate(change_direction = if_else(percent_change <= 0, 1,0)) 

#bring in MHW properties metrics from another dataset
NWAPLLa_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLLa_MHW_total.rds") %>%
  readRDS() %>%
  rename("MHW" = "MHW.x") %>% 
  dplyr::select(prop_MHW_cells,mean_SSTa,sequence_months,mgmt_zone,date,MHW)

#left join those MHW properties with the NWA PLL AIS landscape metrics
NWAPLLa_AIS_total<-left_join(NWAPLLa_AIS_total, NWAPLLa_MHW_total, by=c("mgmt_zone","date"))
  

  