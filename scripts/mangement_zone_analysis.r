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

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NWA_PLL_zones<-sf::st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/NWA_PLL_MangementZone/Zones_PLL.shp", crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NWA_PLL_zones<-st_transform(NWA_PLL_zones)

NWA_PLL_zones$ET_ID<-c("CAR","FEC","GOM",
                       "MAB","NCA","NEC",
                       "NED","SAB","SAR",
                       "TUN","TUS")

NWA_PLL_zones<-NWA_PLL_zones[c(1:4,6:9),]

#############################################
#load in VDM and VDM anomaly raster stacks
#############################################

#VDM stack
VDM_stack<-stack("E:/VDM_results/NWA_PLL/monthly/predictions/NWA_PLL_monthly_predictions_2012to2020.nc")

tt<-as.yearmon(seq(as.Date("2012-01-01"),as.Date("2020-12-01"),by="month"))

VDM_stack<-setZ(VDM_stack,tt)

# #VDM anomaly stack
# VDM_anom_stack<-stack("E:/VDM_results/NWA_PLL/monthly/predictions/NWA_PLL_monthly_anom_predictions_2012to2020.nc")
# 
# tt<-as.yearmon(seq(as.Date("2012-01-01"),as.Date("2020-12-01"),by="month"))
# 
# VDM_anom_stack<-setZ(VDM_anom_stack,tt)


#############################################
#VDM landscape metrics per mgmt zone
#############################################


#need to find the upper 25% threshold 
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
# 
# 
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
      
       
      #this section is because the centroid analysis needs data to be in point format, not raster
      ras_date2=ras_date
      ras_date2[values(ras_date2)==0]=NA
      
      raspnt=rasterToPoints(ras_date2) %>% as.data.frame()
      n.cell_habitat<-raspnt %>% na.omit() %>% nrow()
      
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
      stat3<-stat3 %>% rename("mgmt_zone" = "V48",
                              "n.cell_habitat" = "V49")
      
      
      
      mgmt_metrics_list[[length(mgmt_metrics_list)+1]]=stat3

    }
  }
  return(mgmt_metrics_list)
}


#Monthly VDM landscape metrics
NWA_PLL_landscapemetrics_monthly<-mgmt_zone_landscapemetrics(VDM_stack,
                                                     NWA_PLL_zones, thresh)

NWA_PLL_landscapemetrics_monthly<-bind_rows(NWA_PLL_landscapemetrics_monthly) %>% dplyr::select(1,2,3,39:49) %>%
  mutate(total.area= total.area/1000000)%>% #m2 o km2
  group_by(date,mgmt_zone) %>% 
  mutate(total.area.mgmt.zone = sum(total.area, na.rm = TRUE)) %>% 
  filter(class == 1)

saveRDS(NWA_PLL_landscapemetrics_monthly, here("data","Mgmt_zone","NWA_PLL_landscapemetrics_monthly.rds"))



# #Monthly Anomaly VDM landscape metrics
# thresh1<-data.frame(threshold = 0)
# 
# NWA_PLL_landscapemetrics_monthly_anomaly<-mgmt_zone_landscapemetrics(VDM_anom_stack,NWA_PLL_zones,thresh1)
# 
# #SDMTools units are in m^2. I want to convert it to km^2
# #Need to divide by 1000000 (at least for the area metrics)
# NWA_PLL_landscapemetrics_monthly_anomaly<-bind_rows(NWA_PLL_landscapemetrics_monthly_anomaly) %>% 
#   dplyr::select(1,2,3,39:48) %>%
#   mutate(total.area= total.area/1000000)%>% #m2 o km2
#   group_by(date,mgmt_zone) %>% 
#   mutate(total.area.mgmt.zone = sum(total.area, na.rm = TRUE)) %>% 
#   filter(class == 1)
# 
# 
# saveRDS(NWA_PLL_landscapemetrics_monthly_anomaly, here("data","Mgmt_zone","NWA_PLL_landscapemetrics_monthly_anomaly.rds"))


#############################################
#SST landscape metrics
#############################################

#6) Monthly SST landscape metrics
NWA_SST<-stack("E:/HYCOM/water_temp_month/SST_month_stack_2012to2020.nc")


SSTharddrivepath<-"E:/HYCOM/sst_updated/"

SST_rasters<-list.files(SSTharddrivepath, full.names = TRUE) %>%
  grep(pattern = ".nc", value = TRUE)

tt<-as.yearmon(unique(format(as.Date(substr(SST_rasters, 33,42)), format = "%Y-%m")))
NWA_SST<-setZ(NWA_SST,tt)
names(NWA_SST)<-tt


mgmt_zone_SST<-function(stack,mgmt_zone){
  
  mgmt_metrics_list<-list()
  
  for(i in 1:nrow(mgmt_zone)){
    VDM_crop<-raster::crop(stack, mgmt_zone[i,])
    
    for (j in 1:nlayers(VDM_crop)){
      
      print(paste(getZ(VDM_crop)[[j]],mgmt_zone[i,]$ET_ID))
      ras_date <- VDM_crop[[j]]
      
      udate<-getZ(VDM_crop)[[j]]
      
      #mean SST
      mean_SST<-cellStats(ras_date, "mean")
      #max SST
      max_SST<-cellStats(ras_date, "max")
      
      #reclassify into 1s and 0s based on the preferred range
      # ras_date[values(ras_date) < 15]=0
      # ras_date[values(ras_date) > 24]=0
      # ras_date[values(ras_date)!=0]=1
      # 
      # ras_date2=ras_date
      # ras_date2[values(ras_date2)==0]=NA
      # 
      # raspnt=rasterToPoints(ras_date2) %>% as.data.frame()
      
      datt=data.frame(X25sst=quantile(ras_date)[2],
                      X75sst=quantile(ras_date)[4])
      
      datt=datt %>% mutate(meanSST = mean_SST,
                      maxSST = max_SST,
                      mgmt_zone = mgmt_zone[i,]$ET_ID,
                      date = udate)
      
      # stat=SDMTools::ClassStat(ras_date,cellsize = 0.08,bkgd=NA,latlon = TRUE) %>% filter(class==1) #calculate classStats from the SDMTools package
      # print("hihi")
      # stat2=cbind(stat) %>% mutate(date=udate)
      # stat3=cbind(stat2,datt)
      # 
      # stat3[,42]<-mgmt_zone[i,]$ET_ID
      # stat3[,43]<-mean_SST
      # stat3[,44]<-max_SST
      # stat3<-stat3 %>% rename("mgmt_zone" = "V42", "mean_SST"="V43",
      #                         "max_SST"="V44")
      
      
      mgmt_metrics_list[[length(mgmt_metrics_list)+1]]=datt
      
    }
  }
  return(mgmt_metrics_list)
}


NWA_SST_monthly_metrics<-mgmt_zone_SST(NWA_SST,NWA_PLL_zones)

NWA_SST_monthly_metrics<-bind_rows(NWA_SST_monthly_metrics)

#############################################
#SST anomaly landscape metrics
#############################################
#7) Monthly SST anomaly landscape metrics
NWA_SST_anom<-stack("E:/HYCOM/water_temp_month/SST_month_anom_stack_2012to2020.nc")

NWA_SST_anom<-setZ(NWA_SST_anom,tt)
names(NWA_SST_anom)<-tt


mgmt_zone_SST_anom<-function(stack,mgmt_zone,thresh){
  
  mgmt_metrics_list<-list()
  
  for(i in 1:nrow(mgmt_zone)){
    VDM_crop<-raster::crop(stack, mgmt_zone[i,])
    
    for (j in 1:nlayers(VDM_crop)){
      
      print(paste(getZ(VDM_crop)[[j]],mgmt_zone[i,]$ET_ID))
      ras_date <- VDM_crop[[j]]
      
      udate<-getZ(VDM_crop)[[j]]
      
      ras_date2<-ras_date
      
      #reclassify into 1s and 0s based on the threshold
      ras_date[values(ras_date)>0]=1 
      ras_date[values(ras_date)<0]=0
      
      
      if(all(values(ras_date)==0,na.rm=TRUE)){
        print(paste("Skipped",getZ(VDM_crop)[[j]],mgmt_zone[i,]$ET_ID))
        next
      }
      
      
      #mean and max SST anom of warmer waters
      ras_date3<-ras_date
      ras_date3[values(ras_date3)==0]=NA
      ras_date3<-ras_date3*ras_date2
      #mean SST
      mean_SST_anom<-cellStats(ras_date3, "mean")
      #max SST
      max_SST_anom<-cellStats(ras_date3, "max")
      
      SSTanom25=quantile(ras_date2)[2] %>% as.vector() #25% quantile
      SSTanom75=quantile(ras_date2)[4] %>% as.vector()#75% quantile
      

      stat=SDMTools::ClassStat(ras_date,cellsize = 0.08,bkgd=NA,latlon = TRUE) #%>% filter(class==1) #calculate classStats from the SDMTools package
      
      
      stat=stat %>% mutate(date=udate)
      stat[,40]<-SSTanom25
      stat[,41]<-SSTanom75
      stat[,42]<-mgmt_zone[i,]$ET_ID
      stat[,43]<-mean_SST_anom
      stat[,44]<-max_SST_anom
      stat<-stat %>% rename("SSTanom25" = "V40",
                            "SSTanom75" = "V41",
                            "mgmt_zone" = "V42",
                            "mean_SST_anom"="V43",
                            "max_SST_anom"="V44")
      
      mgmt_metrics_list[[length(mgmt_metrics_list)+1]]=stat
      
    }
  }
  return(mgmt_metrics_list)
}


NWA_SST_anom_monthly_metrics<-mgmt_zone_SST_anom(NWA_SST_anom,NWA_PLL_zones,0)


NWA_SST_anom_monthly_metrics<-bind_rows(NWA_SST_anom_monthly_metrics) %>% 
  dplyr::select(1,2,3,39:44) %>% mutate(total.area= total.area/1000000) %>% 
  group_by(date,mgmt_zone) %>% 
  mutate(total.area.mgmt.zone = sum(total.area, na.rm = TRUE)) %>% 
  filter(class == 1)

saveRDS(NWA_SST_anom_monthly_metrics, here("data","Mgmt_zone","NWA_SST_anom_monthly_metrics.rds"))

#############################################
#Visualizations
#############################################


#suitable fishing habitat area ~ area of anomalously warmer water
NWAPLL_lsm_month<-NWA_PLL_landscapemetrics_monthly %>% 
  dplyr::select(date,total.area,mgmt_zone, total.area.mgmt.zone) %>% 
  rename("NWA_PLL.total.area"="total.area") %>% 
  mutate(prop_habitat_area = NWA_PLL.total.area/total.area.mgmt.zone)

NWASSTANOM_lsm_month<-NWA_SST_anom_monthly_metrics %>% 
  dplyr::select(date,total.area,mgmt_zone, total.area.mgmt.zone, 
         mean_SST_anom, max_SST_anom) %>%
  rename("NWA_SST.total.area" = "total.area") %>% 
mutate(prop_sstanom_area = NWA_SST.total.area/total.area.mgmt.zone)

NWAPLL_SSTanom<-left_join(NWAPLL_lsm_month,NWASSTANOM_lsm_month,
                          by= c("date","mgmt_zone")) %>% na.omit()

saveRDS(NWAPLL_SSTanom, here("data","Mgmt_zone","NWAPLL_SSTanom.rds"))

#proportion plot
ggplot(NWAPLL_SSTanom, aes(x=prop_sstanom_area, y=prop_habitat_area,
                           color = max_SST_anom))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_viridis(name=expression("SST Anomaly"),option ="inferno",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 20))+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x=expression("Proportion of Anomalous SST"~(km^2)), y=expression("Proportion of Suitable Fishing Habitat"~(km^2)))

#total area plot
ggplot(NWAPLL_SSTanom, aes(x=NWA_SST.total.area, y=NWA_PLL.total.area,
                           color = mean_SST_anom))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_viridis(name=expression("SST Anomaly"),option ="inferno",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 20))+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x=expression("Area of Anomalous SST"~(km^2)), y=expression("Area of Suitable Fishing Habitat"~(km^2)))





#center of gravity between MHW years vs normal
quant90SST_mgmtzone<-NWA_SST_monthly_metrics %>% group_by(mgmt_zone) %>% 
  summarise(quant90=quantile(meanSST, probs = 0.90)) %>% 
  left_join(.,NWA_SST_monthly_metrics, by = "mgmt_zone") %>% 
  mutate(MHW = ifelse(meanSST > quant90,1,0)) 


NWAPLL_lsm_month<-NWA_PLL_landscapemetrics_monthly %>% 
  mutate(month=lubridate::month(date),
         year=lubridate::year(date)) %>% 
  left_join(., quant90SST_mgmtzone, by = c("mgmt_zone","date")) %>% 
  group_by(MHW,mgmt_zone) %>% 
  summarise(COGx=mean(COGx, na.rm=TRUE),
            COGy=mean(COGy, na.rm=TRUE),
            X25=mean(X25, na.rm=TRUE),
            X75=mean(X75, na.rm=TRUE),
            Y25=mean(Y25, na.rm=TRUE),
            Y75=mean(Y75, na.rm=TRUE))


ggplot(data = NWAPLL_lsm_month) +
   #bring in world data +
  geom_sf(data = NWA_PLL_zones, color = "blue", fill=NA)+  #bring in EEZ data
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_point(aes(COGx,COGy, color = as.factor(MHW)),shape = 3)+
  geom_errorbar(aes(x=COGx, ymin=Y25, ymax=Y75,color=as.factor(MHW)), 
                 show.legend = NA)+
  geom_errorbarh(aes(y=COGy, xmin=X25, xmax=X75,color=as.factor(MHW)),
                show.legend = NA)+
  coord_sf(xlim = c(-100, -20), ylim = c(0, 55), expand = TRUE) +
  ylab("Latitude") + xlab("Longitude") + theme_bw() +
  #scale_color_discrete(name = "Climate Regime")+
  scale_color_manual(name = "Climate Regime",
                     labels = c("Near-Average", "MHW"), 
                     values = c("blue", "red"))
  




#habitat change (winners and losers plot)
NWAPLL_total.area_monthly<-NWA_PLL_landscapemetrics_monthly %>% 
  mutate(month=lubridate::month(date),
         year=lubridate::year(date)) %>% 
  left_join(., quant90SST_mgmtzone, by = c("mgmt_zone","date")) %>% 
  select(date,MHW,mgmt_zone,total.area)

NWAPLL_total.area_mgmtzone<-NWA_PLL_landscapemetrics_monthly %>% 
  mutate(month=lubridate::month(date),
         year=lubridate::year(date)) %>% 
  left_join(., quant90SST_mgmtzone, by = c("mgmt_zone","date")) %>%
  group_by(mgmt_zone) %>% 
  summarise(total.area_mean=mean(total.area, na.rm=TRUE))

habitat_change<-NWAPLL_total.area_monthly %>% 
  left_join(., NWAPLL_total.area_mgmtzone, by = c("mgmt_zone")) %>% 
  mutate(month=lubridate::month(date),
         year=lubridate::year(date)) %>%
  group_by(year,mgmt_zone) %>% 
  summarise(total.area = mean(total.area,na.rm=TRUE),
            total.area_mean = mean(total.area_mean, na.rm=TRUE),
            MHW = sum(MHW, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(habitat_change = ((total.area - total.area_mean) / total.area_mean)*100,
         MHW= ifelse(MHW > 1, 1, 0),
         MHW = as.factor(MHW))



ggplot(habitat_change, aes(x=year,y=habitat_change, fill = MHW))+
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_manual(values = c("blue", "red"))+
  facet_wrap(~mgmt_zone, scales = "free_y", ncol = 1)+
  theme_bw()+
  ggtitle("Habitat change (year - mean / mean)") +
  labs(y="Percentage change", x="Date") +theme(legend.position="")+
  coord_flip()


#lets at look at this data now
#NWA_PLL_mgmtzones
#NWA_PLL_anom_mgmtzones
#NWA_PLL_landscapemetrics

NWA_PLL_mgmtzones %>% head()

ggplot(NWA_PLL_anom_mgmtzones, aes(x=date, y=mean_suitability))+
  geom_line()+
  facet_wrap(~mgmt_zone)


ggplot(NWA_PLL_landscapemetrics, aes(x=date, y=total.area/1000))+
  geom_line()+
  facet_wrap(~mgmt_zone)

