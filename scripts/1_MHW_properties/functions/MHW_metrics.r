# function to measure MHW intensity and size

MHW_metrics<-function(data){
  
  SSTa_metrics_list<-list()
  
  mgmt_areas<-data$ET_ID %>% unique()
  
  for(i in 1:length(mgmt_areas)){
    oisst_mgmtarea<-data %>% filter(ET_ID == mgmt_areas[i])
    
    dates<-oisst_mgmtarea %>% filter(year >=2012) %>% 
      pull(yearmon) %>% unique()
    
    for(j in 1:length(dates)){
      print(paste(dates[j], mgmt_areas[i], sep= " "))
      
      OISST_date<-oisst_mgmtarea %>% filter(yearmon == dates[j])
      
      OISST_date2<-OISST_date
      
      #binary classify anomalies
      OISST_date<-OISST_date %>% 
        dplyr::select(lon,lat,MHW)
      
      OISST_date<-raster::rasterFromXYZ(OISST_date)
      
      
      if(all(raster::values(OISST_date)==0,na.rm=TRUE)){
        print(paste("Skipped",dates[j]))
        next
      }
      
      OISST_date2<-OISST_date2 %>% 
        filter(MHW == 1)
      
      #mean and max SST anom of warmer waters
      mean_SSTa<-OISST_date2 %>% summarise(mean_SSTa = mean(temp_anomaly, na.rm=TRUE))
      max_SSTa<-OISST_date2 %>% summarise(max_SSTa = max(temp_anomaly, na.rm=TRUE))
      
      #want to know the total amount of cells for entire area and each class
      n.cell_MHW<-ncell(OISST_date[values(OISST_date)==1])
      n.cell_noMHW<-ncell(OISST_date[values(OISST_date)==0])
      n.cell_totalMHW<-ncell(OISST_date)
      
      
      SSTa_25=quantile(OISST_date2$temp_anomaly)[2] %>% as_tibble() %>% setNames("SSTa_25") #25% quantile
      SSTa_75=quantile(OISST_date2$temp_anomaly)[4] %>% as_tibble() %>% setNames("SSTa_75") #%>% as.vector() #75% quantile
      
      
      stat=SDMTools::ClassStat(OISST_date,cellsize = 0.25,bkgd=NA,latlon = TRUE) #%>% filter(class==1) #calculate classStats from the SDMTools package
      
      
      stat=stat %>% mutate(date=dates[j])
      stat[,ncol(stat)+1]<-SSTa_25
      stat[,ncol(stat)+1]<-SSTa_75
      stat[,ncol(stat)+1]<-mean_SSTa
      stat[,ncol(stat)+1]<-max_SSTa
      stat[,ncol(stat)+1]<-n.cell_MHW
      stat[,ncol(stat)+1]<-n.cell_noMHW
      stat[,ncol(stat)+1]<-n.cell_totalMHW
      stat[,ncol(stat)+1]<-mgmt_areas[i]
      
      SSTa_metrics_list[[length(SSTa_metrics_list)+1]]=stat
      
    }
  }
  return(SSTa_metrics_list)
}