#function to calculate core fishing ground area & other metrics per mgmt area

mgmt_area_COGmetrics<-function(stack,mgmt_area,thresh){
  
  mgmt_metrics_list<-list()
  
  for(i in 1:nrow(mgmt_area)){
    VDM_crop<-raster::crop(stack, mgmt_area[i,])
    
    for (j in 1:nlayers(VDM_crop)){
      
      print(paste(getZ(VDM_crop)[[j]],mgmt_area[i,]$ET_ID))
      ras_date <- VDM_crop[[j]]
      
      udate<-getZ(VDM_crop)[[j]]
      
      #reclassify into 1s and 0s based on the threshold
      ras_date[values(ras_date)>=thresh$threshold]=1 
      ras_date[values(ras_date)<thresh$threshold]=0
      
      #want to know the total amount of cells for entire area and each class
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
      stat3[,48]<-mgmt_area[i,]$ET_ID
      stat3[,ncol(stat3)+1]<-n.cell_habitat
      stat3[,ncol(stat3)+1]<-n.cell_nohabitat
      stat3[,ncol(stat3)+1]<-n.cell_total
      stat3<-stat3 %>% rename("mgmt_area" = "V48",
                              "n.cell_habitat" = "V49",
                              "n.cell_nohabitat" = "V50",
                              "n.cell_total" = "V51")
      
      
      
      mgmt_metrics_list[[length(mgmt_metrics_list)+1]]=stat3
      
    }
  }
  return(mgmt_metrics_list)
}