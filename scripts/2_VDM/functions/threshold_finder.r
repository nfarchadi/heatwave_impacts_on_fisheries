# function to find the upper 25% threshold

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