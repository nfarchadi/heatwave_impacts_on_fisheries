# function to find the 90 percentile seasonally varying threshold

find_seasonal_threshold<-function(data,thresh = .90){
  
  seas_list<-list()
  
  for (i in 1:12){
    if(i == 1){
      sea_month_df<-data %>%
        group_by(lat,lon) %>%
        filter(year <= 2011 & (month == 12 | month <=2)) %>% 
        summarise(seas = quantile(temp_anomaly, thresh, na.rm=TRUE),.groups = "drop") %>% 
        mutate(month = 1)
      
    } else if(i == 12){
      sea_month_df<-data %>%
        group_by(lat,lon) %>%
        filter(year <= 2011 & (month == 1 | month >= 11)) %>% 
        summarise(seas = quantile(temp_anomaly, thresh, na.rm=TRUE),.groups = "drop") %>% 
        mutate(month = i)
    } else{
      
      sea_month_df<-data %>%
        group_by(lat,lon) %>%
        filter(year <= 2011 & month >=i-1 & month <= i+1) %>% 
        summarise(seas = quantile(temp_anomaly, thresh, na.rm=TRUE),.groups = "drop") %>% 
        mutate(month = i)
    }
    
    seas_list[[length(seas_list)+1]]=sea_month_df
  }
  return(seas_list)
}