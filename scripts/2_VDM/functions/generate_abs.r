# function to generate absences daily

generate_abs<-function(input_df, udates,template, ratio = 3){    
  subdate_presence<-filter(input_df, 
                           input_df$date==udates)
  
  subdate_absences=rasterToPoints(template) %>% as.data.frame()  %>% rename("unique"="Altitude") %>% as.data.frame() %>%
    filter(!(unique %in%subdate_presence$unique)) %>% mutate(type=0) %>%  ## only select pixels that don't have presences
    .[sample(nrow(.),nrow(subdate_presence)*ratio),] %>% 
    mutate(date=udates,geartype="background",flag="No_flag",
           hours=NA,fishing_hours=0,mmsi_present=0,name=NA) %>% 
    rename("lon"="x", "lat"="y")
  
  subdate_absences$month<-lubridate::month(subdate_absences$date)
  subdate_absences$year<-lubridate::year(subdate_absences$date)
  subdate_absences<-subdate_absences[,c(5,1,2,7,6,8,9,10,11,12,13,3,4)]
  
  return(subdate_absences)
}