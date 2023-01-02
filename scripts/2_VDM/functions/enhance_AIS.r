#function to enhance AIS data

enhance_AIS<-function(input_df, env_dir){
  dates <-unique(input_df$date) %>% as.Date() #get unique dates from df
  enhanced_df<-data.frame()#need a empty df
  
  #for loop that subsets input df daily 
  #then enhances that data specific to the same date of a raster file
  #rbinds it all at the end
  for (i in 1:length(dates)){ 
    day_subset<- filter(input_df, 
                        input_df$date==dates[i])
    
    day_subset<-sf::st_as_sf(day_subset, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    
    #bring in the same env date
    env_file_list <- grep(list.files(path = env_dir, full.names = TRUE, pattern = ".grd"), pattern = dates[i], value = TRUE)
    
    if(length(env_file_list)==0){
      next
    } 
    
    env_day_stack<-stack(env_file_list)
    
    pts.env<-raster::extract(env_day_stack,day_subset)
    
    day.pts.env<-cbind(sf::st_coordinates(day_subset),day_subset,pts.env)%>% dplyr::select(-c("geometry","z")) %>% mutate(id=day_subset$id)
    
    enhanced_df<-rbind(enhanced_df,day.pts.env)
  }
  return(enhanced_df) #returns the fully enhanced df
}