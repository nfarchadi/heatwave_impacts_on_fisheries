# function to AIS points from land due to aggregating cells

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