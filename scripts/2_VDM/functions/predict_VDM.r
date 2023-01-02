#function to predict VDMs daily

predict_VDM<-function(dates, model, hycom_dir, port_dir, seamount_dir, output_dir){
  
  #for loop to do this whole thing
  for (i in 1:length(dates)){
    
    #bring in the env by same date
    env<-grep(list.files(path = hycom_dir, full.names = TRUE, pattern = ".grd"),
              pattern = udates[i], value = TRUE)
    
    if(length(env)==0){
      next
    }
    
    env<-stack(env)
    
    #only want n2, sst_sd, sst, ssh_sd, ssh, bathy, and rugosity
    env<-env[[c(4:8,11:12)]] 

    
    #lunar
    lunar <- env[[1]]
    lunar[] <- lunar::lunar.illumination(udates[i])
    
    env_day_stack<-stack(env,
                         port_dir,
                         seamount_dir,
                         lunar)
    
    
    if(length(names(env_day_stack))!=10){
      next
    } 
    
    print(dates[i]) #tells me it predicted this date
    
    names(env_day_stack)<-c("n2","sst_sd",
                            "sst","ssh_sd",
                            "ssh","bathy","rugosity",
                            "dis_port",
                            "dis_seamount",
                            "lunar")
    
    p <- raster::predict(env_day_stack, brt, n.trees = brt$gbm.call$best.trees, type = "response")
    
    writeRaster(p, paste0(output_dir,udates[i]), format='CDF', overwrite=TRUE)
    
  }
}