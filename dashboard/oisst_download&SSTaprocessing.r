library(HMMoce)
library(beepr)

#######################
#downloading OISST data
#######################
oisst.dir<-"E:/OISST/"

sp.lim <- list(lonmin = -100, lonmax = -30, latmin = 10, latmax = 50)

sst.udates<-seq(as.Date('1982-01-01'),as.Date('2020-12-31'), by="day")

get.env(sst.udates, filename='oisst', type = 'sst', sst.type='oi', spatLim = sp.lim, save.dir = oisst.dir)
beepr::beep(2)

########################
#calculate monthly means
########################
idx<-format(sst.udates, format = "%Y-%m") %>% unique() #year-mon index

oisst_files<-list.files(oisst.dir, full.names = TRUE) %>%
  grep(pattern = ".nc", value = TRUE)

oisst.monthly.dir<-paste(oisst.dir,"monthly/",sep = "")  #creating new file path
dir.create(oisst.monthly.dir, recursive = TRUE)

#function to grab files by year-mon, take the mean, then write raster file
oisst_monthavg<-function(idx, file_paths, output){
  
  for (i in 1:length(idx)){
    
    print(idx[i])
    
    #bring in the env by year-mon and make into rasterstack
    oisst.monthly.stack <- grep(oisst_files, 
                            pattern = idx[i], value = TRUE) %>% raster::stack()
    
    #calculate mean of rasterstack
    oisst.monthly.mean<-raster::calc(oisst.monthly.stack, fun = mean, na.rm = TRUE)
    
    #save year-mon mean as raster
    raster::writeRaster(oisst.monthly.mean, paste0(oisst.monthly.dir,"oisst_",idx[i]), format='CDF', overwrite=TRUE)
    }
  }

oisst_monthavg(idx,oisst_files,oisst.monthly.dir)

############
#climatology
############

#only wants dates from <=2011
sst.clim.dates<-sst.udates[sst.udates <= as.Date('2011-12-31')]

idx<-format(sst.clim.dates, format = "%Y-%m") %>% unique() #year-mon index

oisst_files<-list.files(oisst.monthly.dir, full.names = TRUE) %>%
  grep(pattern = ".nc", value = TRUE)

file_dates <- substr(oisst_files, 24,30)

oisst_files<-oisst_files[file_dates %in% idx]

oisst.climatology.dir<-paste(oisst.dir,"climatology/",sep = "")  #creating new file path
dir.create(oisst.climatology.dir, recursive = TRUE)

idx<-format(sst.clim.dates, format = "%m") %>% unique() #mon index

#function to grab files by mon, take the mean, then write raster file
oisst_climatology<-function(idx, file_paths, output){
  
  for (i in 1:length(idx)){
    
    print(idx[i])
    
    #bring in the env by year-mon and make into rasterstack
    oisst.climatology.stack <- grep(file_paths, 
                                pattern = paste0("-",idx[i]), value = TRUE) %>% raster::stack()
    
    #calculate mean of rasterstack
    oisst.climatology.mean<-raster::calc(oisst.climatology.stack, fun = mean, na.rm = TRUE)
    
    #save year-mon mean as raster
    raster::writeRaster(oisst.climatology.mean, paste0(oisst.climatology.dir,"oisst_",idx[i]), format='CDF', overwrite=TRUE)
  }
}

oisst_climatology(idx,oisst_files,oisst.climatology.dir)

##################
#monthly anomalies
##################
idx<-format(sst.udates, format = "%Y-%m") %>% unique() #year-mon index

oisst_monthly_files<-list.files(oisst.monthly.dir, full.names = TRUE) %>%
  grep(pattern = ".nc", value = TRUE)

oisst_climatology_files<-list.files(oisst.climatology.dir, full.names = TRUE) %>%
  grep(pattern = ".nc", value = TRUE)

oisst.ssta.dir<-paste(oisst.dir,"SSTa/",sep = "")  #creating new file path
dir.create(oisst.ssta.dir, recursive = TRUE)

#function to grab files year-mon and clim files, subtract them, then write raster file
oisst_ssta<-function(idx, monthly_file_paths, climatology_file_paths, output){
  
  for (i in 1:length(idx)){
    
    print(idx[i])
    
    #bring in the year-mon and make into rasterstack
    oisst.monthly<- grep(monthly_file_paths, 
                                    pattern = idx[i], value = TRUE) %>% raster::raster()
    
    #bring in the climatology 
    oisst.climatology <- grep(climatology_file_paths, 
                                pattern = substr(idx[i],6,7), value = TRUE) %>% raster::raster()
    
    #subtract
    oisst.ssta<-oisst.monthly - oisst.climatology
    
    
    #save year-mon ssta as raster
    raster::writeRaster(oisst.ssta, paste0(oisst.ssta.dir,"oisst_ssta_",idx[i]), format='CDF', overwrite=TRUE)
  }
}

oisst_ssta(idx,oisst_monthly_files,oisst_climatology_files,oisst.ssta.dir)



