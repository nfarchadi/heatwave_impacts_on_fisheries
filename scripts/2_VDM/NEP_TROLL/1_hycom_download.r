#download hycom environmental data for the northeast pacific

library(tidyverse)
library(raster)
library(HMMoce)
library(here)
library(foreach)
library(doParallel)

#####################################
# downloading env variables from HYOM
#####################################

envdir<-here("data","hycom","NEP_TROLL")

# start and end dates at daily resolution
st_date<-as.Date("2012-01-02")
en_date<-as.Date("2012-01-02")
udates <-seq(st_date,en_date, by = "day")

# loop to download u/v current, SSH, water_temp, and salinity 
for (i in 1:length(udates)){
  print(udates[i])
  ## 1) water_u -> surface only
  myURL <- try(HMMoce:::get.hycom(limits=c(-109, -180, 60, 17), time=udates[i],
                                  download.file=F, dir='~/NASA-FaCeT',
                                  vars='water_u', depLevels=1), TRUE) 
  # build the URL
  if (class(myURL) == 'try-error') next
  download.file(myURL, destfile = paste(envdir, '/hycom_u_', udates[i], '.nc',
                                        sep=''), method = 'curl')
  
  ## 2) water_v -> surface only
  myURL <- try(HMMoce:::get.hycom(limits=c(-109, -180, 60, 17), time=udates[i], 
                                  download.file=F, dir='~/NASA-FaCeT', 
                                  vars='water_v', depLevels=1), TRUE) 
  # build the URL
  if (class(myURL) == 'try-error') next
  download.file(myURL, destfile = paste(envdir, '/hycom_v_', udates[i], '.nc', 
                                        sep=''),method = 'curl')
  
  ## 3) surface elevation -> surface only
  myURL <- try(HMMoce:::get.hycom(limits=c(-109, -180, 60, 17), time=udates[i], 
                                  download.file=F, dir='~/NASA-FaCeT', 
                                  vars='surf_el', depLevels=1), TRUE) 
  # build the URL
  if (class(myURL) == 'try-error') next
  download.file(myURL, destfile = paste(envdir, '/hycom_ssh_', udates[i], '.nc', 
                                        sep=''),method = 'curl')
  
  ## 4) water_temp -> depths
  myURL <- try(HMMoce:::get.hycom(limits=c(-109, -180, 60, 17), time=udates[i], 
                                  download.file=F, dir='~/NASA-FaCeT', 
                                  vars='water_temp'), TRUE) 
  # build the URL
  if (class(myURL) == 'try-error') next
  download.file(myURL, destfile = paste(envdir, '/hycom_temp_', udates[i], '.nc',
                                        sep=''),method = 'curl')
  
  ## 5) salinity -> depths
  myURL <- try(HMMoce:::get.hycom(limits=c(-109, -180, 60, 17), time=udates[i], 
                                  download.file=F, dir='~/NASA-FaCeT', 
                                  vars='salinity'), TRUE) 
  # build the URL
  if (class(myURL) == 'try-error') next
  download.file(myURL, destfile = paste(envdir, '/hycom_sal_', udates[i], '.nc',
                                        sep=''),method = 'curl')
  
}


##########################################
# calculate derived environmental variables
##########################################

source(here("functions","calculate_eke.r"))
source(here("functions","calculate_ild.r"))
source(here("functions","calculate_n2.r"))

# 6) SST_SD

# get a list of downloaded sst files
sstList <- list.files(path = here("data","hycom","NEP_TROLL")) %>% 
  grep(pattern = "temp",value = TRUE)



## calculate SST_SD
for (i in 1:length(sstList)) {
  sst <- raster::raster(here("data","hycom","NEP_TROLL", sstList[i]))
  sst_sd <- raster::focal(sst, w = matrix(1, nrow = 3, ncol = 3), 
                          fun = function(x){stats::sd(x, na.rm = T)})
  fname <- paste0('hycom_SST_SD', 
                 substr(sstList[i], 11, nchar(sstList[i])))
  raster::writeRaster(sst_sd, filename = here("data","hycom","NEP_TROLL", fname), 
                      overwrite=T)
}

# 7) SSH_SD

# get a list of downloaded ssh files
sshList <- list.files(path = here("data","hycom","NEP_TROLL")) %>% 
  grep(pattern = "ssh",value = TRUE)

## calculate SSH_SD
for (i in 1:length(sshList)){
  ssh <- here("data","hycom","NEP_TROLL", sshList[i]) %>% 
    raster::raster()
  ssh_sd <- raster::focal(ssh, w = matrix(1, nrow = 3, ncol = 3),
                          fun = function(x){stats::sd(x, na.rm = T)})
  fname <- paste0('hycom_SSH_SD', 
                 substr(sshList[i], 10, nchar(sshList[i])))
  raster::writeRaster(ssh_sd, filename = here("data","hycom","NEP_TROLL", fname), 
                      overwrite=T)
}

# 8) EKE

#get list of u and v files
water_u_files<-list.files(path = here("data","hycom","NEP_TROLL")) %>% 
  grep(pattern = "u", value = TRUE)

water_v_files<-list.files(path = here("data","hycom","NEP_TROLL")) %>% 
  grep(pattern = "v", value = TRUE)



## calculating EKE
for (i in 1:length(water_u_files)){
  for (j in i:length(water_v_files)){
    u <- raster::raster(here("data","hycom","NEP_TROLL", water_u_files[i]))
    v <- raster::raster(here("data","hycom","NEP_TROLL", water_v_files[j]))
    u_v <- raster::stack(u, v)
    eke <- calc(u_v, calculate_eke)
    fname <- paste0('hycom_EKE', 
                   substr(water_u_files[i], 8, nchar(water_u_files[i])))
    raster::writeRaster(eke, filename = here("data","hycom","NEP_TROLL", fname), 
                        overwrite=T)
  }
}


##################################################################################
#3D derived variables: isothermal layer depth (ILD) & bulk bouyancy frequency (n2)
##################################################################################

water_temp_files<-list.files(path = here("data","hycom","NEP_TROLL")) %>% 
  grep(pattern = "temp", value = TRUE)


salinity_files<-list.files(path = here("data","hycom","NEP_TROLL")) %>% 
  grep(pattern = "sal", value = TRUE)

# get depth var indices
nc <- here("data","hycom","NEP_TROLL",'hycom_temp_2012-01-01.nc') %>% 
  RNetCDF::open.nc()

ncnames = NULL
nmax <- RNetCDF::file.inq.nc(nc)$nvars-1 #this looks up how many variables and subtracts that by 1 (this is because the index goes from 0 to 5)
for(ii in 0:nmax) ncnames[ii + 1] <- RNetCDF::var.inq.nc(nc, ii)$name #this code tells get each variable name and put it in a list (starts from 0 to ___) 
dep.idx <- grep('dep', ncnames, ignore.case=TRUE) - 1 #getting the index for the variable depth in the list that was made above
dep <- RNetCDF::var.get.nc(nc, dep.idx) %>% 
  as.numeric()#read the contents of a netcdf and then take out the the depths 


# 9) ILD

for (i in 1:length(water_temp_files)){
  #raster brick for each of the depth level for water temp 
  wtemp <- here("data","hycom","NEP_TROLL", water_temp_files[i]) %>% 
    raster::brick(varname = 'water_temp') #* scale + offset
  #calcualte ild
  ild.5 <- calc(wtemp, calculate.ild)
  writeRaster(ild.5, paste0(here("data","hycom","NEP_TROLL", "hycom_ild5"), 
                            gsub("^.*?temp_","_",water_temp_files[i])), 
              format='CDF', overwrite = TRUE)
}


# 10) n2 


## Recommending parallelizing n2 calculation as it is computationally extensive

# detecting the number of cores
numCores<- detectCores()
# need to register the number of cores we want to use. 
registerDoParallel(cores = 8)

# making a dataframe of the two list
sst_sal_list<-data.frame(water_temp_files,salinity_files)

system.time({
  foreach(i = 1:nrow(sst_sal_list), .packages = c('raster','here')) %dopar% {
    wtemp_date <- sst_sal_list[i,c("water_temp_files")]
    wtemp <- raster::brick(here("data","hycom","NEP_TROLL",  wtemp_date),
                           varname = 'water_temp')
    sal_date <-sst_sal_list[i,c("salinity_files")]
    sal <- raster::brick(here("data","hycom","NEP_TROLL",  sal_date), 
                         varname = 'salinity')
    sst_sal_brick <- raster::stack(wtemp[[1:23]], sal[[1:23]])
    n2 <- calc(calc(sst_sal_brick, calculate_n2), fun = function(x) {mean(x, na.rm=T)})
    writeRaster(n2, paste0(here("data","hycom","NEP_TROLL", "hycom_n2"), 
                           gsub("^.*?temp_","_",wtemp_date[i])), 
                format='CDF', overwrite=TRUE)
  }
})