#Load packages
library(raster)
library(spdplyr)
library(rgeos)
library(rgdal)
#devtools::install_github('camrinbraun/HMMoce', ref='dev')
library(HMMoce)

#Downloading HYCOM rasters for each env variable 

home_dir<-"E:/HYCOM"
setwd(home_dir)


## filter dates based on HYCOM availability
st_date<-as.Date("2020-12-14")
en_date<-as.Date("2020-12-31")
udates <-seq(st_date,en_date, by = "day")

## now we have dates, get env data. might as well do global given the spatial extent of the project is some atlantic and most of the pacific
for (i in 1:length(udates)){
  print(udates[i])
  ## water_u -> surface only
  myURL <- try(HMMoce:::get.hycom(limits=c(-100, -30, 10, 50), time=udates[i], download.file=F, dir='~/NASA-FaCeT', vars='water_u', depLevels=1), TRUE) ## build the URL
  if (class(myURL) == 'try-error') next
  download.file(myURL, destfile = paste(home_dir, '/water_u/hycom_u_', udates[i], '.nc', sep=''), method = 'curl')
  
  ## water_v -> surface only
  myURL <- try(HMMoce:::get.hycom(limits=c(-100, -30, 10, 50), time=udates[i], download.file=F, dir='~/NASA-FaCeT', vars='water_v', depLevels=1), TRUE) ## build the URL
  if (class(myURL) == 'try-error') next
  download.file(myURL, destfile = paste(home_dir, '/water_v/hycom_v_', udates[i], '.nc', sep=''),method = 'curl')
  
  ## surface elevation -> surface only
  myURL <- try(HMMoce:::get.hycom(limits=c(-100, -30, 10, 50), time=udates[i], download.file=F, dir='~/NASA-FaCeT', vars='surf_el', depLevels=1), TRUE) ## build the URL
  if (class(myURL) == 'try-error') next
  download.file(myURL, destfile = paste(home_dir, '/surf_el/hycom_ssh_', udates[i], '.nc', sep=''),method = 'curl')
  
  ## water_temp -> 3D
  myURL <- try(HMMoce:::get.hycom(limits=c(-100, -30, 10, 50), time=udates[i], download.file=F, dir='~/NASA-FaCeT', vars='water_temp'), TRUE) ## build the URL
  if (class(myURL) == 'try-error') next
  download.file(myURL, destfile = paste(home_dir, '/water_temp/hycom_temp_', udates[i], '.nc', sep=''),method = 'curl')
  
  ## salinity -> 3D
  myURL <- try(HMMoce:::get.hycom(limits=c(-100, -30, 10, 50), time=udates[i], download.file=F, dir='~/NASA-FaCeT', vars='salinity'), TRUE) ## build the URL
  if (class(myURL) == 'try-error') next
  download.file(myURL, destfile = paste(home_dir, '/salinity/hycom_sal_', udates[i], '.nc', sep=''),method = 'curl')
  
}

#####-------------CALCULATE DERIVED ENVIRONMENTAL VARIABLES-----######
#STANDARD DEVIATION OF 2D GRIDS

## get a list of downloaded sst files
setwd('E:/HYCOM/water_temp/')
sstList <- grep(list.files(path = paste(home_dir, '/water_temp', sep='')), pattern = "_sd", inv=TRUE, value = TRUE)




## SST SD

for (i in 1:length(sstList)) {
  sst <- raster::raster(sstList[i])
  sst_sd <- raster::focal(sst, w = matrix(1, nrow = 3, ncol = 3), fun = function(x) stats::sd(x, na.rm = T))
  fname <- paste(substr(sstList[i], 1, 10), '_sd', substr(sstList[i], 11, nchar(sstList[i])), sep='')
  raster::writeRaster(sst_sd, filename = fname, overwrite=T)
  
}


## SSH SD
setwd('E:/HYCOM/surf_el/')
sshList <- list.files('D:/Env_rasters/HYCOM/surf_el/')
sshList <- grep(list.files(path = paste(home_dir, '/surf_el', sep='')), pattern = "_sd", inv=TRUE, value = TRUE)
#sshList <- sshList[grep('ssh', sshList)]


for (i in 1:length(sshList)){
  ssh <- raster::raster(sshList[i])
  ssh_sd <- raster::focal(ssh, w = matrix(1, nrow = 3, ncol = 3), fun = function(x) stats::sd(x, na.rm = T))
  fname <- paste(substr(sshList[i], 1, 9), '_sd', substr(sshList[i], 10, nchar(sshList[i])), sep='')
  raster::writeRaster(ssh_sd, filename = fname, overwrite=T)
  
}



#3D DERIVED VARIABLES: ISOTHERMAL LAYER DEPTH & BUOYANCY FREQUENCY
water_temp_files<-grep(list.files(path = paste(home_dir, '/water_temp', sep='')), pattern = "_sd", inv=TRUE, value = TRUE)



salinity_files<-list.files(path = paste(home_dir, '/salinity', sep=''))
setwd(paste(home_dir, '/water_temp', sep=''))

# get depth var indices
nc <- RNetCDF::open.nc(paste(home_dir, '/water_temp/hycom_temp_2012-01-01.nc', sep=''))

ncnames = NULL
nmax <- RNetCDF::file.inq.nc(nc)$nvars-1 #this looks up how many variables and subtracts that by 1 (this is because the index goes from 0 to 5)
for(ii in 0:nmax) ncnames[ii + 1] <- RNetCDF::var.inq.nc(nc, ii)$name #this code tells get each variable name and put it in a list (starts from 0 to ___) 
dep.idx <- grep('dep', ncnames, ignore.case=TRUE) - 1 #getting the index for the variable depth in the list that was made above
dep <- as.numeric(RNetCDF::var.get.nc(nc, dep.idx))#read the contents of a netcdf and then take out the the depths 



## MLD, ILD and BBV (N2) calculations
for (i in 1:length(water_temp_files)){
  setwd(paste(home_dir, '/water_temp', sep=''))
  #below is making raster bricks for each of the depth level for sal and water temp 
  wtemp <- raster::brick(water_temp_files[i], varname = 'water_temp') #* scale + offset
  ## define a function to get ILD - 0.5 deg C
  fun <- function(x) {
    if (all(is.na(x))){
      NA
    } else{
      dep[which.min(abs(x[1] - x[2:40] - 0.5))]
    }
  } ## index of depth level closest to -0.5
  ild.5 <- calc(wtemp, fun)
  writeRaster(ild.5, paste(home_dir, '/ild5/hycom_ild5', gsub("^.*?temp_","_",water_temp_files[i]), sep=''), format='CDF', overwrite = TRUE)
}



## define a function to approx N2 for top 200m (e.g. index 1:23) using temp and salinity
water_temp_files<-grep(list.files(path = paste(home_dir, '/water_temp', sep='')), pattern = "_sd", inv=TRUE, value = TRUE)


salinity_files<-list.files(path = paste(home_dir, '/salinity', sep=''))
setwd(paste(home_dir, '/water_temp', sep=''))

fun <- function(x) {
  if (all(is.na(x))){
    rep(NA, length.out=length(dep[1:23]))
  } else if(any(is.na(x[1:4]))){ ## if HYCOM is NA in top 25 m, not worth trying (and failing) to calc N2. (I changed 1:10 to 1:4)-NF
    rep(NA, length.out=length(dep[1:23]))
  } 
  else{
    oce::swN2(pressure = dep[1:23], sigmaTheta = oce::swSigmaTheta(x[24:46], x[1:23], pressure = dep[1:23]))#, referencePressure = median(depth[1:23], na.rm = TRUE))) ##24:26 is salinity layers, 1:23 is temp (or depth)
  }
}

#checking to see if we have the same dates for both water temp and salinity files in order to make sure they line up for thus n2 analysis
fun_check<- function (x){
  if (gsub("^.*?temp_","",water_temp_files[i])== gsub("^.*?sal_","",salinity_files[j])){
    print(paste("yay",gsub("^.*?temp_","",water_temp_files[i]),gsub("^.*?sal_","",salinity_files[j]),sep = " "))
    yay<-paste("yay",gsub("^.*?temp_","",water_temp_files[i]),gsub("^.*?sal_","",salinity_files[j]),sep = " ")
  } else{
    print(paste("nay",gsub("^.*?temp_","",water_temp_files[i]),gsub("^.*?sal_","",salinity_files[j]),sep = " "))
  }
}

check_for_dates<-for (i in 1:length(water_temp_files)){
  for (j in i:length(salinity_files)){
    fun_check()
    break
  }
}  


#running the 4 loop to calculate n2 for each day
#Doing this in parallel because it takes so long

#making a dataframe of the two list
sst_sal_list<-data.frame(water_temp_files,salinity_files)

#the two libraries we need for running in parallel
library(foreach)
library(doParallel)
#detecting the number of cores
numCores<- detectCores()
#need to register the number of cores we want to use. Could just put numCores that I have above or specify it like I did here
registerDoParallel(cores = 8)#by doing it this way I don't have to stopcluster(). "The doParallel package's .onUnload function will do this automatically if the cluster was created automatically by registerDoParallel, but if you created the cluster manually you should stop it using the stopCluster function"


system.time({
  foreach(i = 1:nrow(sst_sal_list), .packages = 'raster') %dopar% {
    setwd(paste(home_dir, '/water_temp', sep=''))
    wtemp_date <-sst_sal_list[i,c("water_temp_files")]
    wtemp <- raster::brick(wtemp_date, varname = 'water_temp')
    setwd(paste(home_dir, '/salinity', sep=''))
    sal_date <-sst_sal_list[i,c("salinity_files")]
    sal <- raster::brick(sal_date, varname = 'salinity')
    sst_sal_brick <- raster::stack(wtemp[[1:23]], sal[[1:23]])
    n2 <- calc(calc(sst_sal_brick, fun), fun = function(x) {mean(x, na.rm=T)})
    writeRaster(n2, paste(home_dir, '/n2/hycom_n2',
                          gsub("^.*?temp_","_",wtemp_date), sep=''), format='CDF', overwrite=TRUE)
  }
})



####------calculating EKE------####
#get list of u and v files
water_u_files<-list.files(path = paste(home_dir, '/water_u', sep=''))


water_v_files<-list.files(path = paste(home_dir, '/water_v', sep=''))


#eke function
fun <- function(x) {
  (x[[1]] ^ 2 + x[[2]] ^ 2) / 2
}


for (i in 1:length(water_u_files)){
  for (j in i:length(water_v_files)){
    setwd(paste(home_dir, '/water_u', sep=''))
    u <- raster::raster(water_u_files[i], varname = 'water_u')
    setwd(paste(home_dir, '/water_v', sep=''))
    v <- raster::raster(water_v_files[j], varname = 'water_v')
    u_v <- raster::stack(u, v)
    eke <- calc(u_v, fun)
    writeRaster(eke, paste(home_dir, '/eke/hycom_eke',
                           gsub("^.*?temp_","_",water_u_files[i]), sep=''), format='CDF', overwrite=TRUE)
    if(exists('eke')){
      break
    }
  }
}




####-------distance from semounts------####
#need a template
r <- raster("D:/Env_rasters/HYCOM/water_temp/hycom_temp_2012-01-01.nc")

seamounts_sf<-readOGR("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/ModelledSeamounts2011_v1/DownloadPack-14_001_ZSL002_ModelledSeamounts2011_v1/01_Data/Seamounts/Seamounts.shp")


seamounts_sf<-seamounts_sf %>% 
  filter(LAT <= 50 & LAT >= 10 , 
         LONG >= -100 & LONG <= -30)

#creating a distance to point raster
d1 <- distanceFromPoints(object = r, xy = seamounts_sf) 
d1

#need to mask d1 with the environmental raster so values over the land is taken out
seamount_dis<-mask(d1,r)
plot(seamount_dis)
seamount_dis
seamount_dis<-seamount_dis*0.001 #converting to km
writeRaster(seamount_dis, "C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance_from_seamount/dis_from_seamount_NWA_largerextent.nc", format='CDF')















