#This code is to determine the US longline fishing ports.

#GFW global anchorages dataset 
ports<-read.csv("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/named_anchorages_v1_20191205.csv")
str(ports)

library(dplyr)
library(sf)
#USA ports
USAports<-subset(ports, iso3=="USA")
USA_atl_ports<-filter(ports, lat <= 60 & lat >= -15 , 
                      lon >= -100 & lon <= -10,iso3=="USA", at_dock=="TRUE",
                      distance_from_shore_m == 0)

#dont worry about this anymore
#ports_shp_WPI<-read_sf("C:/Users/nfarc/Downloads/WPI_Shapefile/WPI.shp")
#ports_shp_WPI<-filter(ports_shp_WPI, COUNTRY == "US", LATITUDE <= 60 & LATITUDE >= -15 , LONGITUDE >= -100 & LONGITUDE <= -10)

#Puerto Rico ports
PRI_ports<-filter(ports, lat <= 60 & lat >= -15 , 
                  lon >= -100 & lon <= -10, 
                  iso3=="PRI", at_dock=="TRUE",
                  distance_from_shore_m == 0)

#US Virgin Island ports
VIR_ports<-filter(ports, lat <= 60 & lat >= -15 , 
                  lon >= -100 & lon <= -10, 
                  iso3=="VIR", at_dock=="TRUE",
                  distance_from_shore_m == 0)

#Combining all the US longline potential ports
longline_ports_all<-rbind(USA_atl_ports, PRI_ports, VIR_ports)

##geocoding 
library(ggmap)
library(tidyverse)
library(RJSONIO)

#this is data I complied on ports that the US longline uses. Taken from various papers and Atlantic Highly Migratory Species Fishery Management Plan. this dataet is only places so I need to geocode to find the lat/lon

US_longline_ports<-read.csv("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/US_longline_ports.csv")

US_longline_cities<-paste(US_longline_ports$port.names,US_longline_ports$state.country, sep = ", ")

###THIS CODE I FOUND ONLINE
#Google Maps API limits querys to 2500 per day and fails to geocode smaller towns. So I wrote a function to geocode using Bing's API. It's also much faster.
geocode <- function( x, verbose=FALSE, service="google", returntype="coordinates", ... ) {
  UseMethod("geocode",x)
}
geocode.default <- function(x,verbose=FALSE, service="google", returntype="coordinates", ...) {
  if( is.na( x ) | gsub(" *", "", x) == ""  ) return(c(NA,NA))
  service <- tolower(service)
  BingMapsKey <- getOption("BingMapsKey")
  if(service=="bing" && is.null(BingMapsKey) ) stop("To use Bing, you must save your Bing Maps API key (obtain at http://msdn.microsoft.com/en-us/library/ff428642.aspx) using options(BingMapsKey='mykey').\n")
  construct.geocode.url <- list()
  construct.geocode.url[["google"]] <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))  
  }
  construct.geocode.url[["bing"]] <- function(address, maxResults=1) {
    root <- "http://dev.virtualearth.net/REST/v1/Locations"
    u <- paste0(root, "?query=", address, "&maxResults=",maxResults,"&key=",BingMapsKey)
    return(URLencode(u))
  }
  if(verbose) message(x,appendLF=FALSE)
  u <- construct.geocode.url[[service]](x)
  doc <- RCurl::getURL(u)
  j <- RJSONIO::fromJSON(doc,simplify = FALSE)
  parse.json <- list()
  parse.json[["google"]] <- function(j) {
    if(j$status=="OK") {
      res <- list()
      if( "coordinates" %in% returntype ) {
        lat <- j$results[[1]]$geometry$location$lat
        lng <- j$results[[1]]$geometry$location$lng
        res$coordinates <- c(lat, lng)
      }
      if( "zip" %in% returntype )  {
        zp <- j$results[[1]]$address_components[[8]]$short_name
        if( j$results[[1]]$address_components[[8]]$types[[1]] != "postal_code" )  warning(paste("Not sure these zips are actually zips.  Type:", j$results[[1]]$address_components[[8]]$types[[1]]) )
        res$zip <- zp
      }
      return( res )
    } else {
      if(j$status=="OVER_QUERY_LIMIT") warning("Google's geocoding quota appears to have been reached for the day.")
      return(c(NA,NA))
    }
  }
  parse.json[["bing"]] <- function(j) {
    if(j$authenticationResultCode != "ValidCredentials") {
      warning("Your BingMapsKey was not accepted.")
      return(c(NA,NA))
    }
    if(j$statusDescription!="OK") {
      warning("Something went wrong. Bing Maps API return status code ",j$statusCode," - ", j$statusDescription)
      return(c(NA,NA))
    }
    if(j$resourceSets[[1]]$estimatedTotal==0) {
      warning("Didn't find any points")
      return(c(NA,NA))
    }
    if(verbose) message(" - Confidence: ", j$resourceSets[[1]]$resources[[1]]$confidence ,appendLF=FALSE)
    res <- list()
    if( "coordinates" %in% returntype ) {
      crds <- unlist(j$resourceSets[[1]]$resources[[1]]$point$coordinates)
      res$coordinates <- crds
    }
    if( "zip" %in% returntype )  {
      res$zip <- sub( "^.*(\\d{5}-?\\d?\\d?\\d?\\d?).*$", "\\1", j$resourceSets[[1]]$resources[[1]]$address$formattedAddress )
    }
    return( res )
  }
  res <- parse.json[[service]](j)
  if(length(returntype)==1) res <- res[[1]]
  if(verbose) message("\n",appendLF=FALSE)
  return( res )
}

#Vectorize function
geocodeVect <- Vectorize(geocode, vectorize.args="x")

#Batch geocode with Bing
options(BingMapsKey="AqLYaCKhMvnKCwSrPnrxDqMr5wHrxkPxE0QaI06RoBtSjlkVe17eqlfIFfw42Lo7")

data<-geocodeVect(US_longline_cities, service="bing", 
                  returntype="coordinates")
data <-as.data.frame(t(as.data.frame(data)))
data $location<-rownames(data)
rownames(data)<-NULL
colnames(data)<-c("lat","lon","port.name")
US_longline_ports<-cbind(US_longline_ports,data[,1:2])
#save the data
saveRDS(US_longline_ports,"C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/US_longline_ports_geocoded.rds")


###now I want to get all points (anchorages from GFW dataset) in a 50km buffer from the geocoded dataset
US_longline_ports_spdf<-US_longline_ports
US_longline_ports_spdf <- SpatialPoints(US_longline_ports_spdf[,c("lon","lat")], CRS("+proj=longlat +datum=WGS84")) #take you long and lat in decimal degrees and make a spatial data frame
US_longline_ports_spdf <- spTransform(US_longline_ports_spdf, CRS("+proj=cea +datum=WGS84 +ellps=WGS84 +units=m")) #transform into cylindrical equal area AKA "cea"
port.buf <- gBuffer(US_longline_ports_spdf, width = 50000) #making 50 km buffer 
port.buf.sf<-st_as_sf(port.buf)

longline_ports_all_spdf<-longline_ports_all
longline_ports_all_spdf <- SpatialPoints(longline_ports_all_spdf[,c("lon","lat")], CRS("+proj=longlat +datum=WGS84")) #take you long and lat in decimal degrees and make a spatial data frame
longline_ports_all_spdf <- spTransform(longline_ports_all_spdf, CRS("+proj=cea +datum=WGS84 +ellps=WGS84 +units=m")) #transform into cylindrical equal area AKA "cea"


port.buf.intersects <- gIntersects (port.buf, longline_ports_all_spdf, byid=TRUE)
port.selection <- longline_ports_all[as.vector(port.buf.intersects), ]

#points in the 50 km buffer
saveRDS(port.selection,"C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/US_atl_longline_ports_50kmbuffer.rds")

###-----------now I want to make a distance to port raster------#####
# create unique values raster
library(raster)
template<-raster("D:/Env_rasters/HYCOM/water_temp/hycom_temp_2012-01-01.nc") ## making a SST raster the template
atl_ocean<-readOGR("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/Atlantic_Ocean_shp/iho.shp") #need a mask for only points in the atlantic ocean
template<-mask(template,atl_ocean)#take out pacific ocean points

#creating a spatial object
port.selection_shp<-port.selection
coordinates(port.selection_shp)<-~lon+lat
#setting the coordinate the same as port.selection_shp
proj4string(port.selection_shp) <- CRS(proj4string(template))
dist_port<-distanceFromPoints(template,port.selection_shp)
dist_port_masked<-mask(dist_port,template)
dist_port_masked<-dist_port_masked*0.001 #converting to km

#save as raster
writeRaster(dist_port_masked, "C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NWA_dis_port_masked.nc", format='CDF', overwrite=TRUE)

#want to create thersholded distance to port rasters
Presences_onlyFishing_all <- readRDS("C:/Users/nfarc/Desktop/NASA_FaCeT/Data/Processed_AIS/Presences_onlyFishing_all.rds")
Presences_onlyFishing_all_ex<-Presences_onlyFishing_all
coordinates(Presences_onlyFishing_all_ex)<-c("lon_bin", "lat_bin")
#have to make a NWA_XXX_ex the same projection as bathy
crs(Presences_onlyFishing_all_ex)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "

pts.ports<-raster::extract(dist_port_masked,Presences_onlyFishing_all_ex)
NWA_longline.ports<-data.frame(cbind(coordinates(Presences_onlyFishing_all_ex),pts.ports))
Presences_onlyFishing_all[,c(10)]<-NWA_longline.ports[,c(3)]
colnames(Presences_onlyFishing_all)[10]<-c("dis_port")
summary(Presences_onlyFishing_all$dis_port)

#1st Qu. = 113.146
dis1<-dist_port_masked
dis1[dis1 > 113] <- 113
writeRaster(dis1, "C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NWA_dis_port_thresholdQ1.nc", format='CDF', overwrite=TRUE)
#Median = 156.325
dis2<-dist_port_masked
dis2[dis2 > 156] <- 156
writeRaster(dis2, "C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NWA_dis_port_thresholdMedian.nc", format='CDF', overwrite=TRUE)
#3rd Qu. = 290.870
dis3<-dist_port_masked
dis3[dis3 > 291] <- 291
writeRaster(dis3, "C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NWA_dis_port_thresholdQ3.nc", format='CDF', overwrite=TRUE)
#max = 2129.803
dismax<-dist_port_masked
dismax[dismax > 2130] <- 2130
writeRaster(dismax, "C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NWA_dis_port_thresholdMax.nc", format='CDF', overwrite=TRUE)
#mean = 403.334
dismean<-dis_port_masked
dismean[dismean > 403] <- 403
writeRaster(dismean, "C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NWA_dis_port_thresholdMean.nc", format='CDF', overwrite=TRUE)

#loading packages 
library(sf) #for mapping
library(rnaturalearth)#package that provides data of the world. Used for mapping`
library(dplyr) # for the pipe operator (%>%)
library(lubridate)
library(rgdal)
library(rgeos)

#port shapefile
ports_shp<-read_sf("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/ne_50m_ports/ne_50m_ports.shp")


#retreiving that data for the continents 
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#inputting EEZ data
eezs <- read_sf('E:/VDMs/VDMs/World_EEZ_v11_20191118', layer = 'eez_v11') %>% 
  filter(TERRITORY1== "United States") # select the 200 nautical mile polygon layer


ggplot() +
  geom_sf(data = world, color= "black", fill = "grey" ) + #bring in world data
  geom_sf(data = eezs, color = "blue") + #bring in EEZ data
  geom_point(port.selection, mapping=aes(x=lon, y=lat), color = "red")+
  #geom_point(data = USA_atl_ports, aes(x=lon, y=lat), color = "red") +
  #geom_point(data = PRI_ports, aes(x=lon, y=lat), color = "yellow") +
  #geom_point(data = VIR_ports, aes(x=lon, y=lat), color = "blue") +
  #geom_sf(data = port.buf.sf, color= "purple")+
  #geom_point(data = US_longline_ports, aes(x=lon, y=lat), color = "green",size=1)+
  #geom_sf(data = ports_shp_WPI, color = "green")+
  coord_sf(xlim = c(-100,-60), ylim = c(15,50), expand = FALSE) + #changing the extent
  ylab("Latitude") + xlab("Longitude")+ theme_bw()


ggsave(path="C:/Users/nfarc/Desktop/NASA_FaCeT/Plots", 
       filename = "US_atlantic_longline_ports.png",
       width=12, height=12, units='in')



