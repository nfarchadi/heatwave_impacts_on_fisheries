##geocoding 
library(ggmap)
library(tidyverse)
library(RJSONIO)

#US pacific albacore ports . Taken from Frawley et al. 2020: "4 ports with the most reported vessels in California (n = 101), Oregon (n = 184) and Washington (n = 160)"

US_ALB_ports<-read.csv("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/US_ALB_troll_ports.csv")

US_ALB_cities<-paste(US_ALB_ports$port.names,US_ALB_ports$state, sep = ", ")

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

data<-geocodeVect(US_ALB_cities, service="bing", 
                  returntype="coordinates")
data <-as.data.frame(t(as.data.frame(data)))
data $location<-rownames(data)
rownames(data)<-NULL
colnames(data)<-c("lat","lon","port.name")
US_ALB_ports<-cbind(US_ALB_ports,data[,1:2])

#save the data
saveRDS(US_ALB_ports,"C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/US_ALB_ports_geocoded.rds")

###-----------now I want to make a distance to port raster------#####
# create unique values raster
library(raster)
library(here)

US_ALB_ports<-readRDS("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/US_ALB_ports_geocoded.rds")

template<-raster("E:/HYCOM_NEP/hycom_combine_2012-01-01.grd") ##need to template raster


#creating a spatial object
US_ALB_ports<-sf::st_as_sf(US_ALB_ports, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


dist_port<-distanceFromPoints(template,US_ALB_ports)
dist_port_masked<-mask(dist_port,template)
dist_port_masked<-dist_port_masked*0.001 #converting to km

#save as raster
writeRaster(dist_port_masked, "C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/NEP_ALB_dis_port_masked.nc", format='CDF', overwrite=TRUE)

