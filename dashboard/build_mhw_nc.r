#' Aggregate and convert MHW results for manipulation and display via Dashboard
#' 
#' @param ssta_stack is a list of raster stacks where levels correspond to time steps in time_vec. List element is per-zone
#' @param hab_list is list of raster stacks where levels correspond to time steps in time_vec. List element is per-zone.
#' @param time_vec is vector of time levels (usually years).
#' @param mgmt_vec is vector of mgmt zone names
#' @param cog_pts a data.frame of COGx, COGy, year, zone. Zone must match one element of mgmt_vec.
#' @param change_df a data.frame of change, time, zone. Zone must match one element of mgmt_vec. Time should correspond to time steps in time vec. Change values are probably % change (negative for losses, positive for gains) for a given zone and time step.
#' @param MHW is a data.frame of MHW, time, zone. Zone must match one element of mgmt_vec. Time should correspond to time steps in time vec. MHW values are 1 or 0 to indicated if there are a heatwave or not, respectively, for a given zone and time step
#' @param ssta_ts is a data.frame of ssta, tima, zone. Zone must match one element of mgmt_vec. Time should correspond to time steps in time vec. ssta values are the average ssta for a given zone and time step
#' @param seasonal_thresh is a data.frame of seasonal_thresh, tima, zone. Zone must match one element of mgmt_vec. Time should correspond to time steps in time vec. seasonal_thresh values are the average 90% seasonal threshold for a given zone and time step which is used to identify marine heatwaves from ssta data
#' @param nc_file is output .nc file

build_mhw_nc <- function(ssta_list, hab_list, time_vec, mgmt_vec, cog_pts, change_df, MHW_df, ssta_ts_df, seasonal_thresh_df, nc_file){
  
  require(RNetCDF); require(sp); require(raster)
  ## lat x lon x time x mgmt zone
  
  #print(paste0('Your time levels are extracted from names(raster). They are ', names(ssta_stack)))
  
  nc <- create.nc(nc_file, format='netcdf4')
  cds <- coordinates(ssta_stack)
  lat_vals <- unique(cds[,2])
  lon_vals <- unique(cds[,1])
  
  dim.def.nc(nc, 'longitude', length(lon_vals))
  dim.def.nc(nc, 'latitude', length(lat_vals))
  dim.def.nc(nc, 'time', raster::nlayers(ssta_stack), unlim=TRUE)
  dim.def.nc(nc, 'mgmt_zone', length(mgmt_vec))
  
  var.def.nc(nc, 'longitude', 'NC_DOUBLE', 'longitude')
  var.def.nc(nc, 'latitude', 'NC_DOUBLE', 'latitude')
  var.def.nc(nc, varname='time', vartype='NC_INT', dimensions='time')
  var.def.nc(nc, varname='mgmt_zone', vartype='NC_STRING', dimensions='mgmt_zone')
  var.def.nc(nc, 'ssta', 'NC_DOUBLE', c('longitude','latitude', 'mgmt_zone', 'time'))
  var.def.nc(nc, 'habitat_suitability', 'NC_DOUBLE', c('longitude', 'latitude', 'mgmt_zone', 'time'))
  var.def.nc(nc, 'pt_longitude', 'NC_DOUBLE', c('mgmt_zone', 'time'))
  var.def.nc(nc, 'pt_latitude', 'NC_DOUBLE', c('mgmt_zone', 'time'))
  var.def.nc(nc, 'habitat_change', 'NC_DOUBLE', c('mgmt_zone', 'time'))
  var.def.nc(nc, 'MHW', 'NC_DOUBLE',c('mgmt_zone','time'))
  var.def.nc(nc, 'ssta_ts', 'NC_DOUBLE',c('mgmt_zone','time'))
  var.def.nc(nc, 'seasonal_thresh', 'NC_DOUBLE',c('mgmt_zone','time'))
  
  att.put.nc(nc, 'ssta', '_FillValue', 'NC_DOUBLE', -9999)
  att.put.nc(nc, 'ssta', 'long_name', 'NC_CHAR', 'Sea surface temperature anomaly')
  
  att.put.nc(nc, 'habitat_suitability', '_FillValue', 'NC_DOUBLE', -9999)
  att.put.nc(nc, 'habitat_suitability', 'long_name', 'NC_CHAR', 'Habitat suitability')
  att.put.nc(nc, 'mgmt_zone', 'long_name', 'NC_CHAR', 'Management zone name')
  
  att.put.nc(nc, 'time', 'units','NC_CHAR', 'days since 1970-01-01 00:00:00')
  att.put.nc(nc, 'time', 'long_name', 'NC_CHAR', 'time')
  att.put.nc(nc, 'time', 'standard_name', 'NC_CHAR', 'time')
  att.put.nc(nc, 'time', 'axis', 'NC_CHAR', 'T')
  
  att.put.nc(nc, 'longitude', 'long_name', 'NC_CHAR', 'Longitude')
  att.put.nc(nc, 'longitude', 'units', 'NC_CHAR', 'degrees_east')
  att.put.nc(nc, 'longitude', 'standard_name', 'NC_CHAR', 'longitude')
  att.put.nc(nc, 'longitude', 'axis', 'NC_CHAR', 'X')
  att.put.nc(nc, 'longitude', 'valid_min', 'NC_DOUBLE', -180)
  att.put.nc(nc, 'longitude', 'valid_max', 'NC_DOUBLE', 180)
  
  att.put.nc(nc, 'latitude', 'long_name', 'NC_CHAR', 'Latitude')
  att.put.nc(nc, 'latitude', 'units', 'NC_CHAR', 'degrees_north')
  att.put.nc(nc, 'latitude', 'standard_name', 'NC_CHAR', 'latitude')
  att.put.nc(nc, 'latitude', 'axis', 'NC_CHAR', 'Y')
  att.put.nc(nc, 'latitude', 'valid_min', 'NC_DOUBLE', -89.875)
  att.put.nc(nc, 'latitude', 'valid_max', 'NC_DOUBLE', 89.875)
  
  att.put.nc(nc, 'NC_GLOBAL', 'title', 'NC_CHAR', 'Distribution model results and associated environmental data')
  att.put.nc(nc, 'NC_GLOBAL', 'institution', 'NC_CHAR', 'Woods Hole Oceanographic Institution')
  att.put.nc(nc, 'NC_GLOBAL', 'url', 'NC_CHAR', 'https://fisheriesclimatetoolkit.sdsu.edu/')
  att.put.nc(nc, 'NC_GLOBAL', 'comment', 'NC_CHAR', 'This file is generated as part of the NASA-funded Fisheries and Climate Toolkit')
  att.put.nc(nc, 'NC_GLOBAL', 'creator_email', 'NC_CHAR', 'cbraun@whoi.edu')
  att.put.nc(nc, 'NC_GLOBAL', 'creator_name', 'NC_CHAR', 'Camrin Braun')
  att.put.nc(nc, 'NC_GLOBAL', 'date_created', 'NC_CHAR', as.character(Sys.Date()))
  
  var.put.nc(nc, 'latitude', lat_vals)
  var.put.nc(nc, 'longitude', lon_vals)
  
  orig_date <- as.POSIXct('1970-01-01 00:00:00', tz='UTC')
  var.put.nc(nc, 'time', as.numeric(difftime(time_vec, orig_date, 'days')))#, start=c(ii), count=c(1))
  var.put.nc(nc, 'mgmt_zone', mgmt_vec)#, start=c(1), count=c(NA))
  
  for (bb in 1:length(ssta_list)){
    for (ii in 1:nlayers(ssta_list[[bb]])){
      var.put.nc(nc, 'ssta', t(as.matrix(ssta_list[[bb]][[ii]])),start=c(1,1,bb,ii), count=c(NA, NA, 1, 1))
    }
  }

  
  for (bb in 1:length(hab_list)){
    for (ii in 1:nlayers(hab_list[[bb]])){
      var.put.nc(nc, 'habitat_suitability', t(as.matrix(hab_list[[bb]][[ii]])), start=c(1,1,bb,ii), count=c(NA, NA, 1, 1))
    }
  }
  
  for (ii in 1:length(mgmt_vec)){
    var.put.nc(nc, 'pt_longitude', cog_pts$COGx[which(cog_pts$mgmt_zone == mgmt_vec[ii])], start=c(ii, 1), count=c(1, NA))
    var.put.nc(nc, 'pt_latitude', cog_pts$COGy[which(cog_pts$mgmt_zone == mgmt_vec[ii])], start=c(ii, 1), count=c(1, NA))
  }
  
  ## put the habitat change values over time (vector), per management zone
  for (ii in 1:length(mgmt_vec)){
    var.put.nc(nc, 'habitat_change', change_df$habitat_change[which(change_df$mgmt_zone == mgmt_vec[ii])], start=c(ii, 1), count=c(1, NA))
  }
  
  ## put the MHW values over time (vector), per management zone
  for (ii in 1:length(mgmt_vec)){
    var.put.nc(nc, 'MHW', MHW_df$MHW[which(MHW_df$mgmt_zone == mgmt_vec[ii])], start=c(ii, 1), count=c(1, NA))
  }
  
  ## put the ssta_ts values over time (vector), per management zone
  for (ii in 1:length(mgmt_vec)){
    var.put.nc(nc, 'ssta_ts', ssta_ts_df$ssta[which(ssta_ts_df$mgmt_zone == mgmt_vec[ii])], start=c(ii, 1), count=c(1, NA))
  }
  
  ## put the seasonal_thresh values over time (vector), per management zone
  for (ii in 1:length(mgmt_vec)){
    var.put.nc(nc, 'seasonal_thresh', seasonal_thresh_df$seasonal_thresh[which(seasonal_thresh_df$mgmt_zone == mgmt_vec[ii])], start=c(ii, 1), count=c(1, NA))
  }
  
  sync.nc(nc)
  close.nc(nc)
  
}

