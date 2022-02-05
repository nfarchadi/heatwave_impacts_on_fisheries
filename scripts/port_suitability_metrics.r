#port suitability
library(dismo)
library(tidyverse)
library(SDMTools)
library(glue)
library(gbm)
library(rasterVis)
library(RStoolbox)
library(sf) #for mapping
library(zoo)
library(scales)
library(sp)
library(lubridate)
library(viridis)
library(gridExtra)
library(patchwork)
library(ggExtra)

###load in vdm predictions
NWA_PLL_anom<-stack("E:/VDM_results/NWA_gbm_convexhull/Monthly_Spatial_Predictions_v2/VDM_month_anom_stack_2012to2020.nc")


VDMharddrivepath<-"E:/VDM_results/NWA_gbm_convexhull"
predir<-glue("{VDMharddrivepath}/Spatial_Predictions_v2/")

VDM_predictions<-list.files(predir) %>%
  grep(pattern = ".nc", value = TRUE) %>% 
  grep(pattern = "", value = TRUE)

tt<-as.yearmon(unique(format(as.Date(substr(VDM_predictions, 1,10)), format = "%Y-%m")))
NWA_PLL_anom<-setZ(NWA_PLL_anom,tt)
idx<-unique(format(as.Date(substr(VDM_predictions, 1,10)),format = "%Y-%m"))
names(NWA_PLL_anom)<-idx


###load in US longline ports
ports<- readRDS("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/distance-from-port-v1/US_longline_ports_geocoded.rds")

#getting rid of duplicates locations
#dont need this coloumn
ports <- ports %>% dplyr::select(-historical.US.longline.fishery.segments)
ports <- unique(ports)
ports <- ports[order(-ports$lat),]



port_suit_fun<-function(stack,ports, km, thresh){
  port_suit_metrics<-as.data.frame(matrix(data = 0, nrow = 5, ncol =  6))
  colnames(port_suit_metrics)<-c("port.name","mean_suit", "total_area", "habitat_area", "radius_km", "date")
  counter = 1
  #now run it by port
  for (i in 1:nrow(ports)) {
    #just one port for now
    by_port<-ports[i,]
    
    port.name<-by_port$port.names #1 getting port name
    
    coordinates(by_port)<- ~lon+lat
    projection(by_port) <- crs(stack)
    
    #set radius for ports in km
    m_dist <- km * 1000 #need to convert to meters due to crs
    port_radius <- circles(by_port, d=m_dist, lonlat = TRUE)
    port_radius_pol <- polygons(port_radius)
    
    #only want cells inside radius
    port_radius_masked<-raster::mask(stack, port_radius_pol)
    
    #calculating the mean suitability and total area inside radius
    port_radius_masked_df<-as.data.frame(raster::rasterToPoints(port_radius_masked))
    
    colnames(port_radius_masked_df)<-(c('x','y','suitability'))
    
    #2 mean suit
    mean_suit<-mean(port_radius_masked_df$suitability, na.rm=TRUE)
    
    #calculating radius area
    port_area<-raster::area(port_radius_masked, na.rm=TRUE)#this computes the approximate surface areas of cells in km2 in an unprojected (lat/lon) raster object. This creates a raster where each cell has its area as its value. We also want to ignore cells that are 'NA'
    
    ##NAs lie outside of the rastered region, can thus be omitted
    port_area<-port_area[!is.na(port_area)]
    
    #3 compute area [km2] of all cells in geo_raster
    total_area<-sum(port_area)
    
    
    #now calculating habitat area
    
    # reclassify into 1s and 0s based on the threshold
    port_radius_masked[values(port_radius_masked)>thresh]=1 
    port_radius_masked[values(port_radius_masked)<thresh]=0
    port_radius_masked[values(port_radius_masked)==0]<-NA
    
    #again calculating radius area but now just for core habitat
    port_area<-raster::area(port_radius_masked, na.rm=TRUE)#this computes the approximate surface areas of cells in km2 in an unprojected (lat/lon) raster object. This creates a raster where each cell has its area as its value. We also want to ignore cells that are 'NA'
    
    ##NAs lie outside of the rastered region, can thus be omitted
    port_area<-port_area[!is.na(port_area)]
    
    #4 compute area [km2] of all cells in geo_raster
    habitat_area<-sum(port_area)
    
    port_suit_metrics[counter,1]<-port.name
    port_suit_metrics[counter,2]<-mean_suit
    port_suit_metrics[counter,3]<-total_area
    port_suit_metrics[counter,4]<-habitat_area
    port_suit_metrics[counter,5]<-km
    port_suit_metrics[counter,6]<- names(stack)
    counter=counter+1 
  }
 return(port_suit_metrics) 
}

#50
port_suitability50<-lapply(as.list(NWA_PLL_anom), FUN = function(x,...)port_suit_fun(x, ports, km = 50, thresh = 0)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

#100
port_suitability100<-lapply(as.list(NWA_PLL_anom), FUN = function(x,...)port_suit_fun(x, ports, km = 100, thresh = 0)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

#150
port_suitability150<-lapply(as.list(NWA_PLL_anom), FUN = function(x,...)port_suit_fun(x, ports, km = 150, thresh = 0)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

#200
port_suitability200<-lapply(as.list(NWA_PLL_anom), FUN = function(x,...)port_suit_fun(x, ports, km = 200, thresh = 0)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

#250
port_suitability250<-lapply(as.list(NWA_PLL_anom), FUN = function(x,...)port_suit_fun(x, ports, km = 250, thresh = 0)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

#300
port_suitability300<-lapply(as.list(NWA_PLL_anom), FUN = function(x,...)port_suit_fun(x, ports, km = 300, thresh = 0)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

port_suitability_thres<-rbind(port_suitability50,port_suitability100,
                        port_suitability150,port_suitability200,
                        port_suitability250,port_suitability300)


#plotting the data
##heat map time-series


port_suitability_thres <- port_suitability_thres %>% 
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  filter(month %in% c(7,8,9,10)) %>% 
  group_by(port.name,radius_km,year) %>% 
  summarise(yearly_mean_suit = mean(mean_suit, na.rm = TRUE),
            yearly_total_area = mean(total_area, na.rm = TRUE),
            yearly_habitat_area = mean(habitat_area, na.rm = TRUE)) %>% mutate(prop_habitat_area = yearly_habitat_area/yearly_total_area)

#df_split<-split(port_suitability_thres, f = port_suitability_thres$radius_km)

p1<-ggplot(port_suitability_thres #df_split$`50`
           ,aes(x=year,y=port.name,fill=prop_habitat_area))+
  geom_tile(size=.5) + 
  scale_fill_viridis(name = expression("Proportion of\navailable fishing area"~(km^2)),
                     option ="D", 
                     guide = guide_colorbar(barwidth = 1.5, 
                                            barheight = 20)) + 
  #scale_y_discrete(limits = rev(levels(as.factor(port_suitability$port.name)))) +
  scale_y_discrete(limits = rev(c("Portland ","New Bedford","Fairhaven",
                              "Montauk","Barnegat Light","Ocean City",
                              "Wanchese","Georgetown","Charleston",
                              "Rockville","Destin","Panama City",
                              "Dulca","Venice","Madiera Beach",
                              "Fort Pierce","Pompano Beach","Dania",
                              "Key West","San Juan","St. Croix")))+
  facet_wrap(~radius_km)+
  #scale_y_continuous(trans = "reverse", breaks = unique(df$hour))+
  #scale_x_continuous(seq(range(port_suitability$mean_suitperarea)[1],range(port_suitability$mean_suitperarea)[2], length.out = 5 ))+ 
  theme_minimal()+
  labs(x="Date", y="Port (ascending by latitude)")+
  theme(legend.position = "right")+
  theme(axis.text.y=element_text(size=17, margin = margin(r = -15))) +
  #theme(strip.background = element_rect(colour="white"))+
  theme(axis.text=element_text(size=20))+
  theme(legend.title=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"))+
  theme(strip.text = element_text(size = 20))+
  theme(axis.title = element_text(size = 20)) +
  removeGrid()#ggExtra
p1
p2<- p1 %+% df_split$`100`
p3<- p1 %+% df_split$`150`
p4<- p1 %+% df_split$`200`
p5<- p1 %+% df_split$`250`
p6<- p1 %+% df_split$`300`

p_all<-grid.arrange(p1,p2,p3,p4,p5,p6)

ggsave(plot=p1,"E:/VDM_results/NWA_gbm_convexhull/Plots_v2/port_vulnerability_proportionhabitatarea_anom_yearly_peakfishingseason.png", width = 20,height = 12, units = c("in"))



####lets do something similar as above but now for SST and SST anomalies. Would like to see mean and max SST, area of anomalous warmer SST, and area of preferred SST 

# ##load in SST 
# NWA_SST<-stack("E:/HYCOM/water_temp_month/SST_month_stack_2012to2020.nc")
# 
# 
# SSTharddrivepath<-"E:/HYCOM/sst_updated/"
# #predir<-glue("{VDMharddrivepath}/Spatial_Predictions_v2/")
# 
# SST_rasters<-list.files(SSTharddrivepath) %>%
#   grep(pattern = ".nc", value = TRUE) %>% 
#   grep(pattern = "", value = TRUE)
# 
# tt<-as.yearmon(unique(format(as.Date(substr(SST_rasters, 12,21)), format = "%Y-%m")))
# NWA_SST<-setZ(NWA_SST,tt)
# idx<-unique(format(as.Date(substr(SST_rasters, 12,21)),format = "%Y-%m"))
# names(NWA_SST)<-idx
# 
# #finding preferred SST range
# brt<-readRDS("E:/VDM_results/NWA_gbm_convexhull/brt_v2.rds")
# brt_SST<-plot(brt, 'SST', return.grid=TRUE)
# brt_SST_preferred<-brt_SST[brt_SST$y>=0,]
# brt_SST_preferred[order(-brt_SST_preferred$SST),] #looks like 15 to 17 C
# #15 to 24
# 
# 
# 
# port_SST_fun<-function(stack, ports, km, lower_range, upper_range){
#   port_SST_metrics<-as.data.frame(matrix(data = 0, nrow = 5, ncol =  7))
#   colnames(port_SST_metrics)<-c("port.name","mean_SST", "max_SST",
#                                  "total_area","prefer_SST_area",
#                                  "radius_km", "date")
#   counter = 1
#   #now run it by port
#   for (i in 1:nrow(ports)) {
#     #just one port for now
#     by_port<-ports[i,]
#     
#     port.name<-by_port$port.names #1 getting port name
#     
#     coordinates(by_port)<- ~lon+lat
#     projection(by_port) <- crs(stack)
#     
#     #set radius for ports in km
#     m_dist <- km * 1000 #need to convert to meters due to crs
#     port_radius <- circles(by_port, d=m_dist, lonlat = TRUE)
#     port_radius_pol <- polygons(port_radius)
#     
#     #only want cells inside radius
#     port_radius_masked<-raster::mask(stack, port_radius_pol)
#     
#     #calculating the mean SST, max SST, preferred SST area and total area inside radius
#     port_radius_masked_df<-as.data.frame(raster::rasterToPoints(port_radius_masked))
#     
#     colnames(port_radius_masked_df)<-(c('x','y','SST'))
#     
#     #2 mean SST
#     mean_SST<-mean(port_radius_masked_df$SST, na.rm=TRUE)
#     #3 max SST
#     max_SST<-max(port_radius_masked_df$SST, na.rm=TRUE)
#     
#     #calculating radius area
#     port_area<-raster::area(port_radius_masked, na.rm=TRUE)#this computes the approximate surface areas of cells in km2 in an unprojected (lat/lon) raster object. This creates a raster where each cell has its area as its value. We also want to ignore cells that are 'NA'
#     
#     ##NAs lie outside of the rastered region, can thus be omitted
#     port_area<-port_area[!is.na(port_area)]
#     
#     #4 compute total area [km2] of all cells in geo_raster
#     total_area<-sum(port_area)
#     
#     
#     #now calculating preferred SST area
#     port_radius_masked[values(port_radius_masked) < lower_range]=0
#     port_radius_masked[values(port_radius_masked) > upper_range]=0
#     port_radius_masked[values(port_radius_masked)==0]=NA
# 
#     
#     #again calculating radius area but now just for preferred SST area
#     port_area<-raster::area(port_radius_masked, na.rm=TRUE)#this computes the approximate surface areas of cells in km2 in an unprojected (lat/lon) raster object. This creates a raster where each cell has its area as its value. We also want to ignore cells that are 'NA'
#     
#     ##NAs lie outside of the rastered region, can thus be omitted
#     port_area<-port_area[!is.na(port_area)]
#     
#     #5 compute preferred SST area [km2]
#     preferred_SST_area<-sum(port_area)
#     
#     
#     port_SST_metrics[counter,1]<-port.name
#     port_SST_metrics[counter,2]<-mean_SST
#     port_SST_metrics[counter,3]<-max_SST
#     port_SST_metrics[counter,4]<-total_area
#     port_SST_metrics[counter,5]<-preferred_SST_area
#     port_SST_metrics[counter,6]<-km
#     port_SST_metrics[counter,7]<-names(stack)
#     counter=counter+1 
#   }
#   return(port_SST_metrics) 
# }
# 
# #50
# port_SST50<-lapply(as.list(NWA_SST), FUN = function(x,...)port_SST_fun(x, ports, km = 50, lower_range = 15, upper_range = 24)) %>% 
#   do.call("rbind", .) %>% as.data.frame() %>% 
#   #fixing the date column
#   mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())
# 
# #100
# port_SST100<-lapply(as.list(NWA_SST), FUN = function(x,...)port_SST_fun(x, ports, km = 100, lower_range = 15, upper_range = 24)) %>% 
#   do.call("rbind", .) %>% as.data.frame() %>% 
#   #fixing the date column
#   mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())
# 
# #150
# port_SST150<-lapply(as.list(NWA_SST), FUN = function(x,...)port_SST_fun(x, ports, km = 150, lower_range = 15, upper_range = 24)) %>% 
#   do.call("rbind", .) %>% as.data.frame() %>% 
#   #fixing the date column
#   mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())
# 
# #200
# port_SST200<-lapply(as.list(NWA_SST), FUN = function(x,...)port_SST_fun(x, ports, km = 200, lower_range = 15, upper_range = 24)) %>% 
#   do.call("rbind", .) %>% as.data.frame() %>% 
#   #fixing the date column
#   mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())
# 
# #250
# port_SST250<-lapply(as.list(NWA_SST), FUN = function(x,...)port_SST_fun(x, ports, km = 250, lower_range = 15, upper_range = 24)) %>% 
#   do.call("rbind", .) %>% as.data.frame() %>% 
#   #fixing the date column
#   mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())
# 
# #300
# port_SST300<-lapply(as.list(NWA_SST), FUN = function(x,...)port_SST_fun(x, ports, km = 300, lower_range = 15, upper_range = 24)) %>% 
#   do.call("rbind", .) %>% as.data.frame() %>% 
#   #fixing the date column
#   mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())
# 
# 
# port_SST<-rbind(port_SST50,port_SST100,
#                         port_SST150,port_SST200,
#                         port_SST250,port_SST300)
# 
# 
# #plotting the data
# ##heat map time-series
# # library(gridExtra)
# # library(patchwork)
# # library(ggExtra)
# 
# port_SST <- port_SST %>% 
#   mutate(month = lubridate::month(date),
#          year = lubridate::year(date)) %>% 
#   filter(month %in% c(7,8,9,10)) %>% 
#   group_by(port.name,radius_km,year) %>% 
#   summarise(mean_SST_fishingseason = mean(mean_SST, na.rm = TRUE),
#             max_SST_fishingseason = mean(max_SST, na.rm = TRUE),
#             total_area_fishingseason = mean(total_area, na.rm = TRUE),
#             prefer_SST_area_fishingseason = mean(prefer_SST_area, na.rm = TRUE)) %>% mutate(prop_prefer_SST_area = prefer_SST_area_fishingseason/total_area_fishingseason)
# 
# 
# df_split<-split(port_SST, f = port_SST$radius_km)
# 
# p1<-ggplot(df_split$`50` 
#            ,aes(x=year,y=port.name,fill=prop_prefer_SST_area))+
#   geom_tile(size=.5) + 
#   scale_fill_viridis(name=expression(Proportion~Preferred~SST~"(°C)"),option ="turbo") + 
#   #scale_y_discrete(limits = rev(levels(as.factor(port_SST$port.name)))) +
#   scale_y_discrete(limits = rev(c("Portland ","New Bedford","Fairhaven",
#                                   "Montauk","Barnegat Light","Ocean City",
#                                   "Wanchese","Georgetown","Charleston",
#                                   "Rockville","Destin","Panama City",
#                                   "Dulca","Venice","Madiera Beach",
#                                   "Fort Pierce","Pompano Beach","Dania",
#                                   "Key West","San Juan","St. Croix")))+
#   facet_wrap(~radius_km)+
#   #scale_y_continuous(trans = "reverse", breaks = unique(df$hour))+
#   #scale_x_continuous(seq(range(port_SST$mean_suitperarea)[1],range(port_SST$mean_suitperarea)[2], length.out = 5 ))+ 
#   theme_minimal()+
#   labs(x="Date", y="Port")+
#   theme(legend.position = "right")+
#   theme(axis.text.y=element_text(size=10, margin = margin(r = -15))) +
#   #theme(strip.background = element_rect(colour="white"))+
#   theme(axis.text=element_text(size=10))+
#   theme(legend.title=element_text(size=8))+
#   theme(legend.text=element_text(size=6))+
#   theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"))+
#   removeGrid()#ggExtra
# 
# p2<- p1 %+% df_split$`100`
# p3<- p1 %+% df_split$`150`
# p4<- p1 %+% df_split$`200`
# p5<- p1 %+% df_split$`250`
# p6<- p1 %+% df_split$`300`
# 
# p_all<-grid.arrange(p1,p2,p3,p4,p5,p6)




####one more time but for SST anom




##load in SST anomalies 
NWA_SST_anom<-stack("E:/HYCOM/water_temp_month/SST_month_anom_stack_2012to2020.nc")


SSTharddrivepath<-"E:/HYCOM/sst_updated/"


SST_rasters<-list.files(SSTharddrivepath) %>%
  grep(pattern = ".nc", value = TRUE) %>% 
  grep(pattern = "", value = TRUE)


tt<-as.yearmon(unique(format(as.Date(substr(SST_rasters, 12,21)), format = "%Y-%m")))
NWA_SST_anom<-setZ(NWA_SST_anom,tt)
idx<-unique(format(as.Date(substr(SST_rasters, 12,21)),format = "%Y-%m"))
names(NWA_SST_anom)<-idx


thresh <- 1.2 #found that regions of MHW SST anomalies are on 1.2 degrees higher than climatological monthly mean



port_SST_anom_fun<-function(stack, ports, km, thresh){
  port_SST_metrics<-as.data.frame(matrix(data = 0, nrow = 5, ncol =  7))
  colnames(port_SST_metrics)<-c("port.name","mean_SST_anom", "max_SST_anom",
                                "total_area","warm_SST_area",
                                "radius_km", "date")
  counter = 1
  #now run it by port
  for (i in 1:nrow(ports)) {
    #just one port for now
    by_port<-ports[i,]
    
    port.name<-by_port$port.names #1 getting port name
    
    coordinates(by_port)<- ~lon+lat
    projection(by_port) <- crs(stack)
    
    #set radius for ports in km
    m_dist <- km * 1000 #need to convert to meters due to crs
    port_radius <- circles(by_port, d=m_dist, lonlat = TRUE)
    port_radius_pol <- polygons(port_radius)
    
    #only want cells inside radius
    port_radius_masked<-raster::mask(stack, port_radius_pol)
    
    #calculating the mean SST, max SST, preferred SST area and total area inside radius
    port_radius_masked_df<-as.data.frame(raster::rasterToPoints(port_radius_masked))
    
    colnames(port_radius_masked_df)<-(c('x','y','SST'))
    
    #2 mean SST anom
    mean_SST<-mean(port_radius_masked_df$SST, na.rm=TRUE)
    #3 max SST anom
    max_SST<-max(port_radius_masked_df$SST, na.rm=TRUE)
    
    #calculating radius area
    port_area<-raster::area(port_radius_masked, na.rm=TRUE)#this computes the approximate surface areas of cells in km2 in an unprojected (lat/lon) raster object. This creates a raster where each cell has its area as its value. We also want to ignore cells that are 'NA'
    
    ##NAs lie outside of the rastered region, can thus be omitted
    port_area<-port_area[!is.na(port_area)]
    
    #4 compute total area [km2] of all cells in geo_raster
    total_area<-sum(port_area)
    
    
    #now calculating preferred SST area
    port_radius_masked[values(port_radius_masked)>=thresh]=1 
    port_radius_masked[values(port_radius_masked)<thresh]=0
    port_radius_masked[values(port_radius_masked)==0]<-NA

    
    #again calculating radius area but now just for preferred SST area
    port_area<-raster::area(port_radius_masked, na.rm=TRUE)#this computes the approximate surface areas of cells in km2 in an unprojected (lat/lon) raster object. This creates a raster where each cell has its area as its value. We also want to ignore cells that are 'NA'
    
    ##NAs lie outside of the rastered region, can thus be omitted
    port_area<-port_area[!is.na(port_area)]
    
    #5 compute warm SST area [km2]
    warm_SST_area<-sum(port_area)
    
    
    port_SST_metrics[counter,1]<-port.name
    port_SST_metrics[counter,2]<-mean_SST
    port_SST_metrics[counter,3]<-max_SST
    port_SST_metrics[counter,4]<-total_area
    port_SST_metrics[counter,5]<-warm_SST_area
    port_SST_metrics[counter,6]<-km
    port_SST_metrics[counter,7]<-names(stack)
    counter=counter+1 
  }
  return(port_SST_metrics) 
}

#50
port_SST_anom50<-lapply(as.list(NWA_SST_anom), FUN = function(x,...)port_SST_anom_fun(x, ports, km = 50, thresh = 1.2)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

#100
port_SST_anom100<-lapply(as.list(NWA_SST_anom), FUN = function(x,...)port_SST_anom_fun(x, ports, km = 100, thresh = 1.2)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

#150
port_SST_anom150<-lapply(as.list(NWA_SST_anom), FUN = function(x,...)port_SST_anom_fun(x, ports, km = 150, thresh = 1.2)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

#200
port_SST_anom200<-lapply(as.list(NWA_SST_anom), FUN = function(x,...)port_SST_anom_fun(x, ports, km = 200, thresh = 1.2)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

#250
port_SST_anom250<-lapply(as.list(NWA_SST_anom), FUN = function(x,...)port_SST_anom_fun(x, ports, km = 250, thresh = 1.2)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())

#300
port_SST_anom300<-lapply(as.list(NWA_SST_anom), FUN = function(x,...)port_SST_anom_fun(x, ports, km = 300, thresh = 1)) %>% 
  do.call("rbind", .) %>% as.data.frame() %>% 
  #fixing the date column
  mutate(date = date %>% str_remove("X") %>% str_replace("\\.","-") %>% as.yearmon())


port_SST_anom<-rbind(port_SST_anom50,port_SST_anom100,
                port_SST_anom150,port_SST_anom200,
                port_SST_anom250,port_SST_anom300)


#plotting the data
##heat map time-series


port_SST_anom <- port_SST_anom %>% 
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  group_by(port.name,radius_km,year) %>% 
  summarise(mean_SST_anom = mean(mean_SST_anom, na.rm = TRUE),
            max_SST_anom = mean(max_SST_anom, na.rm = TRUE),
            total_area = mean(total_area, na.rm = TRUE),
            warm_SST_area = mean(warm_SST_area, na.rm = TRUE)) %>% mutate(prop_warm_SST_area = warm_SST_area/total_area)


df_split<-split(port_SST_anom, f = port_SST_anom$radius_km)

p1<-ggplot(port_SST_anom 
           ,aes(x=year,y=port.name,fill=prop_warm_SST_area))+
  geom_tile(size=.5) + 
  scale_fill_viridis(name=expression("Proportion of\nMHW Area (km^2)"),option ="inferno") + 
  #scale_y_discrete(limits = rev(levels(as.factor(port_SST$port.name)))) +
  scale_y_discrete(limits = rev(c("Portland ","New Bedford","Fairhaven",
                                  "Montauk","Barnegat Light","Ocean City",
                                  "Wanchese","Georgetown","Charleston",
                                  "Rockville","Destin","Panama City",
                                  "Dulca","Venice","Madiera Beach",
                                  "Fort Pierce","Pompano Beach","Dania",
                                  "Key West","San Juan","St. Croix")))+
  facet_wrap(~radius_km)+
  #scale_y_continuous(trans = "reverse", breaks = unique(df$hour))+
  #scale_x_continuous(seq(range(port_SST$mean_suitperarea)[1],range(port_SST$mean_suitperarea)[2], length.out = 5 ))+ 
  theme_minimal()+
  labs(x="Date", y="Port")+
  theme(legend.position = "right")+
  theme(axis.text.y=element_text(size=10, margin = margin(r = -15))) +
  #theme(strip.background = element_rect(colour="white"))+
  theme(axis.text=element_text(size=10))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"))+
  removeGrid()#ggExtra

p2<- p1 %+% df_split$`100`
p3<- p1 %+% df_split$`150`
p4<- p1 %+% df_split$`200`
p5<- p1 %+% df_split$`250`
p6<- p1 %+% df_split$`300`

p_all<-grid.arrange(p1,p2,p3,p4,p5,p6)






############Combine all together#########
port_suitability<-rbind(port_suitability50,port_suitability100,
                        port_suitability150,port_suitability200,
                        port_suitability250,port_suitability300)

port_SST_anom<-rbind(port_SST_anom50,port_SST_anom100,
                     port_SST_anom150,port_SST_anom200,
                     port_SST_anom250,port_SST_anom300)

port_SST<-rbind(port_SST50,port_SST100,
                port_SST150,port_SST200,
                port_SST250,port_SST300)

port_suit_SST_df<-cbind(port_suitability,
                        port_SST_anom,
                        port_SST)

port_suit_SST_df<-port_suit_SST_df[,-c(7,10,12,13,14,19,20)] #getting rid duplicates of port.name and total_area 

port_suit_SST_df<-port_suit_SST_df %>% rename(total_area_anom = total_area,
                                              total_area_SST = total_area.1)

#reordering columns
port_suit_SST_df<-port_suit_SST_df[,c(1,2,3,4,7,8,9,10,11,12,13,5,6)] 

#make these factors
port_suit_SST_df$port.name<-as.factor(port_suit_SST_df$port.name)
port_suit_SST_df$radius_km<-as.factor(port_suit_SST_df$radius_km)

#saveRDS(port_suit_SST_df, "E:/VDM_results/NWA_gbm_convexhull/port_suit_SST_metrics.rds")

port_suit_SST_df<-readRDS("E:/VDM_results/NWA_gbm_convexhull/port_suit_SST_metrics.rds")

#getting yearly means and filter to just the fishing season
port_suit_SST_peakfishseason <- port_suit_SST_df %>% 
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>%
  filter(month %in% c(7,8,9,10)) %>% #just want peak fishing season
  group_by(port.name,radius_km,year) %>% 
  summarise(yearly_mean_suit = mean(mean_suit, na.rm = TRUE),
            yearly_total_area_anom = mean(total_area_anom, na.rm = TRUE),
            yearly_habitat_area = mean(habitat_area, na.rm = TRUE),
            yearly_warm_SST_area = mean(warm_SST_area, na.rm = TRUE),
            yearly_total_area_SST = mean(total_area_SST, na.rm = TRUE)) %>% mutate(prop_habitat_area = yearly_habitat_area/yearly_total_area_anom,
       prop_SST_area = yearly_warm_SST_area / yearly_total_area_SST)
  # mutate(prop_habitat_area = habitat_area/total_area_anom,
  #         prop_SST_area = warm_SST_area / total_area_SST)


ggplot(port_suit_SST_peakfishseason %>% filter(radius_km == 50)
       ,aes(x=year,y=port.name,fill=prop_habitat_area))+
  geom_tile(size=.5) +
  scale_fill_viridis(name=expression("Proportion of\navailable fishing area"~km^2),option ="D",guide = guide_colorbar(barwidth = 1.5,barheight = 28)) +
  #scale_y_discrete(limits = rev(levels(as.factor(port_suitability$port.name)))) +
  scale_y_discrete(limits = rev(c("Portland ","New Bedford","Fairhaven",
                                  "Montauk","Barnegat Light","Ocean City",
                                  "Wanchese","Georgetown","Charleston",
                                  "Rockville","Destin","Panama City",
                                  "Dulca","Venice","Madiera Beach",
                                  "Fort Pierce","Pompano Beach","Dania",
                                  "Key West","San Juan","St. Croix")))+
  facet_wrap(~radius_km,labeller = labeller(radius_km = c("50" = "50 km")))+
  #scale_y_continuous(trans = "reverse", breaks = unique(df$hour))+
  #scale_x_continuous(seq(range(port_suitability$mean_suitperarea)[1],range(port_suitability$mean_suitperarea)[2], length.out = 5 ))+
  theme_minimal()+
  labs(x="Date", y="Port (ascending by latitude)")+
  theme(legend.position = "right")+
  theme(axis.text.y=element_text(size=15, margin = margin(r = -15)),
        axis.text.x = element_text(size = 15)) +
  #theme(strip.background = element_rect(colour="white"))+
  theme(axis.title =element_text(size=20,face = "bold"))+
  theme(legend.title=element_text(size=15))+
  theme(legend.text=element_text(size=10))+
  theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"),
        strip.text = element_text(size=20,face = "bold"))+
  removeGrid()#ggExtra



#looking at correlations

cor.test(port_suit_SST_peakfishseason$prop_habitat_area,
         port_suit_SST_peakfishseason$prop_SST_area) #negative correlation but not that strong 

#Lets see what happens if we break it down by port
port_suit_SST_peakfishseason$port.name = factor(port_suit_SST_peakfishseason$port.name, levels=c("Portland ","New Bedford","Fairhaven","Montauk","Barnegat Light","Ocean City", "Wanchese","Georgetown","Charleston","Rockville","Destin","Panama City","Dulca","Venice","Madiera Beach","Fort Pierce","Pompano Beach","Dania","Key West","San Juan","St. Croix"))

ggplot(port_suit_SST_peakfishseason
       ,aes(x=prop_SST_area,y=prop_habitat_area,color=radius_km))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_viridis(name=expression("Radius"~(km)),option ="viridis",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 20))+
  facet_wrap(~port.name)+
  theme_minimal()+
  labs(x=expression("Proportion of anomalous SST"~(km^2)), y=expression("Proportion of available fishing area"~(km^2))) +
  theme(axis.text=element_text(size=20))+
  theme(legend.title=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  #theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"))+
  theme(strip.text = element_text(size = 20))+
  theme(axis.title = element_text(size = 20))
  

ggsave("E:/VDM_results/NWA_gbm_convexhull/Plots_v2/proportionhabitatarea_vs _propostionofanomalousSST_peakfishingseason_yearlyaverage.png", width = 20,height = 12, units = c("in"))









########linear mixed models
library(lme4)

#investigating the data
hist(port_suit_SST_df$habitat_area) #right skewed - either transform or use glmer 

#how does it look facet wrapped
ggplot(port_suit_SST_df,aes(x=warm_SST_area,y=habitat_area,col=radius_km)) +
  geom_point() + facet_wrap(~port.name) + theme_bw()

#calculate proportion of higher habitat area and warmer SST area 
port_suit_SST_df <- port_suit_SST_df %>% 
  mutate(prop_habitat_area = habitat_area / total_area_anom,
         prop_warm_SST_area = warm_SST_area / total_area_SST)

boxplot(port_suit_SST_df$habitat_area ~ port_suit_SST_df$radius_km)
boxplot(port_suit_SST_df$warm_SST_area ~ port_suit_SST_df$radius_km)


#1 habitat area ~ warmer SST area

#need to scale the predictor variables
port_suit_SST_df$warm_SST_area2 <- scale(port_suit_SST_df$warm_SST_area, center = TRUE, scale = TRUE)


port_suit_SST_df$prop_warm_SST_area2 <- scale(port_suit_SST_df$prop_warm_SST_area, center = TRUE, scale = TRUE)


lm_anom_habitat_SST<- lmer(log(habitat_area+1) ~ warm_SST_area2 + radius_km + (1|port.name),data = port_suit_SST_df)

summary(lm_anom_habitat_SST)

plot(lm_anom_habitat_SST)


#lets only look at peak fishing season
port_suit_SST_fishingseason_df <- port_suit_SST_df %>%
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>%
  filter(month %in% c(7,8,9,10))


port_suit_SST_fishingseason_df$warm_SST_area2 <- scale(port_suit_SST_fishingseason_df$warm_SST_area, center = TRUE, scale = TRUE)


port_suit_SST_fishingseason_df$prop_warm_SST_area2 <- scale(port_suit_SST_fishingseason_df$prop_warm_SST_area, center = TRUE, scale = TRUE)



hist(port_suit_SST_fishingseason_df$habitat_area^(1/3)) #cube root transformation looks normal (log and square root didn't work as well)

#run the linear mixed model
lm_anom_habitat_fishingseason_SST<- lmer(habitat_area^(1/3) ~ warm_SST_area2 + (1|radius_km/port.name), data = port_suit_SST_fishingseason_df)

summary(lm_anom_habitat_fishingseason_SST)

plot(lm_anom_habitat_fishingseason_SST)

ggplot(port_suit_SST_fishingseason_df,aes(x=warm_SST_area,y=habitat_area,col=port.name)) +
  geom_point() + facet_wrap(~radius_km) + theme_bw()


#linear models to test Proportion of anomalous positive suitable fishing area vs anomalous positive SST area during peak fishing season
port_suitability_thres <- port_suit_SST_peakfishseason 
  group_by(port.name,radius_km,year) %>% 
  summarise(yearly_mean_suit = mean(mean_suit, na.rm = TRUE),
            yearly_total_area = mean(total_area, na.rm = TRUE),
            yearly_habitat_area = mean(habitat_area, na.rm = TRUE)) %>% mutate(prop_habitat_area = yearly_habitat_area/yearly_total_area)


ggplot(port_suit_SST_fishingseason_df, aes(x=))



# #donut-hole approach
# 
# port_suit_SST_df<-readRDS("E:/VDM_results/NWA_gbm_convexhull/port_suit_SST_metrics.rds") #this contains the metrics for all the months (does not only contain the fishing season months)
# 
# #calculating the yearly means for mean_suit, total_area, and habitat_area
# port_suit_habitat <- port_suit_SST_df %>% 
#   mutate(month = lubridate::month(date),
#          year = lubridate::year(date)) %>% 
#   group_by(port.name,radius_km,year) %>% 
#   summarise(yearly_mean_suit = mean(mean_suit, na.rm = TRUE),
#             yearly_total_area = mean(total_area_anom, na.rm = TRUE),
#             yearly_habitat_area = mean(habitat_area, na.rm = TRUE)) %>% 
#   ungroup()
# 
# 
# #so to do the donut hole I would need to subtract the larger radius by the smaller. so 100 - 50, 150 - 100, 200 - 150,......300 - 250
# #lets spilt them up first by radius
# df_split<-split(port_suit_habitat, f = port_suit_habitat$radius_km)
# 
# #need to calculated prop_habitat_area for just 50
# df_split$`50` <- df_split$`50` %>% 
#   mutate(prop_habitat_area = yearly_habitat_area / yearly_total_area)
# 
# #100 - 50
# df_split$`100` <- df_split$`100` %>% #100 radius km
#   mutate(donut_habitat_area = yearly_habitat_area -  df_split$`50`$yearly_habitat_area, #subtracting 100 - 50 km metrics
#          donut_total_area = yearly_total_area - df_split$`50`$yearly_total_area) %>% 
#   mutate(prop_habitat_area = donut_habitat_area / donut_total_area) %>%  #calculating proportion of habitat area 
#   select(-yearly_habitat_area,-yearly_total_area) %>% 
#   rename(yearly_habitat_area = donut_habitat_area, 
#          yearly_total_area = donut_total_area)
# 
# #150 - 100
# df_split$`150` <- df_split$`150` %>% 
#   mutate(donut_habitat_area = yearly_habitat_area -  df_split$`100`$yearly_habitat_area, 
#          donut_total_area = yearly_total_area - df_split$`100`$yearly_total_area) %>% 
#   mutate(prop_habitat_area = donut_habitat_area / donut_total_area) %>%  #calculating proportion of habitat area
#   select(-yearly_habitat_area,-yearly_total_area) %>% 
#   rename(yearly_habitat_area = donut_habitat_area, 
#          yearly_total_area = donut_total_area)
# 
# #200 - 150
# df_split$`200` <- df_split$`200` %>% 
#   mutate(donut_habitat_area = yearly_habitat_area -  df_split$`150`$yearly_habitat_area, 
#          donut_total_area = yearly_total_area - df_split$`150`$yearly_total_area) %>% 
#   mutate(prop_habitat_area = donut_habitat_area / donut_total_area) %>%  #calculating proportion of habitat area
#   select(-yearly_habitat_area,-yearly_total_area) %>% 
#   rename(yearly_habitat_area = donut_habitat_area, 
#          yearly_total_area = donut_total_area)
# 
# #250 - 200
# df_split$`250` <- df_split$`250` %>% 
#   mutate(donut_habitat_area = yearly_habitat_area -  df_split$`200`$yearly_habitat_area, 
#          donut_total_area = yearly_total_area - df_split$`200`$yearly_total_area) %>% 
#   mutate(prop_habitat_area = donut_habitat_area / donut_total_area) %>%  #calculating proportion of habitat area
#   select(-yearly_habitat_area,-yearly_total_area) %>% 
#   rename(yearly_habitat_area = donut_habitat_area, 
#          yearly_total_area = donut_total_area)
# 
# #300 - 250
# df_split$`300` <- df_split$`300` %>% 
#   mutate(donut_habitat_area = yearly_habitat_area -  df_split$`250`$yearly_habitat_area, 
#          donut_total_area = yearly_total_area - df_split$`250`$yearly_total_area) %>% 
#   mutate(prop_habitat_area = donut_habitat_area / donut_total_area) %>%  #calculating proportion of habitat area
#   select(-yearly_habitat_area,-yearly_total_area) %>% 
#   rename(yearly_habitat_area = donut_habitat_area, 
#          yearly_total_area = donut_total_area)
# 
# #now lets plot it
# donut_port_suit_df<-rbind(df_split$`50`, df_split$`100`,
#                           df_split$`150`, df_split$`200`,
#                           df_split$`250`,df_split$`300`)
# 
# ggplot(donut_port_suit_df
#            ,aes(x=year,y=port.name,fill=prop_habitat_area))+
#   geom_tile(size=.5) +
#   scale_fill_viridis(name=expression("Proportion of\nhabitat area"~km^2),option ="D") +
#   #scale_y_discrete(limits = rev(levels(as.factor(port_suitability$port.name)))) +
#   scale_y_discrete(limits = rev(c("Portland ","New Bedford","Fairhaven",
#                                   "Montauk","Barnegat Light","Ocean City",
#                                   "Wanchese","Georgetown","Charleston",
#                                   "Rockville","Destin","Panama City",
#                                   "Dulca","Venice","Madiera Beach",
#                                   "Fort Pierce","Pompano Beach","Dania",
#                                   "Key West","San Juan","St. Croix")))+
#   facet_wrap(~radius_km)+
#   #scale_y_continuous(trans = "reverse", breaks = unique(df$hour))+
#   #scale_x_continuous(seq(range(port_suitability$mean_suitperarea)[1],range(port_suitability$mean_suitperarea)[2], length.out = 5 ))+
#   theme_minimal()+
#   labs(x="Date", y="Port")+
#   theme(legend.position = "right")+
#   theme(axis.text.y=element_text(size=10, margin = margin(r = -15))) +
#   #theme(strip.background = element_rect(colour="white"))+
#   theme(axis.text=element_text(size=10))+
#   theme(legend.title=element_text(size=8))+
#   theme(legend.text=element_text(size=6))+
#   theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"))+
#   removeGrid()#ggExtra
