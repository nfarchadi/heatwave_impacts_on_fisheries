library(here)
library(sf)
library(tidyverse)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(heatwaveR)
library(ggnewscale)
library(raster)
library(zoo)
library(cmocean)
library(ggridges)
library(viridis)
world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

# ########################################
# #average SST throughout the study period
# ########################################
# 
# ## build a time vector to specify the time points of the grids
# time_vec <- seq.POSIXt(as.POSIXct('2012-01-01', tz='UTC'), as.POSIXct('2020-12-01', tz='UTC'), by='month')
# 
# ## we also need SSTa
# fList <- list.files("E:/OISST/monthly", full.names = TRUE, pattern = ".nc")
# 
# fList_years <- c()
# for (i in 1:length(fList)) fList_years[i] <- substr(fList[i], 24,27)
# 
# ssta_stack <- raster::stack(fList[which(fList_years %in% lubridate::year(time_vec))])
# 
# NWA_SST<-calc(ssta_stack, mean, na.rm=TRUE)
# 
# NWA_SST_df<-rasterToPoints(NWA_SST) %>% as.data.frame() %>% 
#   rename("SST"="layer")

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NWA_PLL_zones<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

#trying to clip the management zones to the coast
land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_zones<-st_difference(NWA_PLL_zones, st_union(st_combine(land)))

# ###########################################
# #load and combine PLL landscape metrics dfs
# ###########################################
# 
# NWAPLL_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_total.rds") %>% 
#   readRDS()
# 
# near_avg<-NWAPLL_MHW_total %>% filter(MHW == 0) %>% 
#   filter(mgmt_zone %in% c('NED','MAB','FEC','GOM')) %>% 
#   group_by(mgmt_zone) %>% 
#   summarise(COGx = mean(COGx, na.rm = TRUE),
#             COGy = mean(COGy, na.rm = TRUE))
# 
# may_2012_mhw<-NWAPLL_MHW_total %>% 
#   filter(date == as.yearmon("2012-05-01")) %>%
#   filter(mgmt_zone %in% c('NED','MAB','FEC','GOM')) %>% 
#   dplyr::select("mgmt_zone","COGx","COGy")
# 
# NED_MAB<-ggplot()+
#   geom_tile(NWA_SST_df, mapping = aes(x=x,y=y,fill=SST))+
#   scale_fill_cmocean(name="thermal",
#                      limits = c(0,30),
#                      guide = guide_colorbar(title.position = "right"))+
#   labs(x="",y="",fill="SST (°C)", title = "NED & MAB")+
#   ggnewscale::new_scale_fill()+
#   geom_sf(data = world, color= "black", fill = "grey")+
#   geom_sf(data = NWA_PLL_zones %>% filter(ET_ID %in% c('NED','MAB','FEC','GOM')), color = "cyan", fill=NA, size = 1)+ 
#   geom_point(NWAPLL_MHW_total %>% filter(MHW == 1) %>% filter(mgmt_zone %in% c('NED','MAB','FEC','GOM')), 
#              mapping = aes(x = COGx, y = COGy,color = mgmt_zone),
#              shape = 21, size = 2, show.legend = FALSE) +
#   geom_point(near_avg, mapping = aes(x=COGx,y=COGy, fill = mgmt_zone), 
#              color = "black",
#              shape = 23, size = 3,show.legend = FALSE)+
#   geom_point(may_2012_mhw, mapping = aes(x=COGx,y=COGy, fill = mgmt_zone), 
#              color = "black", 
#              shape = 21, size = 3, show.legend = FALSE)+
#   shadowtext::geom_shadowtext(may_2012_mhw, mapping = aes(x=COGx + 5.5,y=COGy, 
#                                                           color = mgmt_zone,
#                                                           label = "May 2012",
#                                                           fontface = "bold"),
#                               show.legend = FALSE)+
#   coord_sf(xlim =  c(-97.5, -40), ylim = c(35, 49.5), expand = TRUE)+
#   theme_bw()+
#   theme(legend.spacing.y = unit(0.2, 'cm'),
#         legend.spacing.x = unit(0.15, 'cm'),
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(5,5,-7,-7),
#         legend.title = element_text(angle = 90),
#         legend.title.align = 0.5) 
# 
# 
# FEC_GOM<-ggplot()+
#   geom_tile(NWA_SST_df, mapping = aes(x=x,y=y,fill=SST))+
#   scale_fill_cmocean(name="thermal",
#                      limits = c(0,30),
#                      guide = guide_colorbar(title.position = "right"))+
#   labs(x="",y="",fill="SST (°C)", title = "FEC & GOM")+
#   ggnewscale::new_scale_fill()+
#   geom_sf(data = world, color= "black", fill = "grey")+
#   geom_sf(data = NWA_PLL_zones %>% filter(ET_ID %in% c('NED','MAB','FEC','GOM')), color = "cyan", fill=NA, size = 1)+
#   geom_point(NWAPLL_MHW_total %>% filter(MHW == 1) %>% filter(mgmt_zone %in% c('NED','MAB','FEC','GOM')), 
#              mapping = aes(x = COGx, y = COGy, color = mgmt_zone),
#              pch = 21, size = 3, show.legend = FALSE) +
#   geom_point(near_avg, mapping = aes(x=COGx,y=COGy, fill = mgmt_zone),
#              color = "black",
#              shape = 23, size = 3,show.legend = FALSE)+
#   geom_point(may_2012_mhw, mapping = aes(x=COGx,y=COGy, fill = mgmt_zone), 
#              color = "black", 
#              shape = 21, size = 3, show.legend = FALSE)+
#   shadowtext::geom_shadowtext(may_2012_mhw, mapping = aes(x=COGx + 6,y=COGy, 
#                                                           color = mgmt_zone,
#                                                           label = "May 2012",
#                                                           fontface = "bold"),
#                               show.legend = FALSE)+
#   coord_sf(xlim =  c(-97.5, -40), ylim = c(11.5, 33.5), expand = TRUE)+
#   theme_bw()+
#   theme(legend.spacing.y = unit(0.2, 'cm'),
#         legend.spacing.x = unit(0.15, 'cm'),
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(5,5,-7,-7),
#         legend.title = element_text(angle = 90),
#         legend.title.align = 0.5)
# 
# NED_MAB / FEC_GOM

###############################################################################
#NEP TROLL
###############################################################################

# ########################################
# #average SST throughout the study period
# ########################################
# Conception_Monterey_Eureka_Columbia_Vancouver_MHW<-here("data","water_temp",
#                                                         "NEP","oisst","Conception_Monterey_Eureka_Columbia_Vancouver_MHW.rds") %>% readRDS()
# 
# NEP_SST<-Conception_Monterey_Eureka_Columbia_Vancouver_MHW %>%
#   filter(yearmon >= as.yearmon("2012-01-01")) %>% 
#   dplyr::select(lat,lon,temp_monthly) %>% 
#   group_by(lat,lon) %>% 
#   summarise(SST = mean(temp_monthly, na.rm=TRUE),.groups = 'drop')

#############################################
#load in mgmt zone shapefiles
#############################################

#mangement zones shapefile
NEP_TROLL_zones<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


#trying to clip the management zones to the coast
land<-st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp",crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NEP_TROLL_zones<-st_difference(NEP_TROLL_zones, st_union(st_combine(land)))

# ###########################################
# #load and combine TROLL landscape metrics dfs
# ###########################################
# 
# NEPTROLL_MHW_total<-here("data","Mgmt_zone","NEP_TROLL",
#                          "NEPTROLL_MHW_total.rds") %>% 
#   readRDS()
# 
# near_avg<-NEPTROLL_MHW_total %>% filter(MHW == 0) %>% 
#   filter(mgmt_zone %in% c('VN','CL','MT','CP')) %>% 
#   group_by(mgmt_zone) %>% 
#   summarise(COGx = mean(COGx, na.rm = TRUE),
#             COGy = mean(COGy, na.rm = TRUE))
# 
# may_2015_mhw<-NEPTROLL_MHW_total %>% 
#   filter(date == as.yearmon("2015-05-01")) %>%
#   filter(mgmt_zone %in% c('VN','CL','MT','CP')) %>% 
#   dplyr::select("mgmt_zone","COGx","COGy")
# 
# VN_CL<-ggplot()+
#   geom_tile(NEP_SST, mapping = aes(x=lon,y=lat,fill=SST))+
#   scale_fill_cmocean(name="thermal",
#                      limits = c(0,30),
#                      guide = guide_colorbar(title.position = "right"))+
#   labs(x="",y="",fill="SST (°C)", title = "VN & CL")+
#   ggnewscale::new_scale_fill()+
#   geom_sf(data = world, color= "black", fill = "grey")+
#   geom_sf(data = NEP_TROLL_zones %>% filter(ET_ID %in% c('VN','CL','MT','CP')), color = "cyan", fill=NA, size = 1)+
#   geom_point(NEPTROLL_MHW_total %>% filter(MHW == 1) %>% filter(mgmt_zone %in% c('VN','CL','MT','CP')), 
#              mapping = aes(x = COGx, y = COGy,color = mgmt_zone),
#              shape = 21, size = 2, show.legend = FALSE) +
#   geom_point(near_avg, mapping = aes(x=COGx,y=COGy, fill = mgmt_zone), 
#              color = "black",
#              shape = 23, size = 3,show.legend = FALSE)+
#   geom_point(may_2015_mhw, mapping = aes(x=COGx,y=COGy, fill = mgmt_zone), 
#              color = "black", 
#              shape = 21, size = 3, show.legend = FALSE)+
#   shadowtext::geom_shadowtext(may_2015_mhw, mapping = aes(x=COGx - 3,y=COGy, 
#                                                           color = mgmt_zone,
#                                                           label = "May 2015",
#                                                           fontface = "bold"),
#                               show.legend = FALSE)+
#   coord_sf(xlim =  c(-134.5, -117), ylim = c(43, 54), expand = TRUE)+
#   theme_bw()+
#   theme(legend.spacing.y = unit(0.2, 'cm'),
#         legend.spacing.x = unit(0.15, 'cm'),
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(5,5,-7,-7),
#         legend.title = element_text(angle = 90),
#         legend.title.align = 0.5)
# 
# MT_CP<-ggplot()+
#   geom_tile(NEP_SST, mapping = aes(x=lon,y=lat,fill=SST))+
#   scale_fill_cmocean(name="thermal",
#                      limits = c(0,30),
#                      guide = guide_colorbar(title.position = "right"))+
#   
#   labs(x="",y="",fill="SST (°C)", title = "MT & CP")+
#   ggnewscale::new_scale_fill()+
#   geom_sf(data = world, color= "black", fill = "grey")+
#   geom_sf(data = NEP_TROLL_zones %>% filter(ET_ID %in% c('VN','CL','MT','CP')), color = "cyan", fill=NA, size = 1)+ 
#   geom_point(NEPTROLL_MHW_total %>% filter(MHW == 1) %>% filter(mgmt_zone %in% c('VN','CL','MT','CP')), 
#              mapping = aes(x = COGx, y = COGy,color = mgmt_zone),
#              shape = 21, size = 2, show.legend = FALSE) +
#   geom_point(near_avg, mapping = aes(x=COGx,y=COGy, fill = mgmt_zone), 
#              color = "black",
#              shape = 23, size = 3,show.legend = FALSE)+
#   geom_point(may_2015_mhw, mapping = aes(x=COGx,y=COGy, fill = mgmt_zone), 
#              color = "black", 
#              shape = 21, size = 3, show.legend = FALSE)+
#   shadowtext::geom_shadowtext(may_2015_mhw, mapping = aes(x=COGx - 3,y=COGy,
#                                                           color = mgmt_zone,
#                                                           label = "May 2015",
#                                                           fontface = "bold"),
#                               show.legend = FALSE)+
#   coord_sf(xlim =  c(-134.5, -117), ylim = c(32, 40), expand = TRUE)+
#   theme_bw()+
#   theme(legend.spacing.y = unit(0.2, 'cm'),
#         legend.spacing.x = unit(0.15, 'cm'),
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(5,5,-7,-7),
#         legend.title = element_text(angle = 90),
#         legend.title.align = 0.5)
# 
# #(VN_CL | NED_MAB) / (MT_CP | FEC_GOM)
# 
# (VN_CL / MT_CP) | (NED_MAB / FEC_GOM)



######################################
#shift / total area in mgmt zone
######################################

####
#PLL
####
NWAPLL_MHW_total<-here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_total.rds") %>% 
  readRDS()

month_avgCOG<-NWAPLL_MHW_total %>% filter(MHW == 0) %>% 
  group_by(mgmt_zone,month) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG_NWA<-NWAPLL_MHW_total %>% #filter(MHW == 1) %>% 
  dplyr::select(MHW,month,event_index,duration_months, 
                mgmt_zone,COGx,COGy,prop_MHW_area,
                mean_SSTa,date,prop_habitat_cells, 
                prop_MHW_cells) %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326")


MHWCOG_NWA$distance <- map_dbl(1:nrow(MHWCOG_NWA), function(x){
  x <- MHWCOG_NWA[x,]
  y <- month_avgCOG[month_avgCOG$mgmt_zone == x$mgmt_zone & month_avgCOG$month == x$month,]
  st_distance(x, y)
})


MHWCOG_NWA<-MHWCOG_NWA %>% mutate(distance = distance * 0.001,
                          lon = sf::st_coordinates(.)[,1],
                          lat = sf::st_coordinates(.)[,2])

mgmt_zone_area<-st_area(NWA_PLL_zones)
units(mgmt_zone_area)<-NULL #remove the units

area_df<-data.frame(mgmt_zone = NWA_PLL_zones$ET_ID %>% unique(),area = mgmt_zone_area*0.000001) #converting to km^2

MHWCOG_NWA<-left_join(MHWCOG_NWA, area_df, by=c("mgmt_zone")) %>% 
  mutate(dis_per_area = distance / area)


MHWCOG_NWA$mgmt_zone<-factor(MHWCOG_NWA$mgmt_zone,
                         levels = c("CAR","GOM","FEC",
                                        "SAR","SAB",
                                        "MAB","NEC","NED"))

# MHWCOG_NWA<-MHWCOG_NWA %>%
#   mutate(bin = cut_interval(MHWCOG_NWA$dis_per_area, n=15)) %>%
#   group_by(bin,mgmt_zone) %>%
#   mutate(mean_prop_hab = mean(prop_habitat_cells, na.rm=TRUE)) %>%
#   ungroup()

#plot the finding
# #trying ridges
# MHWCOG_NWA %>% filter(MHW == 1) %>% 
#   ggplot() +
#   stat_density_ridges(aes(x = dis_per_area, y = mgmt_zone, 
#                                    fill = stat(y)), alpha=0.7,scale = 2.5, 
#                                rel_min_height = 0.01)+
#   scale_fill_viridis(option = "D")+
#   # geom_vline(xintercept = 7.388133e-05, color = "red") +
#   # geom_vline(xintercept = 5.611003e-05, color = "orange") +
#   theme_minimal() +
#   labs(x = "Relative Distance", y="Mangement Zone") +
#   theme(legend.position = "none")+
#   theme(axis.text.x=element_blank())
# 
# 
# #trying lollipop plot
# MHWCOG_NWA %>% 
#   ggplot(aes(x=date, y=dis_per_area)) +
#   geom_point(color = "red") + 
#   geom_segment(aes(x=date, xend=date, y=0, yend=dis_per_area),color = "red",
#                alpha = 0.5)+
#   facet_wrap(~mgmt_zone)+
#   theme_bw() +
#   labs(x = "Date", y="Distance / Area")
# 
# #barplot distributions
# MHWCOG_NWA %>% 
#   as.data.frame() %>% 
#   group_by(bin, mean_prop_hab,mgmt_zone) %>% 
#   summarise(n = n(),.groups = "drop") %>%
#   mutate(mgmt_zone = factor(mgmt_zone,
#                             levels = c("NED","NEC","MAB",
#                                        "SAB","SAR",
#                                        "FEC","GOM","CAR"))) %>%
#   ggplot()+
#   geom_col(aes(bin,mean_prop_hab, fill=mean_prop_hab))+
#   facet_wrap(~mgmt_zone)+
#   cmocean::scale_fill_cmocean(name="haline")+
#   theme_bw()+
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())+
#   labs(x = "Relative Distance", y="Available fishing ground (%)", fill = "Available\nFishing\nGround (%)")
# 
# #scatter plot 
# MHWCOG_NWA %>% 
#   mutate(mgmt_zone = factor(mgmt_zone,
#                             levels = c("NED","NEC","MAB",
#                                        "SAB","SAR",
#                                        "FEC","GOM","CAR"))) %>%
#   #filter(mgmt_zone %in% c("NED","MAB","FEC","CAR")) %>% 
#   ggplot(aes(date,dis_per_area, color = mgmt_zone))+
#   geom_point(alpha = 0.5, size = 2)+
#   geom_smooth(method = "loess",span = 3, se = FALSE, size = 1.2)+
#   facet_wrap(~mgmt_zone, nrow = 4)+
#   theme_bw() +
#   labs(y = "Relative Distance", x="Available Fishing Ground (%)",
#        color = "Management\nZone")

#ks test
MHWCOG_NWA<-MHWCOG_NWA %>%
  group_by(mgmt_zone,month) %>% 
  mutate(month_mean_reldis = mean(dis_per_area, na.rm=TRUE),
         percent_change_reldis=((dis_per_area - month_mean_reldis)/month_mean_reldis)*100) %>% ungroup()

mgmtzones<-unique(as.character(MHWCOG_NWA$mgmt_zone))
ks<-data.frame(mgmt_zone=NA, pvalue=NA, D=NA)
counter = 1
for(i in 1:length(mgmtzones)){
  a<-MHWCOG_NWA %>% filter(mgmt_zone == mgmtzones[i])
  b<-a %>% filter(MHW == 1)
  c<-a %>% filter(MHW == 0)
  print(mgmtzones[i])
  ks_test<-ks.test(log(c$dis_per_area),log(b$dis_per_area),
                   alternative = 'greater')
  pvalue<-round(ks_test$p.value,4)
  d<-round(ks_test$statistic,4)
  ks[counter,1]<-mgmtzones[i]
  ks[counter,2]<-pvalue
  ks[counter,3]<-d
    
    
  #ks[counter,1]<-paste0(mgmtzones[i]," : ","P-value = ",pvalue)
  counter = counter + 1
}

ks_pvD_NWA <- ks


# names(ks_pvD_NWA) <- unique(as.character(MHWCOG_NWA$mgmt_zone))
# 
# labels_NWA<-data.frame(mgmt_zone = factor(c("NED","MAB","FEC","CAR")),
#                        label = c("(E)","(F)","(G)","(H)"))
# #density plots
# NWA_plot<-MHWCOG_NWA %>% filter(mgmt_zone %in% c("NED", "MAB", "FEC", "CAR")) %>% 
#   group_by(mgmt_zone,month) %>% 
#   mutate(month_mean_reldis = mean(dis_per_area, na.rm=TRUE),
#          percent_change_reldis=((dis_per_area - month_mean_reldis)/month_mean_reldis)*100) %>% ungroup %>%  
#   mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
#          MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
#   ggplot() +
#   geom_density(aes(log(dis_per_area*10e5), fill = MHW, color = MHW),
#                alpha = 0.1)+
#   theme_minimal()+
#   facet_wrap(~mgmt_zone, labeller = labeller(mgmt_zone = kspv_NWA), nrow = 4)+
#   labs(title = "", x = "log(Relative Distance x 10e5)", y = "Density")+
#   theme(legend.title = element_blank())+
#   geom_text(data=labels_NWA, aes(label = label, x = -Inf, y = Inf),
#             hjust = 0, vjust = 1, fontface="bold")
# 


NWA_plot<-MHWCOG_NWA %>%
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
  ggplot()+
  introdataviz::geom_split_violin(aes(mgmt_zone, y=dis_per_area*1000000,
                                      fill=MHW), alpha = 0.4)+
  geom_boxplot(aes(mgmt_zone, y=dis_per_area*1000000,fill=MHW),
               width = .2, alpha = .6, show.legend = FALSE) + 
  theme_bw()+
  #north
  labs(title = "", y = "Relative Distance x 10e5", 
       x = "", fill = "")+
  geom_label(ks_pvD_NWA %>% filter(mgmt_zone %in% c("NED","NEC","MAB")), 
             mapping=aes(x=c(8.2,7.2,6.2), y=200, label = D))+
  geom_label(ks_pvD_NWA %>% filter(mgmt_zone %in% c("NED","NEC","MAB")), 
             mapping=aes(x=c(8.2,7.2,6.2), y=150, label = pvalue))+
  geom_text(ks_pvD_NWA %>% filter(mgmt_zone %in% c("NED","NEC","MAB")), 
            mapping=aes(x=c(8.2,7.2,6.2),y=178, label = "D:"))+
  geom_text(ks_pvD_NWA %>% filter(mgmt_zone %in% c("NED","NEC","MAB")), 
            mapping=aes(x=c(8.2,7.2,6.2),y=116, label = "p-value:"))+
  #south
  geom_label(ks_pvD_NWA %>% filter(mgmt_zone %in% c("SAB","SAR","FEC",
                                                    "GOM","CAR")), 
             mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2), y=200, label = D),
             fontface = "bold")+
  geom_label(ks_pvD_NWA %>% filter(mgmt_zone %in% c("SAB","SAR","FEC",
                                                    "GOM","CAR")), 
             mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2), y=150, label = pvalue),
             fontface = "bold")+
  geom_text(ks_pvD_NWA %>% filter(mgmt_zone %in% c("SAB","SAR","FEC",
                                                   "GOM","CAR")), 
            mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2),y=178, label = "D:"),
            fontface = "bold")+
  geom_text(ks_pvD_NWA %>% filter(mgmt_zone %in% c("SAB","SAR","FEC",
                                                   "GOM","CAR")), 
            mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2),y=116, label = "p-value:"),
            fontface = "bold")+
  coord_flip()



######
#TROLL
######
NEPTROLL_MHW_total<-here("data","Mgmt_zone","NEP_TROLL",
                         "NEPTROLL_MHW_total.rds") %>% 
  readRDS()

NEPTROLL_MHW_total<-here("data","Mgmt_zone","NEP_TROLL","NEPTROLL_MHW_total.rds") %>% 
  readRDS()

month_avgCOG<-NEPTROLL_MHW_total %>% filter(MHW == 0) %>% 
  group_by(mgmt_zone,month) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG_NEP<-NEPTROLL_MHW_total %>% #filter(MHW == 1) %>% 
  dplyr::select(MHW,month,event_index,duration_months, 
                mgmt_zone,COGx,COGy,prop_MHW_area,
                mean_SSTa,date,prop_habitat_cells, 
                prop_MHW_cells) %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326")


MHWCOG_NEP$distance <- map_dbl(1:nrow(MHWCOG_NEP), function(x){
  x <- MHWCOG_NEP[x,]
  y <- month_avgCOG[month_avgCOG$mgmt_zone == x$mgmt_zone & month_avgCOG$month == x$month,]
  st_distance(x, y)
})


MHWCOG_NEP<-MHWCOG_NEP %>% mutate(distance = distance * 0.001,
                                  lon = sf::st_coordinates(.)[,1],
                                  lat = sf::st_coordinates(.)[,2])

mgmt_zone_area<-st_area(NEP_TROLL_zones)
units(mgmt_zone_area)<-NULL #remove the units

area_df<-data.frame(mgmt_zone = NEP_TROLL_zones$ET_ID %>% unique(),area = mgmt_zone_area*0.000001) #converting to km^2

MHWCOG_NEP<-left_join(MHWCOG_NEP, area_df, by=c("mgmt_zone")) %>% 
  mutate(dis_per_area = distance / area)


MHWCOG_NEP$mgmt_zone<-factor(MHWCOG_NEP$mgmt_zone,
                             levels = rev(c("VN","CL","EK",
                                        "MT","CP")))

# MHWCOG<-MHWCOG %>%
#   mutate(bin = cut_interval(MHWCOG$dis_per_area, n=15)) %>%
#   group_by(bin,mgmt_zone) %>%
#   mutate(mean_prop_hab = mean(prop_habitat_cells, na.rm=TRUE)) %>%
#   ungroup()

#plot the finding
# #trying ridges
# MHWCOG %>% filter(MHW == 1) %>% 
#   ggplot() +
#   stat_density_ridges(aes(x = dis_per_area, y = mgmt_zone, 
#                                    fill = stat(y)), alpha=0.7,scale = 2.5, 
#                                rel_min_height = 0.01)+
#   scale_fill_viridis(option = "D")+
#   # geom_vline(xintercept = 7.388133e-05, color = "red") +
#   # geom_vline(xintercept = 5.611003e-05, color = "orange") +
#   theme_minimal() +
#   labs(x = "Relative Distance", y="Mangement Zone") +
#   theme(legend.position = "none")+
#   theme(axis.text.x=element_blank())
# 
# 
# #trying lollipop plot
# MHWCOG %>% 
#   ggplot(aes(x=date, y=dis_per_area)) +
#   geom_point(color = "red") + 
#   geom_segment(aes(x=date, xend=date, y=0, yend=dis_per_area),color = "red",
#                alpha = 0.5)+
#   facet_wrap(~mgmt_zone)+
#   theme_bw() +
#   labs(x = "Date", y="Distance / Area")
# 
# #barplot distributions
# MHWCOG %>% 
#   as.data.frame() %>% 
#   group_by(bin, mean_prop_hab,mgmt_zone) %>% 
#   summarise(n = n(),.groups = "drop") %>%
#   mutate(mgmt_zone = factor(mgmt_zone,
#                             levels = c("NED","NEC","MAB",
#                                        "SAB","SAR",
#                                        "FEC","GOM","CAR"))) %>%
#   ggplot()+
#   geom_col(aes(bin,mean_prop_hab, fill=mean_prop_hab))+
#   facet_wrap(~mgmt_zone)+
#   cmocean::scale_fill_cmocean(name="haline")+
#   theme_bw()+
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())+
#   labs(x = "Relative Distance", y="Available fishing ground (%)", fill = "Available\nFishing\nGround (%)")
# 
# #scatter plot 
# MHWCOG %>% 
#   mutate(mgmt_zone = factor(mgmt_zone,
#                             levels = c("NED","NEC","MAB",
#                                        "SAB","SAR",
#                                        "FEC","GOM","CAR"))) %>%
#   #filter(mgmt_zone %in% c("NED","MAB","FEC","CAR")) %>% 
#   ggplot(aes(date,dis_per_area, color = mgmt_zone))+
#   geom_point(alpha = 0.5, size = 2)+
#   geom_smooth(method = "loess",span = 3, se = FALSE, size = 1.2)+
#   facet_wrap(~mgmt_zone, nrow = 4)+
#   theme_bw() +
#   labs(y = "Relative Distance", x="Available Fishing Ground (%)",
#        color = "Management\nZone")

#ks test
MHWCOG_NEP<-MHWCOG_NEP %>%
  group_by(mgmt_zone,month) %>% 
  mutate(month_mean_reldis = mean(dis_per_area, na.rm=TRUE),
         percent_change_reldis=((dis_per_area - month_mean_reldis)/month_mean_reldis)*100) %>% ungroup()

mgmtzones<-unique(as.character(MHWCOG_NEP$mgmt_zone))
ks<-data.frame(mgmt_zone=NA, pvalue=NA, D=NA)
counter = 1
for(i in 1:length(mgmtzones)){
  a<-MHWCOG_NEP %>% filter(mgmt_zone == mgmtzones[i])
  b<-a %>% filter(MHW == 1)
  c<-a %>% filter(MHW == 0)
  print(mgmtzones[i])
  ks_test<-ks.test(log(c$dis_per_area),log(b$dis_per_area),
                   alternative = 'greater')
  pvalue<-round(ks_test$p.value,4)
  d<-round(ks_test$statistic,4)
  ks[counter,1]<-mgmtzones[i]
  ks[counter,2]<-pvalue
  ks[counter,3]<-d
  
  
  #ks[counter,1]<-paste0(mgmtzones[i]," : ","P-value = ",pvalue)
  counter = counter + 1
}

ks_pvD_NEP <- ks
#names(kspv_NEP) <- unique(as.character(MHWCOG_NEP$mgmt_zone))
# 
# labels_NEP<-data.frame(mgmt_zone = factor(c("VN","CL","EK","MT")),
#                           label = c("(A)","(B)","(C)", "(D)"))
# #density plots
# NEP_plot<-MHWCOG_NEP %>% filter(mgmt_zone %in% c("VN","CL","EK","MT")) %>% 
#   group_by(mgmt_zone,month) %>% 
#   mutate(month_mean_reldis = mean(dis_per_area, na.rm=TRUE),
#          percent_change_reldis=((dis_per_area - month_mean_reldis)/month_mean_reldis)*100) %>% ungroup %>%  
#   mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
#          MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
#   ggplot() +
#   geom_density(aes(log(dis_per_area*10e5), fill = MHW, color = MHW),
#                alpha = 0.1, show.legend = FALSE)+
#   theme_minimal()+
#   facet_wrap(~mgmt_zone, labeller = labeller(mgmt_zone = kspv_NEP),nrow = 4)+
#   labs(title = "", x = "log(Relative Distance x 10e5)", y = "Density")+
#   theme(legend.title = element_blank())+
#   geom_text(data=labels_NEP, aes(label = label, x = -Inf, y = Inf),
#             hjust = 0, vjust = 1, fontface="bold")



NEP_plot<-MHWCOG_NEP %>%
  mutate(MHW = if_else(MHW == 1, "MHW", "Non-MHW"),
         MHW = factor(MHW, levels = c("MHW","Non-MHW"))) %>% 
  filter(mgmt_zone %in% c("VN","CL","EK","MT")) %>% 
  ggplot()+
  introdataviz::geom_split_violin(aes(mgmt_zone, y=dis_per_area*1000000,
                                      fill=MHW), alpha = 0.4,show.legend = FALSE)+
  geom_boxplot(aes(mgmt_zone, y=dis_per_area*1000000,fill=MHW),
               width = .2, alpha = .6, show.legend = FALSE) + 
  theme_bw()+
  labs(title = "", y = "Relative Distance x 10e5", 
       x = "")+
  #peripheral
  geom_text(ks_pvD_NEP %>% filter(mgmt_zone %in% c("VN","MT")), 
             mapping=aes(x=c(1.2,4.2), y=440, label = "D:"),
             fontface = "bold")+
  geom_text(ks_pvD_NEP %>% filter(mgmt_zone %in% c("VN","MT")), 
             mapping=aes(x=c(1.2,4.2), y=255, label = "p-value:"),
             fontface = "bold")+
  geom_label(ks_pvD_NEP %>% filter(mgmt_zone %in% c("VN","MT")), 
            mapping=aes(x=c(1.2,4.2),y=505, label = D),
            fontface = "bold")+
  geom_label(ks_pvD_NEP %>% filter(mgmt_zone %in% c("VN","MT")), 
            mapping=aes(x=c(1.2,4.2),y=355, label = pvalue),
            fontface = "bold")+
  #central
  geom_text(ks_pvD_NEP %>% filter(mgmt_zone %in% c("CL","EK")), 
             mapping=aes(x=c(2.2,3.2), y=440, label = "D:"))+
  geom_text(ks_pvD_NEP %>% filter(mgmt_zone %in% c("CL","EK")), 
             mapping=aes(x=c(2.2,3.2), y=260, label = "p-value:"))+
  geom_label(ks_pvD_NEP %>% filter(mgmt_zone %in% c("CL","EK")), 
            mapping=aes(x=c(2.2,3.2),y=505, label = D))+
  geom_label(ks_pvD_NEP %>% filter(mgmt_zone %in% c("CL","EK")), 
            mapping=aes(x=c(2.2,3.2),y=355, label = pvalue))+
  coord_flip()#+scale_y_reverse()+scale_x_discrete(position = "top")



F5_Relative_Displacement_distributions<-cowplot::plot_grid(NEP_plot,NWA_plot,labels = c('(A)', '(B)'), label_size = 12,
                   rel_widths = c(1,1.5))

ggsave(here("Plots","both_coasts","F5_Relative_Displacement_distributions.png"),
       width = 10, height = 7, units = "in", dpi = 300)
ggsave(here("Plots","both_coasts","F5_Relative_Displacement_distributions.svg"),
       width = 10, height = 7, units = "in", dpi = 300)
