library(here)
library(tidyverse)
library(pracma)
library(heatwaveR)
library(sf)
library(patchwork)
library(raster)
library(viridis)

sf_use_s2(FALSE)# need to do this to remove spherical geometry

#So far we have calculated the climatology, calculated and detrended the anomalies, calculated the seasonal 90% threshold, and Identified MHW.  

#now lets find the size and magnitude of the anomalies and see how they related with habitat suitability 

#function to find all these metrics for the SSTa
mgmt_zone_MHW<-function(data){
  
  SSTa_metrics_list<-list()
  
  mgmt_zones<-data$ET_ID %>% unique()
  
  for(i in 1:length(mgmt_zones)){
    oisst_mgmtzone<-data %>% filter(ET_ID == mgmt_zones[i])
  
    dates<-oisst_mgmtzone %>% filter(year >=2012) %>% 
      pull(yearmon) %>% unique()
    
    for(j in 1:length(dates)){
      print(paste(dates[j], mgmt_zones[i], sep= " "))
      
      OISST_date<-oisst_mgmtzone %>% filter(yearmon == dates[j])
      
      OISST_date2<-OISST_date
      
      #binary classify anomalies
      OISST_date<-OISST_date %>% 
        dplyr::select(lon,lat,MHW)
      
      OISST_date<-raster::rasterFromXYZ(OISST_date)
      
      
      if(all(raster::values(OISST_date)==0,na.rm=TRUE)){
        print(paste("Skipped",dates[j]))
        next
      }
      
      OISST_date2<-OISST_date2 %>% 
        filter(MHW == 1)
      
      #mean and max SST anom of warmer waters
      mean_SSTa<-OISST_date2 %>% summarise(mean_SSTa = mean(detrend, na.rm=TRUE))
      max_SSTa<-OISST_date2 %>% summarise(max_SSTa = max(detrend, na.rm=TRUE))
      
      #want to know the total amount of cells for entire zone and each class
      n.cell_MHW<-ncell(OISST_date[values(OISST_date)==1])
      n.cell_noMHW<-ncell(OISST_date[values(OISST_date)==0])
      n.cell_totalMHW<-ncell(OISST_date)
      
      
      SSTa_25=quantile(OISST_date2$detrend)[2] %>% as_tibble() %>% setNames("SSTa_25") #25% quantile
      SSTa_75=quantile(OISST_date2$detrend)[4] %>% as_tibble() %>% setNames("SSTa_75") #%>% as.vector() #75% quantile
      
      
      stat=SDMTools::ClassStat(OISST_date,cellsize = 0.25,bkgd=NA,latlon = TRUE) #%>% filter(class==1) #calculate classStats from the SDMTools package
      
      
      stat=stat %>% mutate(date=dates[j])
      stat[,ncol(stat)+1]<-SSTa_25
      stat[,ncol(stat)+1]<-SSTa_75
      stat[,ncol(stat)+1]<-mean_SSTa
      stat[,ncol(stat)+1]<-max_SSTa
      stat[,ncol(stat)+1]<-n.cell_MHW
      stat[,ncol(stat)+1]<-n.cell_noMHW
      stat[,ncol(stat)+1]<-n.cell_totalMHW
      stat[,ncol(stat)+1]<-mgmt_zones[i]
      
      SSTa_metrics_list[[length(SSTa_metrics_list)+1]]=stat
      
    }
  }
  return(SSTa_metrics_list)
}

############################################################
#Summer/Fall Winter Months -- MAB NEC NED mgmts zones
############################################################

MAB_NEC_NED_MHW<-here("data","water_temp","NWA","oisst",
                      "MAB_NEC_NED_MHW.rds") %>% readRDS()

MAB_NEC_NED_MHW$yearmon<-zoo::as.yearmon(MAB_NEC_NED_MHW$yearmon)

#lets run this for just MAB NEC NED mgmts zones
NWA_MHW_monthly_metrics_MAB_NEC_NED<-mgmt_zone_MHW(MAB_NEC_NED_MHW)


NWA_MHW_monthly_metrics_MAB_NEC_NED<-bind_rows(NWA_MHW_monthly_metrics_MAB_NEC_NED) %>% 
  dplyr::select(1,2,3,39:47) %>% rename("n.cell_MHW"="V44",
                                        "n.cell_noMHW"="V45",
                                        "n.cell_totalMHW"="V46",
                                        "mgmt_zone"="V47") %>% 
  mutate(total.area= total.area/1000000) %>% 
  group_by(date,mgmt_zone) %>% 
  mutate(total.area.mgmt.zone_MHW = sum(total.area, na.rm = TRUE)) %>% ungroup %>%
  filter(class == 1) 

rm(MAB_NEC_NED_MHW)

NWA_MHW_monthly_metrics_summer<-NWA_MHW_monthly_metrics_MAB_NEC_NED %>%
  mutate(month = lubridate::month(date))

#This is landscape metrics for the NWA PLL
NWA_PLL_landscape_metrics<-readRDS(here("data","Mgmt_zone","NWA_PLL_landscapemetrics_monthly.rds"))

#suitable fishing habitat area ~ area of anomalously warmer water
NWAPLL_lsm_month_summer<-NWA_PLL_landscape_metrics %>% 
  dplyr::select(date,total.area,mgmt_zone,total.area.mgmt.zone, 
                COGx, COGy, COGx.sd, COGy.sd,X25,X75,Y25,Y75,n.cell_habitat,
                n.cell_nohabitat,n.cell_total) %>% 
  rename("NWA_PLL.total.area"="total.area") %>% 
  mutate(prop_habitat_area = NWA_PLL.total.area/total.area.mgmt.zone,
         prop_habitat_cells = n.cell_habitat / n.cell_total)

NWAMHW_lsm_month_summer<-NWA_MHW_monthly_metrics_summer %>%
  dplyr::select(date,total.area,total.area.mgmt.zone_MHW, 
                mean_SSTa, max_SSTa, mgmt_zone,n.cell_MHW,
                n.cell_noMHW,n.cell_totalMHW) %>%
  rename("NWA_MHW.total.area" = "total.area") %>% 
  mutate(prop_MHW_area = NWA_MHW.total.area/total.area.mgmt.zone_MHW,
         prop_MHW_cells = n.cell_MHW / n.cell_totalMHW)

#combine the suitability and MHW class stat dfs and classify MHW sizes
NWAPLL_MHW_summer<-left_join(NWAPLL_lsm_month_summer,NWAMHW_lsm_month_summer,
                          by= c("date","mgmt_zone")) %>% 
  filter(mgmt_zone %in% c("MAB","NEC","NED")) %>% 
  mutate(MHW = case_when(prop_MHW_area >= .3 ~ "Large MHW",
                         prop_MHW_area < .3 & prop_MHW_area >= .1 ~ "Small MHW",
                         prop_MHW_area < .1 ~ "Near-Average",
                         TRUE ~ "Near-Average")) %>%
  mutate(month = lubridate::month(date)) 

saveRDS(NWAPLL_MHW_summer, here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_summer.rds"))

#proportion plot
summer_plot<-NWAPLL_MHW_summer %>% na.omit() %>% 
  mutate(month = lubridate::month(date)) %>% 
  #filter(month %in% c(12,1,2,3,4,5)) %>% 
  ggplot(aes(x=prop_MHW_cells, y=prop_habitat_cells,
             color = mean_SSTa))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_viridis(name=stringr::str_wrap("SSTa\nMagnitude (°C)", width = 13),
                      option ="inferno",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 12,
                                             #label.hjust = .5,
                                             title.position = "top",
                                             title.hjust = .5))+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x=expression("Proportion of MHW"~(km^2)), y=expression("Proportion of Suitable Fishing Habitat"~(km^2)))

summer_plot
############################################################
#Winter/Spring Months -- GOM CAR FEC SAR SAB mgmts zones
############################################################

GOM_CAR_FEC_SAR_SAB_MHW<-here("data","water_temp","NWA","oisst",
                      "GOM_CAR_FEC_SAR_SAB_MHW.rds") %>% readRDS()

#lets run this for just GOM CAR FEC SAR SAB mgmts zones
NWA_MHW_monthly_metrics_GOM_CAR_FEC_SAR_SAB<-mgmt_zone_MHW(GOM_CAR_FEC_SAR_SAB_MHW)


NWA_MHW_monthly_metrics_GOM_CAR_FEC_SAR_SAB<-bind_rows(NWA_MHW_monthly_metrics_GOM_CAR_FEC_SAR_SAB) %>%
  dplyr::select(1,2,3,39:47) %>% rename("n.cell_MHW"="V44",
                                        "n.cell_noMHW"="V45",
                                        "n.cell_totalMHW"="V46",
                                        "mgmt_zone"="V47") %>%  
  mutate(total.area= total.area/1000000) %>% 
  group_by(date,mgmt_zone) %>% 
  mutate(total.area.mgmt.zone_MHW = sum(total.area, na.rm = TRUE)) %>% ungroup %>% 
  filter(class == 1)

rm(GOM_CAR_FEC_SAR_SAB_MHW)

NWA_MHW_monthly_metrics_winter<-NWA_MHW_monthly_metrics_GOM_CAR_FEC_SAR_SAB %>% 
  mutate(month = lubridate::month(date))


#This is landscape metrics for the NWA PLL
NWA_PLL_landscape_metrics_winter<-readRDS(here("data","Mgmt_zone","NWA_PLL_landscapemetrics_monthly.rds"))


#suitable fishing habitat area ~ area of anomalously warmer water
NWAPLL_lsm_month_winter<-NWA_PLL_landscape_metrics %>% 
  dplyr::select(date,total.area,mgmt_zone,total.area.mgmt.zone, 
                COGx, COGy, COGx.sd, COGy.sd,X25,X75,Y25,Y75,n.cell_habitat,
                n.cell_nohabitat, n.cell_total) %>% 
  rename("NWA_PLL.total.area"="total.area") %>% 
  mutate(prop_habitat_area = NWA_PLL.total.area/total.area.mgmt.zone,
         prop_habitat_cells = n.cell_habitat / n.cell_total)

NWAMHW_lsm_month_winter<-NWA_MHW_monthly_metrics_winter %>% 
  dplyr::select(date,total.area,total.area.mgmt.zone_MHW, 
                mean_SSTa, max_SSTa, mgmt_zone,n.cell_MHW,
                n.cell_noMHW, n.cell_totalMHW) %>%
  rename("NWA_MHW.total.area" = "total.area") %>% 
  mutate(prop_MHW_area = NWA_MHW.total.area/total.area.mgmt.zone_MHW,
         prop_MHW_cells = n.cell_MHW / n.cell_totalMHW)

NWAPLL_MHW_winter<-left_join(NWAPLL_lsm_month_winter,NWAMHW_lsm_month_winter,
                          by= c("date","mgmt_zone"))  %>% 
  filter(mgmt_zone %in% c("GOM", "CAR", "FEC", "SAR", "SAB")) %>% 
  mutate(MHW = case_when(prop_MHW_area >= .3 ~ "Large MHW",
                         prop_MHW_area < .3 & prop_MHW_area >= .1 ~ "Small MHW",
                         prop_MHW_area < .1 ~ "Near-Average",
                         TRUE ~ "Near-Average")) %>%
  mutate(month = lubridate::month(date))

saveRDS(NWAPLL_MHW_winter, here("data","Mgmt_zone","NWA_PLL","NWAPLL_MHW_winter.rds"))

#proportion plot
winter_plot<-NWAPLL_MHW_winter %>% na.omit() %>% 
  mutate(month = lubridate::month(date)) %>% 
  #filter(month %in% c(6,7,8,9,10,11)) %>% 
  ggplot(aes(x=prop_MHW_area, y=prop_habitat_area,
             color = mean_SSTa))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_viridis(name=stringr::str_wrap("SSTa\nMagnitude (°C)", width = 13),
                      option ="inferno",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 12,
                                             title.position = "top",
                                             title.hjust = .5))+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x=expression("Proportion of MHW"~(km^2)), y=expression("Proportion of Suitable Fishing Habitat"~(km^2)))

summer_plot / winter_plot



####################################################
#same graph but maximum intensity in the color scale
####################################################
summer_plot<-NWAPLL_MHW_summer %>% na.omit() %>% 
  mutate(month = lubridate::month(date)) %>% 
  #filter(month %in% c(12,1,2,3,4,5)) %>% 
  ggplot(aes(x=prop_MHW_cells, y=prop_habitat_cells,
             color = max_SSTa))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_viridis(name=stringr::str_wrap("Maximum Intensity (°C)", width = 13),
                      option ="inferno",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 12,
                                             #label.hjust = .5,
                                             title.position = "top",
                                             title.hjust = .5))+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x=expression("Proportion of MHW"~(km^2)), y=expression("Proportion of Suitable Fishing Habitat"~(km^2)))

winter_plot<-NWAPLL_MHW_winter %>% na.omit() %>% 
  mutate(month = lubridate::month(date)) %>% 
  #filter(month %in% c(6,7,8,9,10,11)) %>% 
  ggplot(aes(x=prop_MHW_area, y=prop_habitat_area,
             color = max_SSTa))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_viridis(name=stringr::str_wrap("Maximum Intensity (°C)", width = 13),
                      option ="inferno",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 12,
                                             title.position = "top",
                                             title.hjust = .5))+
  facet_wrap(~mgmt_zone, scales = "free")+
  theme_minimal()+
  labs(x=expression("Proportion of MHW"~(km^2)), y=expression("Proportion of Suitable Fishing Habitat"~(km^2)))

summer_plot / winter_plot
