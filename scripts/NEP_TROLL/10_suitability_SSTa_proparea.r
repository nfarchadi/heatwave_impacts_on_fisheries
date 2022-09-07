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
#NEP mgmts zones
############################################################

Conception_Monterey_Eureka_Columbia_Vancouver_MHW<-here("data",
                                                        "water_temp","NEP","oisst","Conception_Monterey_Eureka_Columbia_Vancouver_MHW.rds") %>% readRDS()


NEP_MHW_monthly_metrics_Conception_Monterey_Eureka_Columbia_Vancouver<-mgmt_zone_MHW(Conception_Monterey_Eureka_Columbia_Vancouver_MHW)


NEP_MHW_monthly_metrics_Conception_Monterey_Eureka_Columbia_Vancouver<-bind_rows(NEP_MHW_monthly_metrics_Conception_Monterey_Eureka_Columbia_Vancouver) %>% 
  dplyr::select(1,2,3,39:47) %>% rename("n.cell_MHW"="V44",
                                        "n.cell_noMHW"="V45",
                                        "n.cell_totalMHW"="V46",
                                        "mgmt_zone"="V47") %>% 
  mutate(total.area= total.area/1000000) %>% 
  group_by(date,mgmt_zone) %>% 
  mutate(total.area.mgmt.zone_MHW = sum(total.area, na.rm = TRUE)) %>% ungroup %>%
  filter(class == 1) 

rm(Conception_Monterey_Eureka_Columbia_Vancouver_MHW)


NEP_MHW_monthly_metrics<-NEP_MHW_monthly_metrics_Conception_Monterey_Eureka_Columbia_Vancouver %>%
  mutate(month = lubridate::month(date))

#This is landscape metrics for the NEP TROLL
NEP_TROLL_landscape_metrics<-readRDS(here("data","Mgmt_zone","NEP_TROLL","NEP_TROLL_landscapemetrics_monthly.rds"))

#suitable fishing habitat area ~ area of anomalously warmer water
NEPTROLL_lsm_month<-NEP_TROLL_landscape_metrics %>% 
  dplyr::select(date,total.area,mgmt_zone,total.area.mgmt.zone, 
                COGx, COGy, COGx.sd, COGy.sd,X25,X75,Y25,Y75,n.cell_habitat,
                n.cell_nohabitat,n.cell_total) %>% 
  rename("NEP_TROLL.total.area"="total.area") %>% 
  mutate(prop_habitat_area = NEP_TROLL.total.area/total.area.mgmt.zone,
         prop_habitat_cells = n.cell_habitat / n.cell_total) 

NEPMHW_lsm_month<-NEP_MHW_monthly_metrics %>%
  dplyr::select(date,total.area,total.area.mgmt.zone_MHW, 
                mean_SSTa, max_SSTa, mgmt_zone,n.cell_MHW,
                n.cell_noMHW,n.cell_totalMHW) %>%
  rename("NEP_MHW.total.area" = "total.area") %>% 
  mutate(prop_MHW_area = NEP_MHW.total.area/total.area.mgmt.zone_MHW,
         prop_MHW_cells = n.cell_MHW / n.cell_totalMHW)%>% 
  mutate(mgmt_zone = case_when(mgmt_zone == "Vancouver" ~ "VN",
                               mgmt_zone == "Columbia" ~ "CL",
                               mgmt_zone == "Eureka" ~ "EK",
                               mgmt_zone == "Monterey" ~ "MT",
                               mgmt_zone == "Conception" ~ "CP"))

#combine the suitability and MHW class stat dfs and classify MHW sizes
NEPTROLL_MHW<-left_join(NEPTROLL_lsm_month,NEPMHW_lsm_month,
                          by= c("date","mgmt_zone")) %>% 
  mutate(MHW = case_when(prop_MHW_area >= .3 ~ "Large MHW",
                         prop_MHW_area < .3 & prop_MHW_area >= .1 ~ "Small MHW",
                         prop_MHW_area < .1 ~ "Near-Average",
                         TRUE ~ "Near-Average")) %>%
  mutate(month = lubridate::month(date)) 

saveRDS(NEPTROLL_MHW, here("data","Mgmt_zone","NEP_TROLL","NEPTROLL_MHW.rds"))

#proportion plot
CCS_plot<-NEPTROLL_MHW %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("VN","CL","EK","MT",
                                                  "CP"))) %>% 
  na.omit() %>% 
  mutate(month = lubridate::month(date)) %>% group_by(mgmt_zone) %>% 
  mutate(slope = lm(prop_habitat_cells ~ prop_MHW_cells)$coefficients[2]) %>%
  ungroup %>% 
  ggplot(aes(x=prop_MHW_cells, y=prop_habitat_cells,
             color = mean_SSTa))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  scale_color_viridis(name=stringr::str_wrap("SSTa\nIntensity (°C)", width = 13),
                      option ="inferno",
                      guide = guide_colorbar(barwidth = 1.5, 
                                             barheight = 12,
                                             #label.hjust = .5,
                                             title.position = "top",
                                             title.hjust = .5))+
  facet_wrap(~mgmt_zone, scales = "free", nrow = 1)+
  geom_label(aes(label=paste("Slope =",round(slope,2))), color = "black", 
             x = Inf, y = Inf, hjust=1, vjust=1) +
  theme_minimal()+
  labs(x=expression("Proportion of MHW"~(km^2)), y=expression("Proportion of Suitable Fishing Grounds"~(km^2)))

CCS_plot











####################################################
#same graph but maximum intensity in the color scale
####################################################
CCS_plot<-NEPTROLL_MHW %>% 
  mutate(mgmt_zone = factor(mgmt_zone, levels = c("Vancouver","Columbia","Eureka","Monterey","Conception"))) %>% 
  na.omit() %>% 
  mutate(month = lubridate::month(date)) %>% 
  ggplot(aes(x=prop_MHW_area, y=prop_habitat_area,
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

CCS_plot
