#Identifying the peak fishing seasons for each management zone which will be used in the nalyses to see when a MHW occurs during that time. 

library(tidyverse)
library(sf)
library(here)
library(patchwork)
#mangement zones shapefile
NWA_PLL_zones<-sf::st_read("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Shapefiles/NWA_PLL_MangementZone/Zones_PLL.shp", crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% st_transform()

NWA_PLL_zones$ET_ID<-c("CAR","FEC","GOM",
                       "MAB","NCA","NEC",
                       "NED","SAB","SAR",
                       "TUN","TUS")


NWA_PLL<-here("data","AIS_processed","NWA_PLL","Pres_Abs_2013to2020_NWA_USA_PLL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull_v2_enhanced.rds") %>% 
  readRDS() %>% 
  filter(Pres_abs ==1) %>% 
  sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>% 
  mutate(lon = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  # find points within mgmt zone
  st_join(., NWA_PLL_zones) %>% as.data.frame() %>% filter(ET_ID != "TUN" & ET_ID !="TUS" & ET_ID !="NCA")



NWA_PLL %>% 
  mutate(month=as.factor(month)) %>%
  # group_by(ET_ID,month) %>% 
  # summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE), .groups = "drop") %>%
  mutate(season = if_else(month %in% c(6,7,8,9,10,11), "summer","winter")) %>% 
  group_by(ET_ID,season) %>% 
  summarise(fishing_hours=sum(fishing_hours, na.rm = TRUE)) %>% 
  mutate(prop = prop.table(fishing_hours)*100) %>%
  arrange(ET_ID,-fishing_hours) %>% View()
  
  
#plot
summer<- NWA_PLL %>% filter(ET_ID %in% c("MAB","NEC","NED")) %>% 
  ggplot()+
  geom_bar(aes(x=factor(month, 
                        levels=c("6","7","8","9","10", "11",
                                 "12","1","2","3","4","5")),
               y=fishing_hours), stat = "identity")+
  geom_vline(xintercept = 6.5, color = "red")+
  facet_wrap(~ET_ID)+
  labs(x = expression("Summer/Fall" %<-%Month%->% "Winter/Spring"), y = "Fishing Hours", 
       title = "Summer - Fall Peak Fishing Season")+
  theme_bw()

`%notin%` <- Negate(`%in%`) #oppsite of %in%

winter<- NWA_PLL %>% filter(ET_ID %notin% c("MAB","NEC","NED")) %>%
  ggplot()+
  geom_bar(aes(x=factor(month, 
                        levels=c("6","7","8","9","10", "11",
                                 "12","1","2","3","4","5")),
               y=fishing_hours), stat = "identity")+
  geom_vline(xintercept = 6.5, color = "red")+
  facet_wrap(~ET_ID, ncol = 6)+
  labs(x = expression("Summer/Fall" %<-%Month%->% "Winter/Spring"), y = "Fishing Hours", 
       title = "Winter - Spring Peak Fishing Season")+
  theme_bw()

summer / winter




