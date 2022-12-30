# Fig 3. Seasonally averaged spatial predictions of suitable fishing grounds for both fishing fleets (2012-2020).

library(tidyverse)
library(patchwork)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(cmocean)
library(zoo)
library(here)
library(sf)
library(tidyverse)
library(viridis)

world <- ne_countries(scale = "medium", returnclass = "sf")
sf_use_s2(FALSE)# need to do this to remove spherical geometry

#############################################
#load in mgmt area shapefiles
#############################################

#mangement areas shapefile
NWA_PLL_areas<-here("data","shapefiles","NWA_PLL","areas_PLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
#trying to clip the management areas to the coast

#trying to clip the management areas to the coast
land<-here("data","shapefiles","Land","GSHHS_l_L1.shp") %>% st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") 

NWA_PLL_areas<-st_difference(NWA_PLL_areas, st_union(st_combine(land)))

NEP_TROLL_areas<-here("data","shapefiles","NEP_TROLL","areas_TROLL.shp") %>% sf::st_read(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

NEP_TROLL_areas<-st_difference(NEP_TROLL_areas, st_union(st_combine(land)))

################################
#load in NWA PLL VDM predictions
################################

NWA_PLL_pred<-here("data","spatial_predictions",
                   "NWA_PLL","NWA_PLL_monthly_predictions_2012to2020.nc") %>% 
  raster::stack()

dates<-seq(as.Date("2012-01-01"), as.Date("2020-12-01"), by = "month")

idx<-case_when(dates %>% lubridate::month(.) %in% c(12,1,2)~ "winter",
          dates %>% lubridate::month(.) %in% c(3,4,5)~ "spring",
          dates %>% lubridate::month(.) %in% c(6,7,8)~ "summer",
          dates %>% lubridate::month(.) %in% c(9,10,11)~ "fall")

#seasonal means
NWA_PLL_seasonal<-stackApply(NWA_PLL_pred,idx, fun = mean, na.rm = TRUE)

NWA_PLL_seasonal<-rasterToPoints(NWA_PLL_seasonal) %>% as.data.frame() %>% 
  rename("Winter - DJF"="index_winter",
         "Summer - JJA"="index_summer",
         "Spring - MAM"="index_spring",
         "Fall - SON"="index_fall") %>% 
  gather("season","suitability",-x,-y) %>% 
  mutate(season = factor(season, levels = c("Winter - DJF","Spring - MAM",
                                            "Summer - JJA","Fall - SON")))

NWA_PLL_plot<-NWA_PLL_seasonal %>% 
  ggplot()+
  geom_raster(aes(x=x,y=y,fill=suitability))+
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_sf(data = NWA_PLL_areas, color = "black", fill=NA)+ 
  facet_wrap(~season)+
  scale_fill_cmocean(name="haline",
                     limits = c(0,1),
                     labels=c("0","0.25","0.5","0.75","1"),
                     guide = guide_colorbar(barwidth = 1, 
                                            barheight = 15))+
  coord_sf(xlim = c(-97.5, -40), ylim = c(11.5, 48.5), expand = TRUE) +
  scale_y_continuous(breaks = c(10,20,30,40,50)) + 
  scale_x_continuous(breaks = c(-100,-80,-60,-40)) +
  labs(x="",y="",fill = "Fishing\nGround\nSuitability")+
  theme_bw()+theme(strip.background = element_blank(),
                   legend.spacing.y = unit(0.5, 'cm'))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))


##########
#NEP TROLL
##########

################################
#load in NWA PLL VDM predictions
################################
NEP_TROLL_pred<-here("data","spatial_predictions",
                   "NEP_TROLL","NEP_TROLL_monthly_predictions_2012to2020.nc") %>% 
  raster::stack()

dates<-seq(as.Date("2012-01-01"), as.Date("2020-12-01"), by = "month") %>% subset(format.Date(., "%m") %in% c("05", "06","07","08","09","10", "11"))

idx<-case_when(dates %>% lubridate::month(.) %in% c(5,6,7,8)~ "summer",
               dates %>% lubridate::month(.) %in% c(9,10,11)~ "fall")

#seasonal means
NEP_TROLL_seasonal<-stackApply(NEP_TROLL_pred,idx, fun = mean, na.rm = TRUE)

NEP_TROLL_seasonal<-rasterToPoints(NEP_TROLL_seasonal) %>% as.data.frame() %>% 
  rename("Summer - MJJA"="index_summer",
         "Fall - SON"="index_fall") %>% 
  gather("season","suitability",-x,-y) %>% 
  mutate(season = factor(season, levels = c("Summer - MJJA","Fall - SON"))) %>% 
  filter((x >= -135 & x <= -117) & (y <= 54.5 & y >= 32))

NEP_TROLL_plot<-NEP_TROLL_seasonal %>% 
  ggplot()+
  geom_raster(aes(x=x,y=y,fill=suitability))+
  geom_sf(data = world, color= "black", fill = "grey")+
  geom_sf(data = NEP_TROLL_areas, color = "black", fill=NA, size = 1)+
  facet_wrap(~season)+
  scale_fill_cmocean(name="haline",
                     limits = c(0,1),
                     labels=c("0","0.25","0.5","0.75","1"),
                     guide = guide_colorbar(barwidth = 2, 
                                            barheight = 15))+
  coord_sf(xlim = c(-134.5, -117), ylim = c(36.75, 53.75), expand = TRUE) +
  labs(x="",y="",fill = "Fishing\nGround\nSuitability")+
  theme_bw()+theme(strip.background = element_blank(),
                   legend.spacing.y = unit(0.5, 'cm'),
                   legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

F2_seasonal_suitability_plots<-cowplot::plot_grid(NEP_TROLL_plot, NWA_PLL_plot, 
                                                  labels = c('(A)','(B)'), 
                                                  label_size = 10,nrow=1)



ggsave(here("Plots","F3_seasonal_suitability_plots.png"),
       width = 8, height = 3.5, units = "in", dpi = 300, scale = 1.5)
ggsave(here("Plots","F3_seasonal_suitability_plots.svg"),
       width = 8, height = 3.5, units = "in", dpi = 300, scale = 1.5)

