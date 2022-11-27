library(DBI)
library(bigrquery)
library(tidyverse)
library(dbplyr)
library(logger)
library(data.table)
library(gargle)
library(here)

# using get_AIS to get AIS from BigQuery

# **** READ BEFORE USING ****

# In this function I am pointing to a specific billing project in MY BigQuery account (bp = 'facet-ais') as well as a service account key (bq_path = FILE NAME). Both will need to be made and changed for your project.

# For more info on using BigQuery to download GFW AIS data take a look at: 
# http://jsmayorga.com/post/getting-global-fishing-watch-from-google-bigquery-using-r/

source(here("scripts","2_VDM","functions","get_AIS.r"))

USA_NWA_PLL_2012<-get_AIS(2012, "C:/Users/nfarc/Desktop/NASA_FaCeT/NASA-FaCeT/pipelines/AIS/R/facet-ais-credentials.json", -30, -100, 50, 10, "USA", "drifting_longlines", 0)

USA_NWA_PLL_2013<-get_AIS(2013, "C:/Users/nfarc/Desktop/NASA_FaCeT/NASA-FaCeT/pipelines/AIS/R/facet-ais-credentials.json", -30, -100, 50, 10, "USA", "drifting_longlines", 0)

USA_NWA_PLL_2014<-get_AIS(2014, "C:/Users/nfarc/Desktop/NASA_FaCeT/NASA-FaCeT/pipelines/AIS/R/facet-ais-credentials.json", -30, -100, 50, 10, "USA", "drifting_longlines", 0)

USA_NWA_PLL_2015<-get_AIS(2015, "C:/Users/nfarc/Desktop/NASA_FaCeT/NASA-FaCeT/pipelines/AIS/R/facet-ais-credentials.json", -30, -100, 50, 10, "USA", "drifting_longlines", 0)

USA_NWA_PLL_2016<-get_AIS(2016, "C:/Users/nfarc/Desktop/NASA_FaCeT/NASA-FaCeT/pipelines/AIS/R/facet-ais-credentials.json", -30, -100, 50, 10, "USA", "drifting_longlines", 0)

USA_NWA_PLL_2017<-get_AIS(2017, "C:/Users/nfarc/Desktop/NASA_FaCeT/NASA-FaCeT/pipelines/AIS/R/facet-ais-credentials.json", -30, -100, 50, 10, "USA", "drifting_longlines", 0)

USA_NWA_PLL_2018<-get_AIS(2018, "C:/Users/nfarc/Desktop/NASA_FaCeT/NASA-FaCeT/pipelines/AIS/R/facet-ais-credentials.json", -30, -100, 50, 10, "USA", "drifting_longlines", 0)

USA_NWA_PLL_2019<-get_AIS(2019, "C:/Users/nfarc/Desktop/NASA_FaCeT/NASA-FaCeT/pipelines/AIS/R/facet-ais-credentials.json", -30, -100, 50, 10, "USA", "drifting_longlines", 0)

USA_NWA_PLL_2020<-get_AIS(2020, "C:/Users/nfarc/Desktop/NASA_FaCeT/NASA-FaCeT/pipelines/AIS/R/facet-ais-credentials.json", -30, -100, 50, 10, "USA", "drifting_longlines", 0)



USA_NWA_PLL<-rbind(USA_NWA_PLL_2012,USA_NWA_PLL_2013,USA_NWA_PLL_2014,
                   USA_NWA_PLL_2015,USA_NWA_PLL_2016,USA_NWA_PLL_2017,
                   USA_NWA_PLL_2018,USA_NWA_PLL_2019,USA_NWA_PLL_2020)

colnames(USA_NWA_PLL)<-c("date","lat","lon","fishing_hours")

saveRDS(USA_NWA_PLL, here("data","AIS","NWA_PLL","AIS_USA_NWA_PLL_2013_2020.rds"))


#GFW AIS data exploration
library(tidyverse)
library(sf) #for mapping
library(rnaturalearth)#package that provides data of the world. Used for mapping`
library(dplyr) # for the pipe operator (%>%)
library(lubridate)
library(rgdal)
library(rgeos)
library(raster)
library(rasterVis)
library(maps)

#Constructing the map
#retreiving that data for the continents 
world <- ne_countries(scale = "medium", returnclass = "sf")


## general plot setup
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF",
                                 "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


bzz <- c(1,10,100,1000,10000)



NWA_PLL_plot<-USA_NWA_PLL %>% filter(fishing_hours >= 2) %>%   ggplot() +
  #geom_sf(data = world, color= "black", fill = "grey" ) + #bring in world data
  geom_path(data = map_data("world"),
            aes(x=long, y=lat, group = group)) +
  coord_fixed(xlim = c(-100,-30), ylim = c(10,50)) + #changing the extent
  ylab("Latitude") + xlab("Longitude") + 
  geom_bin2d(aes(x=lon, y=lat), binwidth=.5) +
  #geom_rect(mapping=aes(xmin=-100, xmax=-30, ymin=10, ymax=50), color="red", size = 1, alpha = 0) + 
  scale_fill_gradientn(" Frequency of\n occurence", trans='log', 
                       colours = jet.colors(100), 
                       breaks = bzz, #labels = format(bzz), 
                       guide = guide_colorbar(barwidth = 1, 
                                              barheight = 20)) + 
  theme_bw(base_size = 10) + theme(panel.grid=element_blank())+
  theme(axis.text=element_text(size=20))+
  theme(legend.title=element_text(size=20))+
  theme(legend.text=element_text(size=20))+
  #theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"))+
  theme(strip.text = element_text(size = 20))+
  theme(axis.title = element_text(size = 20))



NWA_PLL_plot




