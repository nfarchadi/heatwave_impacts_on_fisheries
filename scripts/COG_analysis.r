#center of gravity analysis 
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
library(rnaturalearth)#package that provides data of the world. Used for mapping`
world <- ne_countries(scale = "medium", returnclass = "sf")

### step 1. determining presence/absence threshold for each model ####

#load data and the model
NWA_PLL<-readRDS("C:/Users/nfarc/Desktop/NASA_FaCeT/Data/2_AIS_wENV/processed/Pres_Abs_2013to2020_NWA_USA_LL_onlyfishing_v2_1to1ratio_absenceconstrained_convexhull.rds")

brt<-readRDS("E:/VDM_results/NWA_gbm_convexhull/brt_v2.rds")

#empty=list()

#need to evaluate the model first
NWA_PLL$lunar <- lunar::lunar.illumination(NWA_PLL$date)
evall<-dismo::evaluate(p=NWA_PLL %>% filter(Pres_abs==1),a=NWA_PLL %>% filter(Pres_abs==0),model=brt,type='response')
  
NWA_PLL_thres<-threshold(evall) #this is for finding the best threshold to reclassify your predictions into presence/absence
#threshold is from the dismo package if you want to learn more

#empty[[length(empty)+1]]=a ----this is if I need to evaluate a bunch of models as it add the threshold values from above to a empty list


### step 2. reclassify predictions into presence absence, calculate habitat metrics on reclassified predictions ####

VDMharddrivepath<-"E:/VDM_results/NWA_gbm_convexhull"

predir=glue("{VDMharddrivepath}/Spatial_Predictions_v2/")

#statdir=glue("{VDMharddrivepath}/Spatial_Predictions/Binary_reclassifed") ## whre you want to write out results of the habitat metric analysis

#thresh<-NWA_PLL_thres$equal_sens_spec #this is the value that Heather used in a previous study
thresh <- 0.75 #trying what elliott did as >0.75 is essential habitat 

VDM_predictions<-list.files(predir) %>%
  grep(pattern = ".nc", value = TRUE)

cog_df<-list()

for (i in 1:length(VDM_predictions)){
  setwd(predir)
  ras_date <- VDM_predictions[i] %>% raster() 
  
  udate<-substr(VDM_predictions[i], 1, 10)
  names(ras_date)<-udate
  ras_date[values(ras_date)>=thresh]=1 # reclassify into 1s and 0s based on the threshold
  ras_date[values(ras_date)<thresh]=0
  
  ## this section is because the centroid analysis needs data to be in point format, not raster
  ras_date2=ras_date
  ras_date2[values(ras_date2)==0]=NA
  
  raspnt=rasterToPoints(ras_date2) %>% as.data.frame()
  
  # datt=data.frame(maxX=max(raspnt$x),
  #                 minX=min(raspnt$x),
  #                 maxY=max(raspnt$y),
  #                 minY=min(raspnt$y))
  
  datt=data.frame(X25=quantile(raspnt$x)[2],
                   X75=quantile(raspnt$x)[4],
                   Y25=quantile(raspnt$y)[2],
                   Y75=quantile(raspnt$y)[4])
  
  
  sat=SDMTools::ClassStat(ras_date,cellsize = 8000,bkgd=NA,latlon = TRUE) %>% filter(class==1) #calculate classStats from the SDMTools package
  grav=SDMTools::COGravity(ras_date2) %>% as.data.frame() %>% t() %>% as.data.frame() # calculate centroids
  stat2=cbind(sat,grav) %>% mutate(date=udate)
  stat3=cbind(stat2,datt)
  cog_df[[length(cog_df)+1]]=stat3
}
cog_df<-bind_rows(cog_df)

#save this
write.csv(cog_df, "E:/VDM_results/NWA_gbm_convexhull/COG_classstats_corehabitat_quantiles.csv")



##making a plot to see changes
cog_df$month<-lubridate::month(cog_df$date)
cog_df$year<-lubridate::year(cog_df$date)

cog_df_yearavg<-cog_df %>% group_by(year,month) %>% summarise(lon=mean(COGx),lat=mean(COGy))

plot1<-ggplot() +
  geom_sf(data = world, color= "black", fill = "grey" ) + #bring in world data
  #geom_sf(data = eezs, color = "blue") + #bring in EEZ data
  #geom_point(data = cog_df_yearavg, aes(x=lon, y=lat, color = as.factor(year)))+
  geom_point(data= cog_df, aes(x=COGx, y=COGy, color= as.factor(year)))+
  coord_sf(xlim = c(-100,-30), ylim = c(10,50), expand = FALSE) + #changing the extent
  ylab("Latitude") + xlab("Longitude") + labs(color = "Year") +
  theme_bw()


ggsave(plot1,
       path = "E:/VDM_results/NWA_gbm_convexhull/Plots_v2/"
       , filename = "daily_COG.png",
       width = 9, height = 6, units = 'in')


plot2<-ggplot() +
  geom_sf(data = world, color= "black", fill = "grey" ) + #bring in world data
  #geom_sf(data = eezs, color = "blue") + #bring in EEZ data
  #geom_point(data = cog_df_yearavg, aes(x=lon, y=lat, color = as.factor(year)))+
  geom_point(data= cog_df_yearavg, aes(x=lon, y=lat, color= as.factor(year)))+
  coord_sf(xlim = c(-100,-30), ylim = c(10,50), expand = FALSE) + #changing the extent
  ylab("Latitude") + xlab("Longitude") + labs(color = "Year") +
  theme_bw()


ggsave(plot2,
       path = "E:/VDM_results/NWA_gbm_convexhull/Plots_v2/"
       , filename = "monthly_COG.png",
       width = 9, height = 6, units = 'in')

cog_df_yearavg<-cog_df_yearavg %>% mutate(date = make_date(year, month))


plot3<-ggplot(data= cog_df, aes(x=as.Date(date), y=COGy, color= as.factor(month))) +
  geom_point() +
  ylab("Latitude") + xlab("Date") + labs(color = "Month") +
  theme_bw() + stat_smooth(method = "lm", aes(group=1), color = "black")

ggsave(plot3,
       path = "E:/VDM_results/NWA_gbm_convexhull/Plots_v2/"
       , filename = "daily_cog_regression.png",
       width = 9, height = 6, units = 'in')




#########################################################
#Hovmoller plots


VDMharddrivepath<-"E:/VDM_results/NWA_gbm_convexhull"

predir=glue("{VDMharddrivepath}/Spatial_Predictions_v2/")


VDM_predictions<-list.files(predir) %>%
  grep(pattern = ".nc", value = TRUE) %>% 
  grep(pattern = "", value = TRUE)

setwd(predir)

ras_stack_VDM <- VDM_predictions %>% stack()
#ras_stack_VDM<-brick(ras_stack_VDM)
idx<-format(as.Date(substr(VDM_predictions, 1,10)), format = "%Y.%m")
names(ras_stack_VDM)<-idx

VDM_month<-stackApply(ras_stack_VDM,idx, fun = mean)
VDM_month2<-VDM_month
#making the hovmoller plot
tt<-as.yearmon(unique(format(as.Date(substr(VDM_predictions, 1,10)), format = "%Y-%m")))
VDM_month2<-setZ(VDM_month2,tt)
names(VDM_month2)<-as.character(tt)

#I want to save these monthly predictions for later use
#first as a whole stack
writeRaster(VDM_month2, paste("E:/VDM_results/NWA_gbm_convexhull/Monthly_Spatial_Predictions_v2/","VDM_month_stack_2012to2020", sep=''), format='CDF', overwrite=TRUE)
#second individually

## Extract month value from a Date or yearmon object
month <- function(x)format(x, '%m')
## Compute anomaly using monthly grouping with ave  
anomaly <- function(x){
  ## Monthly means
  mm <- ave(x, month(tt), FUN = mean)
  ## Monthly standard deviation
  msd <- ave(x, month(tt), FUN = sd)
  ## anomaly
  (x - mm)/msd
}

## Use anomaly with calc
VDManom <- calc(VDM_month2, anomaly)
VDManom <- setZ(VDManom, tt)
writeRaster(VDManom, paste("E:/VDM_results/NWA_gbm_convexhull/Monthly_Spatial_Predictions_v2/","VDM_month_anom_stack_2012to2020", sep=''), format='CDF', overwrite=TRUE)


library(PNWColors)
pal <- pnw_palette("Bay",7)
bay<-rasterTheme(region = pal)
hov_plot<-hovmoller(VDManom,
          at = seq(-2.5, 2.5, .25),
          panel = panel.levelplot.raster,
          interpolate = FALSE,
          yscale.components = yscale.raster.subticks,
          #par.settings = BuRdTheme
          par.settings = bay
)

hov_plot

setwd("E:/VDM_results/NWA_gbm_convexhull/Plots_v2")
ggsave("monthly_anomalies_2012to2020.jpg", width = 7, height = 7)


#####now hovmueller plot but for SST
SSTharddrivepath<-"E:/HYCOM"

predir=glue("{SSTharddrivepath}/sst_updated/")

SST_predictions<-list.files(predir) %>%
  grep(pattern = "_sd", value = TRUE, inv=TRUE) 

setwd(predir)

# z<-raster("C:/Users/nfarc/Desktop/RCodes_Shapefiles/Static_rasters/ETOPO1/NWA_bathy_largerextent.nc")
# 
# for (i in 1:length(udates)) { 
#   #SST
#   setwd(paste(home_dir, '/water_temp', sep=''))
#   SST <- raster::raster(sst_files[grep(paste('temp_', udates[i], sep=''), sst_files)])
#   raster_extent<-matrix(raster::extent(SST))
#   if(raster_extent[1,1]==-100.04) {
#     SST
#   } else {
#     suppressWarnings(SST<-raster::rotate(SST))
#   }
#   if(raster_extent[4,1]==50.04) {
#     SST
#   } else {
#     n_ext<-extent(z)
#     SST<-raster::shift(SST,dy=-0.02)
#     SST<-extend(SST,n_ext)
#     SST<-resample(SST,z)
#   }
#   writeRaster(SST, paste("E:/HYCOM/sst_updated/hycom_temp_",udates[i], sep=''), format='CDF', overwrite=TRUE)
# }



ras_stack_SST <- SST_predictions %>% stack()
idx<-format(as.Date(substr(SST_predictions, 12,21)), format = "%Y.%m")
names(ras_stack_SST)<-idx

SST_month<-stackApply(ras_stack_SST,idx, fun = mean)
SST_month2<-SST_month
#making the hovmoller plot
tt<-as.yearmon(unique(format(as.Date(substr(SST_predictions, 12,21)), format = "%Y-%m")))
SST_month2<-setZ(SST_month2,tt)
names(SST_month2)<-as.character(tt)

#I want to save these monthly predictions for later use
#first as a whole stack
writeRaster(SST_month2, paste("E:/HYCOM/water_temp_month/","SST_month_stack_2012to2020", sep=''), format='CDF', overwrite=TRUE)
#second individually

## Extract month value from a Date or yearmon object
month <- function(x)format(x, '%m')
## Compute anomaly using monthly grouping with ave  
anomaly <- function(x){
  ## Monthly means
  mm <- ave(x, month(tt), FUN = mean)
  ## Monthly standard deviation
  msd <- ave(x, month(tt), FUN = sd)
  ## anomaly
  (x - mm)/msd
}

## Use anomaly with calc
SSTanom <- calc(SST_month2, anomaly)
SSTanom <- setZ(SSTanom, tt)
writeRaster(SSTanom, paste("E:/HYCOM/water_temp_month/","SST_month_anom_stack_2012to2020", sep=''), format='CDF', overwrite=TRUE)


library(PNWColors)
pal <- pnw_palette("Anemone",7)

hov_plot<-hovmoller(SSTanom,
                    at = seq(-2.5, 2.5, .25),
                    panel = panel.levelplot.raster,
                    interpolate = FALSE,
                    yscale.components = yscale.raster.subticks,
                    par.settings = BuRdTheme
)

hov_plot

setwd("E:/HYCOM/water_temp_month")
ggsave("monthly_anomalies_2012to2020.jpg", width = 7, height = 7)



#subtracting VDM anom with SST anom to see where there are differences
VDM_SST_anom_diff<-VDManom - SSTanom

VDM_SST_anom_diff<-setZ(VDM_SST_anom_diff,tt)

pal <- pnw_palette("Anemone",100)
winter<-rasterTheme(region = pal)
hovmoller(VDM_SST_anom_diff,
          at = seq(-3.5,3.5, 1),
          panel = panel.levelplot.raster,
          interpolate = FALSE,
          yscale.components = yscale.raster.subticks,
          par.settings = winter
)

#note: any pixel that is brown means that vessel suitability was higher where SST was lower. Any pixel that is green means that vessel suitability was lower where SST was higher. Any pixel that is white indicates similar anomalous responses, so SST and vessel suitability were both lower or both higher (we can differentiate that though)

setwd("E:/VDM_results/NWA_gbm_convexhull/Plots_v2")
ggsave("monthly_anomalies_2012to2020.jpg", width = 7, height = 7)

###----climatology----###
cog_df<-read.csv("E:/VDM_results/NWA_gbm_convexhull/COG_classstats_corehabitat.csv")

#first looking at latitude
a=cog_df %>% dplyr::select(date,COGy)
test=ts(a$COGy,start = c(2012, as.numeric(format(as.Date("2012-01-01"), "%j"))),frequency=365)
decomptest=decompose(test,type="additive")
plot(decomptest)


dat<-decomptest$trend
dat<-data.frame(Y=as.matrix(dat), date=as.Date(date_decimal(index(dat))))


ggplot(dat)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898", #red
            xmin = as.Date("2012-01-01"),
            xmax = as.Date("2012-12-31"),
            ymin = 32,
            ymax = 35)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898",#red
            xmin = as.Date("2016-01-01"),
            xmax = as.Date("2016-12-31"),
            ymin = 32,
            ymax = 35)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898",#red
            xmin = as.Date("2018-01-01"),
            xmax = as.Date("2018-12-31"),
            ymin = 32,
            ymax = 35)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898",#red
            xmin = as.Date("2020-01-01"),
            xmax = as.Date("2020-12-31"),
            ymin = 32,
            ymax = 35)+
  geom_line(aes(x=date, y=Y))+
  theme_classic()+xlab("Year")+ylab("Center of Gravity Latitude (°N)")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  labs(title=glue("NWA PLL Annual Latitude"),
       subtitle = "Red bars = Marine Heatwave Years")

#setwd("E:/VDM_results/NWA_gbm_convexhull/Plots_v2")
ggsave("E:/VDM_results/NWA_gbm_convexhull/Plots_v2/MHW_lat_timeseries_cog2.png", width = 7, height = 7)


#longitude
a=cog_df %>% dplyr::select(date,COGx)
test=ts(a$COGx,start = c(2012, as.numeric(format(as.Date("2012-01-01"), "%j"))),frequency=365)
decomptest=decompose(test,type="additive")
plot(decomptest)


dat<-decomptest$trend
dat<-data.frame(X=as.matrix(dat), date=as.Date(date_decimal(index(dat))))


ggplot(dat)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898", #red
            xmin = as.Date("2012-01-01"),
            xmax = as.Date("2012-12-01"),
            ymin = -72,
            ymax = -65)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898",#red
            xmin = as.Date("2016-01-01"),
            xmax = as.Date("2016-12-01"),
            ymin = -72,
            ymax = -65)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898",#red
            xmin = as.Date("2018-01-01"),
            xmax = as.Date("2018-12-31"),
            ymin = -72,
            ymax = -65)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898",#red
            xmin = as.Date("2020-01-01"),
            xmax = as.Date("2020-12-31"),
            ymin = -72,
            ymax = -65)+
  geom_line(aes(x=date, y=X))+
  theme_classic()+xlab("Year")+ylab("Average longitude")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  labs(title=glue("NWA PLL Annual Longitude"),
       subtitle = "Red bars = Marine Heatwave")


#ggsave("MHW_lon_timeseries_cog.png", width = 7, height = 7)
ggsave("E:/VDM_results/NWA_gbm_convexhull/Plots_v2/MHW_lon_timeseries_cog.png", width = 7, height = 7)


#annual average plots
dat<-cog_df %>% mutate(month=str_pad(month(date),2,side="left",pad="0"),year=year(date),dt=glue("{year}-01-01")) %>% 
  group_by(dt) %>% 
  summarise(mlon=mean(COGx,na.rm=T),
            mlat=mean(COGy,na.rm=T)) %>% 
  mutate(month=month(dt))



MHW_yr_cog<-ggplot(dat)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898", #red
            xmin = as.Date("2012-01-01"),
            xmax = as.Date("2012-12-01"),
            ymin = min(dat$mlon)-5,
            ymax = max(dat$mlon)+5)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898",#red
            xmin = as.Date("2016-01-01"),
            xmax = as.Date("2016-12-01"),
            ymin = min(dat$mlon)-5,
            ymax = max(dat$mlon)+5)+
  stat_smooth(aes(x=as.Date(dt),y=mlon),se=F,size=2)+
  geom_point(aes(x=as.Date(dt),y=mlon),shape=21,color="black", fill='black')+
  scale_x_date(date_breaks="1 year",date_labels = "%Y",limits = c(as.Date(min(dat$dt)),as.Date(max(dat$dt))))+
  # scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  xlab("Year")+ylab("Average longitude")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  labs(title=glue("NWA PLL Annual Longitude"),
       subtitle = "Red bars = Marine Heatwave")

setwd("E:/VDM_results/NWA_gbm_convexhull/Plots")
ggsave("MHW_yr_abg_lon_cog.png", width = 7, height = 7)

ggplot(dat)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898", #red
            xmin = as.Date("2012-01-01"),
            xmax = as.Date("2012-12-01"),
            ymin = min(dat$mlat)-5,
            ymax = max(dat$mlat)+5)+
  geom_rect(color = "black", alpha = 0.2,fill="#F79898",#red
            xmin = as.Date("2016-01-01"),
            xmax = as.Date("2016-12-01"),
            ymin = min(dat$mlat)-5,
            ymax = max(dat$mlat)+5)+
  stat_smooth(aes(x=as.Date(dt),y=mlat),se=F,size=2)+
  geom_point(aes(x=as.Date(dt),y=mlat),shape=21,color="black", fill='black')+
  scale_x_date(date_breaks="1 year",date_labels = "%Y",limits = c(as.Date(min(dat$dt)),as.Date(max(dat$dt))))+
  # scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  xlab("Year")+ylab("Average latitude")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  labs(title=glue("NWA PLL Annual Latitude"),
       subtitle = "Red bars = Marine Heatwave")

setwd("E:/VDM_results/NWA_gbm_convexhull/Plots")
ggsave("MHW_yr_avg_lat_cog.png", width = 7, height = 7)
