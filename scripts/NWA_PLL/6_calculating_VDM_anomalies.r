#Calculating anomalies

library(tidyverse)
library(raster)
library(zoo)
library(glue)

####----Monthly VDM anomalies (monthly mean - climatolgical monthly mean / month SD)----####

VDMharddrivepath<-"E:/VDM_results/NWA_PLL"
predir=glue("{VDMharddrivepath}/spatial_predictions/")


VDM_predictions<-list.files(predir, full.names = TRUE) %>%
  grep(pattern = ".nc", value = TRUE)

ras_stack_VDM <- VDM_predictions %>% stack()
idx<-format(as.Date(substr(VDM_predictions, 44,53)), format = "%Y-%m")
names(ras_stack_VDM)<-idx

#monthly means
VDM_month<-stackApply(ras_stack_VDM,idx, fun = mean)

#adding in a time variable
tt<-as.yearmon(unique(format(as.Date(substr(VDM_predictions, 44,53)), format = "%Y-%m")))
VDM_month<-setZ(VDM_month,tt)
names(VDM_month)<-as.character(tt)

#save these monthly predictions for later use
writeRaster(VDM_month, paste0(VDMharddrivepath,"/monthly/","predictions/","NWA_PLL_monthly_predictions_2012to2020"), format='CDF', overwrite=TRUE)

#calculating monthly anomalies 
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
VDManom <- calc(VDM_month, anomaly)
VDManom <- setZ(VDManom, tt)
writeRaster(VDManom, paste0(VDMharddrivepath,"/monthly/","predictions/","NWA_PLL_monthly_anom_predictions_2012to2020"), format='CDF', overwrite=TRUE)


####----Monthly SST anomalies (monthly mean - climatolgical monthly mean / month SD)----###

HYCOMharddrivepath<-"E:/HYCOM"
sstdir=glue("{HYCOMharddrivepath}/sst_updated/")


sst_files<-list.files(sstdir, full.names = TRUE) %>%
  grep(pattern = ".nc", value = TRUE)

ras_stack_sst <- sst_files %>% stack()
idx<-format(as.Date(substr(sst_files, 33,42)), format = "%Y-%m")
names(ras_stack_sst)<-idx


#monthly means
sst_month<-stackApply(ras_stack_sst,idx, fun = mean)
sst_month<-mask(sst_month,VDM_month[[1]]) #take out pacific

#adding in a time variable
tt<-as.yearmon(unique(format(as.Date(substr(sst_files, 33,42)), format = "%Y-%m")))
sst_month<-setZ(sst_month,tt)
names(sst_month)<-as.character(tt)

#save these monthly predictions for later use
writeRaster(sst_month, paste0(VDMharddrivepath,"/monthly/","sst/","SST_month_stack_2012to2020"), format='CDF', overwrite=TRUE)

#calculating monthly anomalies 
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
sstanom <- calc(sst_month, anomaly)
sstanom<-mask(sstanom,VDM_month[[1]]) #take out pacific
sstanom <- setZ(sstanom, tt)
writeRaster(sstanom, paste0(VDMharddrivepath,"/monthly/","sst/","SST_month_anom_stack_2012to2020"), format='CDF', overwrite=TRUE)




###----climatology of SST monthly anomaly----###



#SST monthly anomaly

NWA_SST_anom<-stack("E:/VDM_results/NWA_PLL/monthly/sst/SST_month_anom_stack_2012to2020.nc")

NWA_SST_anom_timeseries<-cellStats(NWA_SST_anom,"mean") %>% 
  as.data.frame() %>% rownames_to_column()

colnames(NWA_SST_anom_timeseries)<-c("date","mean_sst_anom")

NWA_SST_anom_timeseries$date<-seq(as.Date("2012-01-01"), as.Date("2020-12-01"), by="month") %>% as.yearmon()


#getting the 90 percentile for each year
NWA_SST_anom_timeseries$year<-lubridate::year(NWA_SST_anom_timeseries$date)



yearly_quanitle<-tapply(NWA_SST_anom_timeseries$mean_sst_anom,
       NWA_SST_anom_timeseries$year,
       quantile, prob = c(.9)) %>% as.data.frame() %>% 
  rownames_to_column() %>% rename("year" = "rowname", "90"=".")



ggplot()+
  geom_line(NWA_SST_anom_timeseries,mapping = aes(x=date, y=mean_sst_anom))+
  geom_hline(aes(yintercept = yearly_quanitle$`90`),color = "gray69")+
  geom_hline(aes(yintercept = mean(yearly_quanitle$`90`),
                 color = "Mean 90% Quantile"),
             linetype = "dashed",
             size = 1)+
  theme_classic()+xlab("Date")+ylab("SST Anomaly")+
  labs(title="NW Atlantic SST Monthly Anomaly")
  
  


### calculating anomalies from a baseline of 1992-10-02 - 2011-12-31
SSTharddrivepath<-"E:/HYCOM_NWA/SST"

SST_rasters<-list.files(SSTharddrivepath, full.names = TRUE) %>%
  grep(pattern = ".nc", value = TRUE)

SST_rasters_baseline<-SST_rasters[1:7029] %>% stack()

SST_baseline<-calc(SST_rasters_baseline,mean,na.rm=TRUE)
