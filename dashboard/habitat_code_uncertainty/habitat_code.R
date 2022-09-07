
## the beginnings of a habitat suitability plot(s) for the uncertainty page
## will need some minor modifications to look the way we want
## but all the pieces should be here

## the .csv is here: https://drive.google.com/file/d/1VIkhH9jwIFu_bp-SBch3VwOCirAtH5Ld/view?usp=sharing
all <-here("dashboard","habitat_code_uncertainty","hms_cc_update1.csv") %>% data.table::fread()
all$year <- format(as.Date(all$date, format="%m/%d/%Y"),"%Y")
names(all) <- tolower(names(all))
#head(aggregate(weight ~ Time + Diet, data = data, mean))
habitat_mean<- stats::aggregate(mean_suit ~ year + gcm + species, data=all, FUN = function(x){mean(x,na.rm=T)})
head(habitat_mean)
habitat_mean_mean <- aggregate(mean_suit ~ year + species, data=all, FUN = function(x){mean(x,na.rm=T)})
habitat_mean_mean$gcm <- "mean"
habitat_T=rbind(habitat_mean, habitat_mean_mean)


habitat_T_mean<-habitat_T %>% filter(species %in% c("smsh","swor")) %>% 
  mutate(species = case_when(species == "smsh"~"Shortfin Mako Shark",
                             species == "swor"~"Swordfish")) %>% 
  mutate(year = as.integer(year),
         mean_suit = as.numeric(mean_suit))




ggplot()+
  geom_smooth(data = habitat_T_mean %>% filter(gcm != "mean"), 
              aes(x=year, y=mean_suit, fill=gcm, col=gcm),
              method="loess",se=TRUE, alpha = 0.2)+
  ggnewscale:::new_scale_color()+
  geom_smooth(data = habitat_T_mean %>% filter(gcm == "mean"),
              aes(x=year, y=mean_suit, col=gcm),
              linetype = "dashed",method="loess", se=FALSE, 
              size=2)+
  scale_color_manual(values = c("black"))+
  facet_wrap(~species, scales = "free_y")+
  theme_bw()+
  scale_y_continuous(breaks = c(0.14,0.226,0.6935,0.766),
                      labels = c("Low","High","Low", "High")) +
  theme(legend.title = element_blank(),
        #panel.border = element_rect(colour = "black", fill=NA, size=1),
    axis.text.x =element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.y = element_text(size=20),
    #axis.title.x = element_text(size=10),
    title = element_text(size=20),
    strip.text = element_text(size=15),
    strip.background = element_blank())+
  labs(y="Habitat Suitability Index", x="") +   theme(legend.position="bottom")


#ggsave("Habitat_suit.png", width = 20, height = 20, units = "cm")



#===================
## OLD / DIDNT USE
#===================










#HMS PROJECTION PROJECT-FACET 01/26/2022
#Analysis to plot:
#1.anomaly centroid by species (month, year, 5 year) and ESM (together and one by one)
#2.change of total habitat (% losses and gain)
#3.Direction and intensity of shift (rose plots by ESM)
#4.plot direction (N & S) for species
#Code for calculations created by H.W and modified by N.L.O

rm(list=ls())
library(tidyverse)
library(zoo)
library(roll)
library(ncdf4)
library(dplyr)
library(ggplot2)
library(magrittr)
library(dplyr)
library(ggplot2)
library(glue)
library(dplyr)
setwd("C:\\Users\\nereo\\OneDrive\\Escritorio\\Results_HMS\\FINAL_RESULTS\\cam\\")

############
#plot by species smoothed by month, year and 5 years and sd from 5 years
##########
file_list <- list.files()
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=TRUE, sep=",")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.table(file, header=TRUE, sep=",")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}
head(dataset)
all=dataset
dim(all)#
all$ESM=all$gcm
all$Species=all$species
str(all)
#setwd("C:\\Users\\nereo\\OneDrive\\Escritorio\\Results_HMS\\FINAL_RESULTS\\swor_clim\\")
names(all$Species)

######
#using only threshold for each ESM
###############
all=all %>% filter(threshold_type=="GFDL" | threshold_type=="HAD" |threshold_type=="IPSL")
head(all)


head(all)


#########
# CHANGE OF HABITAT SUITABILITY BY SPECIES 
#####
all=rbind(all_blsh_obs, all_blsh_trk, all_casl, all_ctsh, all_hbwh, all_lbst, 
          all_smsh,all_swor)

head(all)
all=na.omit(all)


