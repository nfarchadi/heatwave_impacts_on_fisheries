#########
#run glmm
#########
all_AIS_total<-rbind(NWAPLLa_AIS_total,NEPTROLLa_AIS_total) %>% 
  filter(date >= "Jan 2013") %>% 
  na.omit() %>% 
  mutate(sequence_months = as.numeric(sequence_months),
         sequence_months = if_else(MHW == 1, 0, sequence_months))
  


#lets normalize/scale the data so we can better interpret the LMM coefficients
all_AIS_total<-all_AIS_total %>% 
  na.omit() %>% 
  mutate(prop_MHW_cells = scale(prop_MHW_cells),
         mean_SSTa = scale(mean_SSTa),
         sequence_months = scale(sequence_months)) %>% 
  as.data.frame() %>%
  mutate(mgmt_zone = fct_relevel(mgmt_zone, "NED","NEC","MAB","SAB",
                                                  "SAR","FEC","GOM","CAR",
                                                  "VN","CL","EK","MT"))


#Keeps correlation parameter
MHW_glmm<-lmerTest::lmer(percent_change ~ prop_MHW_cells + mean_SSTa +
                           sequence_months +
                           (1+prop_MHW_cells + mean_SSTa +
                              sequence_months|mgmt_zone),
                         data = all_AIS_total)

summary(MHW_glmm)
coef(MHW_glmm)#estiamte coefficients in each mgmt zone


######################################
#shift / total area in mgmt zone
######################################

####
#PLL
####

month_avgCOG<-NWAPLLa_AIS_total %>% filter(MHW == 0) %>%
  filter(date >= "Jan 2015") %>% 
  group_by(mgmt_zone,month) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG_NWA<-NWAPLLa_AIS_total %>% #filter(MHW == 1) %>% 
  filter(date >= "Jan 2015") %>% 
  dplyr::select(MHW,month,mgmt_zone,COGx,COGy,
                mean_SSTa,date,
                prop_MHW_cells) %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326") %>% 
  left_join(.,month_avgCOG,by=c("month","mgmt_zone")) %>% 
  na.omit()


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

#ks test
MHWCOG_NWA<-MHWCOG_NWA %>%
  group_by(mgmt_zone,month) %>% 
  mutate(month_mean_reldis = mean(dis_per_area, na.rm=TRUE),
         percent_change_reldis=((dis_per_area - month_mean_reldis)/month_mean_reldis)*100) %>% ungroup() 

mgmtzones<-levels(all_AIS_total$mgmt_zone)
ks<-data.frame(mgmt_zone=NA, pvalue=NA, D=NA)
counter = 1
for(i in 1:length(mgmtzones)){
  a<-MHWCOG_NWA %>% filter(mgmt_zone == mgmtzones[i])
  b<-a %>% filter(MHW == 1)
  c<-a %>% filter(MHW == 0)
  print(mgmtzones[i])
  ks_test<-ks.test(log(c$dis_per_area),log(b$dis_per_area),
                   alternative = 'g')
  pvalue<-round(ks_test$p.value,4)
  d<-round(ks_test$statistic,4)
  ks[counter,1]<-mgmtzones[i]
  ks[counter,2]<-pvalue
  ks[counter,3]<-d
  
  
  #ks[counter,1]<-paste0(mgmtzones[i]," : ","P-value = ",pvalue)
  counter = counter + 1
}

ks_pvD_NWA <- ks

MHWCOG_NWA %>%
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
             mapping=aes(x=c(8.2,7.2,6.2), y=720, label = D))+
  geom_label(ks_pvD_NWA %>% filter(mgmt_zone %in% c("NED","NEC","MAB")), 
             mapping=aes(x=c(8.2,7.2,6.2), y=550, label = pvalue))+
  geom_text(ks_pvD_NWA %>% filter(mgmt_zone %in% c("NED","NEC","MAB")), 
            mapping=aes(x=c(8.2,7.2,6.2),y=650, label = "D:"))+
  geom_text(ks_pvD_NWA %>% filter(mgmt_zone %in% c("NED","NEC","MAB")), 
            mapping=aes(x=c(8.2,7.2,6.2),y=450, label = "p-value:"))+
  #south
  geom_label(ks_pvD_NWA %>% filter(mgmt_zone %in% c("SAB","SAR","FEC",
                                                    "GOM","CAR")), 
             mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2), y=720, label = D))+
  geom_label(ks_pvD_NWA %>% filter(mgmt_zone %in% c("SAB","SAR","FEC",
                                                    "GOM","CAR")), 
             mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2), y=550, label = pvalue))+
  geom_text(ks_pvD_NWA %>% filter(mgmt_zone %in% c("SAB","SAR","FEC",
                                                   "GOM","CAR")), 
            mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2),y=650, label = "D:"))+
  geom_text(ks_pvD_NWA %>% filter(mgmt_zone %in% c("SAB","SAR","FEC",
                                                   "GOM","CAR")), 
            mapping=aes(x=c(5.2,4.2,3.2,2.2,1.2),y=450, label = "p-value:"))+
  coord_flip()

