#NWAPLL_MHW_total from 11_COG_HabitatChange_analysis script

near_avgCOG<-NWAPLL_MHW_total %>% filter(MHW == 0) %>%
  group_by(mgmt_zone) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop")

MHWCOG<-NWAPLL_MHW_total %>% filter(MHW == 1) %>% 
  group_by(event_index, mgmt_zone) %>% 
  summarise(COGx=mean(COGx, na.rm=TRUE),
            COGy=mean(COGy, na.rm=TRUE),
            prop_MHW_area=mean(prop_MHW_area, na.rm=TRUE),
            mean_SSTa= weighted.mean(mean_SSTa,n.cell_MHW, na.rm = TRUE),
                                     .groups = "drop") %>% 
  inner_join(near_avgCOG, by = "mgmt_zone")




p<-MHWCOG %>%
  # mutate(MHW = as.factor(MHW)) %>%
   filter(mgmt_zone == "NED") %>%
  # filter(prop_MHW_area <= 0.5) %>%
  ggplot() +
  geom_sf(data = NWA_PLL_zones, color = "goldenrod3", fill=NA)+  #bring in EEZ data
  geom_sf(data = world, color= "black", fill = "grey") +
  geom_point(aes(COGx,COGy, fill = mean_SSTa, size = prop_MHW_area), alpha = 0.7,shape=21, color="black")+
  geom_segment(aes(x=COGx_NA,COGy_NA,xend=COGx,yend=COGy, group = mgmt_zone), color = "black",
            size = .75,
            arrow = arrow(length = unit(.2, "in")),
            show.legend = FALSE) +
  scale_size(range = c(1, 8), name=expression("Proportion\nof MHW"(km^2)))+
  coord_sf(xlim = c(-60, -40), ylim = c(40, 50), expand = TRUE) +
  ylab("Latitude") + xlab("Longitude") + theme_bw()+
  scale_fill_viridis(discrete=FALSE, option="inferno", 
                     name=stringr::str_wrap("SSTa\nMagnitude (°C)", width = 13))

library(gganimate)

anim = p + 
  transition_reveal(along = event_index)+
  ease_aes("linear")

animate(anim, nframes = 17, fps = 1)
anim_save(here("plots","updated_PLLzones","NED_MHW_gif.gif"))



##############################################################
#3D plot
##############################################################
sf_use_s2(FALSE)

#need to find the distance from MHW COG to near-avg COG
near_avgCOG<-NWAPLL_MHW_total %>% filter(MHW == 0) %>%
  group_by(mgmt_zone) %>% 
  summarise(COGx_NA=mean(COGx, na.rm=TRUE),
            COGy_NA=mean(COGy, na.rm=TRUE),
            .groups = "drop") %>% 
  st_as_sf(coords=c("COGx_NA","COGy_NA"), crs = "EPSG:4326")

MHWCOG<-NWAPLL_MHW_total %>% filter(MHW == 1) %>% 
  dplyr::select(event_index,mgmt_zone,COGx,COGy,prop_MHW_area,mean_SSTa) %>% 
  # group_by(event_index, mgmt_zone) %>% 
  # summarise(COGx=mean(COGx, na.rm=TRUE),
  #           COGy=mean(COGy, na.rm=TRUE),
  #           prop_MHW_area=mean(prop_MHW_area, na.rm=TRUE),
  #           mean_SSTa= weighted.mean(mean_SSTa,n.cell_MHW, na.rm = TRUE),
  #           .groups = "drop") %>% 
  st_as_sf(coords=c("COGx","COGy"),crs = "EPSG:4326")


MHWCOG$distance <- map_dbl(1:nrow(MHWCOG), function(x){
  x <- MHWCOG[x,]
  y <- near_avgCOG[near_avgCOG$mgmt_zone == x$mgmt_zone,]
  st_distance(x, y)
})

MHWCOG<-MHWCOG %>% mutate(distance = distance * 0.001,
                          lon = sf::st_coordinates(.)[,1],
                          lat = sf::st_coordinates(.)[,2])


library(akima)
library(plotly)
library(cmocean)
library("plot3D")

MHWCOG_mgmtzones<-split(MHWCOG, MHWCOG$mgmt_zone)
s<- lapply(MHWCOG_mgmtzones, function(x) interp(x=x$mean_SSTa,
                                                y=x$prop_MHW_area,
                                                z=x$distance))

#indiviual plots
fig1<-plot_ly(x = s$NED$x,y = s$NED$y,z = s$NED$z, scene = 'scene1') %>%
  add_surface(showscale=FALSE)

fig2<-plot_ly(x = s$NEC$x,y = s$NEC$y,z = s$NEC$z, scene = 'scene2') %>%
  add_surface(showscale=FALSE)

fig<-subplot(fig1,fig2)
fig
#s<-with(MHWCOG, interp(mean_SSTa,prop_MHW_area,distance))


names(s)<-c("mean_SSTa","prop_MHW","distance")
plot_ly(x = s$prop_MHW, y=s$mean_SSTa, z=s$distance) %>% add_surface() %>% 
  layout() 

MHWCOG %>% ggplot(aes(x=mean_SSTa,y=prop_MHW_area))+
  geom_point(aes(color=distance))+
  labs(x="Intensity of MHW",y="Size of MHW",color='Distance from\nnear-average\nposition')+ 
  facet_wrap(~mgmt_zone, scales = "free")+
  scale_color_cmocean(name = "deep")+
  theme_bw()


scatter3D(MHWCOG$prop_MHW_area,MHWCOG$mean_SSTa,MHWCOG$distance, bty = "b2",  
          type = "h",ticktype = "detailed", pch = 20,cex=2, phi = 40, theta = 50,
          xlab = "MHW Size",
          ylab ="MHW Intensity", zlab = "Displacement")




fig <- plot_ly(MHWCOG, x = ~mean_SSTa, y = ~prop_MHW_area, z = ~distance,
               color = ~distance, colors = cmocean("deep")(100), name = "km")
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'MHW Intensity'),
                                   yaxis = list(title = 'MHW Size'),
                                   zaxis = list(title = 'Displacement')))
fig




kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
fig <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()

fig
