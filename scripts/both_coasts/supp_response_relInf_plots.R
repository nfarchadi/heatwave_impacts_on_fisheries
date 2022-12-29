#making response plots / variable contribution bar plot
library(here)
library(tidyverse)

###----response plots----####

#NWA PLL
brt_PLL <- here("data","VDM_and_val","NWA_PLL","brt_hoursabove1_VDM_eval_gbm_step.rds") %>% readRDS()


# get the matrix out of the `plot.gbm`
var<-brt_PLL$brt$var.names
response_data_PLL<-data.frame()

for (i in 1:length(var)){
response_plot_data_PLL <- gbm::plot.gbm(brt_PLL$brt,
                                 i.var = var[i],
                                 return.grid = TRUE)

response_plot_data_PLL<-response_plot_data_PLL %>% gather("variable","x",-y)

response_data_PLL<-rbind(response_data_PLL, response_plot_data_PLL)

}

response_data_PLL <-response_data_PLL %>% 
  mutate(variable = case_when(variable == "SST" ~ "SST",
                              variable == "SST_SD" ~ "SST_sd",
                              variable == "SSH" ~ "SSH",
                              variable == "SSH_SD" ~ "SSH_sd",
                              variable == "z.1" ~ "bathy",
                              variable == "z_SD" ~ "rugosity",
                              variable == "dist_port" ~ "dis_port",
                              variable == "dis_seamount" ~ "dis_seamount",
                              variable == "lunar" ~ "lunar",
                              variable == "n2" ~ "N2"),
         Fleet = "Longline")

#NEP TROLL
brt_TROLL <- here("data","VDM_and_val","NEP_TROLL","brt_hoursabove1_VDM_eval_gbm_step.rds") %>% readRDS()


# get the matrix out of the `plot.gbm`
var<-brt_TROLL$brt$var.names
response_data_TROLL<-data.frame()

for (i in 1:length(var)){
  response_plot_data_TROLL <- gbm::plot.gbm(brt_TROLL$brt,
                                          i.var = var[i],
                                          return.grid = TRUE)
  
  response_plot_data_TROLL<-response_plot_data_TROLL %>% gather("variable","x",-y)
  
  response_data_TROLL<-rbind(response_data_TROLL, response_plot_data_TROLL)
  
}

response_data_TROLL <-response_data_TROLL %>% 
  mutate(variable = case_when(variable == "sst" ~ "SST",
                              variable == "sst_sd" ~ "SST_sd",
                              variable == "ssh" ~ "SSH",
                              variable == "ssh_sd" ~ "SSH_sd",
                              variable == "bathy" ~ "bathy",
                              variable == "rugosity" ~ "rugosity",
                              variable == "dis_port" ~ "dis_port",
                              variable == "dis_seamount" ~ "dis_seamount",
                              variable == "lunar" ~ "lunar",
                              variable == "n2" ~ "N2"),
         Fleet = "Troll")


response_data_PLL_TROLL<-rbind(response_data_PLL,response_data_TROLL)

response_data_PLL_TROLL %>%
  ggplot() + 
  geom_line(aes(x=x, y=y, color = Fleet), size =1) + 
  facet_wrap(~variable, scales = "free", nrow = 5) +
  labs(x = "Variable Values",
       y = "Marginal Effect")+
  theme_bw() + 
  theme(panel.spacing = unit(.30, "lines"),
        strip.text = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(margin = margin(0,0,.05,0, "cm")))+
  scale_color_manual(values=c("orange","lightseagreen"))
  
ggsave(here("Plots","both_coasts","supp & other","supp_response_plots.png"),
       width = 7, height = 6, units = "in", dpi = 300)
ggsave(here("Plots","both_coasts","supp & other","supp_response_plots.svg"),
       width = 7, height = 6, units = "in", dpi = 300)

###----variable contribution----####
rel.inf_pll<-data.frame(Variable = brt_PLL$brt$contributions$var,
                        rel.inf=brt_PLL$brt$contributions$rel.inf) %>% 
  mutate(Variable = case_when(Variable == "SST" ~ "SST",
                              Variable == "SST_SD" ~ "SST_sd",
                              Variable == "SSH" ~ "SSH",
                              Variable == "SSH_SD" ~ "SSH_sd",
                              Variable == "z.1" ~ "bathy",
                              Variable == "z_SD" ~ "rugosity",
                              Variable == "dist_port" ~ "dis_port",
                              Variable == "dis_seamount" ~ "dis_seamount",
                              Variable == "lunar" ~ "lunar",
                              Variable == "n2" ~ "N2"),
         Fleet = "Longline")

rel.inf_troll<-data.frame(Variable = brt_TROLL$brt$contributions$var,
                        rel.inf=brt_TROLL$brt$contributions$rel.inf) %>% 
  mutate(Variable = case_when(Variable == "sst" ~ "SST",
                              Variable == "sst_sd" ~ "SST_sd",
                              Variable == "ssh" ~ "SSH",
                              Variable == "ssh_sd" ~ "SSH_sd",
                              Variable == "bathy" ~ "bathy",
                              Variable == "rugosity" ~ "rugosity",
                              Variable == "dis_port" ~ "dis_port",
                              Variable == "dis_seamount" ~ "dis_seamount",
                              Variable == "lunar" ~ "lunar",
                              Variable == "n2" ~ "N2"),
         Fleet = "Troll")

rel.inf<-rbind(rel.inf_pll,rel.inf_troll)  

rel.inf %>% 
  ggplot()+
  geom_bar(aes(x = Variable,y=rel.inf, fill = Fleet),
           stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("orange","lightseagreen")) +
  theme_bw() +
  labs(x = "Environmental Variable",
       y = "Relative Importance (%)")+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

ggsave(here("Plots","both_coasts","supp & other","supp_relative_importance.png"),
       width = 7, height = 6, units = "in", dpi = 300)
ggsave(here("Plots","both_coasts","supp & other","supp_relative_importance.svg"),
       width = 7, height = 6, units = "in", dpi = 300)
