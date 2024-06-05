#Script to plot tagging data on SIOFA 3b 
#Launch Map, 01_LoadData

#Load --------------------------------------------------------------------------
T_Rels=read.csv(paste0("Output/Output_All_Releases_",Time,".csv"))
T_Recs=read.csv(paste0("Output/Output_All_Recaptures_",Time,".csv"))
T_Catch=read.csv(paste0("Output/Output_All_Catch_",Time,".csv"))
T_Links=read.csv(paste0("Output/Output_Recaptures_Linked_",Time,".csv"))
T_Biol=read.csv(paste0("Output/Output_All_Biology_",Time,".csv"))

# T_Links=T_Links%>%filter(taglink_mismatch_yn=="N")

#Load filtered links
Ls=read.csv(paste0("Output/Output_Recaptures_Linked_Filtered_",Time,".csv"),check.names = F)


#Map ---------------------------------------------------------------------------
load(file="Data/TOP_MU_map.RData")


## Tag releases  
df_rel = T_Rels %>% group_by(Season) %>% dplyr::summarise(N=dplyr::n())

TOP_rel_map = 
  TOP_MU_map + 
  geom_point(data = T_Rels, aes(x=Lon_release, y =Lat_release), size=0.5, col='blue') +
  geom_text(data= df_rel, aes(label = paste0("N:",N)), x = -Inf, y = Inf,  hjust = -0.5, vjust = 1.5) + 
  facet_wrap(~Season, ncol=2) 
  

ggsave(filename = paste0('Data/TOP_rel_map.tiff') , plot=TOP_rel_map, device='tiff', width = 12, height = 12 )


## Tag linked 
df_link = T_Links %>% group_by(Recapture_Season) %>% dplyr::summarise(N=dplyr::n()) %>% mutate(Season=Recapture_Season)

TOP_link_map = 
TOP_MU_map + 
  geom_segment(data = T_Links %>% mutate(Season=Recapture_Season), 
               aes(y=Release_Latitude,yend=Recapture_Latitude,
                   x=Release_Longitude,xend=Recapture_Longitude),size=0.5)+
  geom_point(data = T_Links %>% mutate(Season=Recapture_Season), aes(x=Release_Longitude, y =Release_Latitude), size=0.5, col='blue') +
  geom_point(data = T_Links %>% mutate(Season=Recapture_Season), aes(x=Recapture_Longitude, y =Recapture_Latitude), size=0.5, col='red') +
  geom_text(data= df_link, aes(label = paste0("N:",N)), x = -Inf, y = Inf,  hjust = -0.5, vjust = 1.5) + 
  facet_wrap(~Season, ncol=2)


ggsave(filename = paste0('Data/TOP_link_map.tiff') , plot=TOP_link_map, device='tiff', width = 12, height = 12 )



#LW relationship----------------------------------------------------------------

length_weight_data = T_Biol

mod_lwt = list()
for (m in 1:length(Mu)){
  mod_lwt[[m]] <- stats::lm(log(bsWeight_Kg) ~ log(bsLength_cm), 
                       data = length_weight_data %>% filter(MU ==Mu[m]))
}
names(mod_lwt) = Mu

length_seq= seq(20,180,1)
pred_dc = exp(predict(mod_lwt[['DC']], newdata = data.frame(bsLength_cm = (length_seq) ) ) ) 
df_lw_dc = as.data.frame(cbind(bsLength_cm = length_seq, bsWeight_Kg = pred_dc))
df_lw_dc$MU='DC'
pred_sir = exp(predict(mod_lwt[['SIR']], newdata = data.frame(bsLength_cm = (length_seq) ) ) ) 
df_lw_sir = as.data.frame(cbind(bsLength_cm = length_seq, bsWeight_Kg = pred_sir))
df_lw_sir$MU='SIR'
df_lw = rbind(df_lw_dc, df_lw_sir)

plot_lw = 
  ggplot() + 
  geom_point(data= length_weight_data, aes(x=bsLength_cm, y=bsWeight_Kg, col=MU)) +
  geom_line(data=df_lw, aes(x=bsLength_cm, y=bsWeight_Kg, color=MU)) + 
  ylab('Weight (kg)') + xlab('Length (cm)')

ggsave(filename = paste0('Data/plot_lw.tiff') , plot=plot_lw, device='tiff', width = 6, height = 4 )


# GLM exploration LW relationship 
# 
# plot_glm_lw = 
#   ggplot(data = length_weight_data,
#          aes(x = bsLength_cm, y = bsWeight_Kg, colour = bsSex))  +
#   geom_point(shape = 1) +
#   stat_smooth(method = "glm",
#               method.args = list(family = gaussian(link = 'log')),
#               formula = y ~ x,  se = FALSE) +
#   scale_color_manual(name='Sex', values=c("#E69F00", "#56B4E9")) +
#   xlab('Total length') + ylab('Weight (kg)') +
#   theme(legend.position = c(0.2, 0.8))

