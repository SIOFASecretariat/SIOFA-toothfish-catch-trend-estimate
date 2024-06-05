#Script to explore catch and effort data on SIOFA 3b 
#Launch Map, 01_LoadData

#Load Data###################
#Catch 
Catch= read.csv( "Output/Output_Catch.csv")
 
Catch$Catch =  rowSums(cbind(Catch$Catch_Kg , Catch$Weigth),na.rm=T) 

Catch$DistLL = 
  sapply(1:nrow(Catch),function(i)
    distGeo(as.data.frame(Catch)[i,c('fishopSetStartLongitude','fishopSetStartLatitude')],
            as.data.frame(Catch)[i,c('fishopSetEndLongitude','fishopSetEndLatitude')]))

Catch = Catch %>% mutate(CPUE= Catch/HooksSet, CPUE_line =Catch/(DistLL*10^-3) ) 

Catch = Catch %>% 
  mutate(Depth = rowMeans(.[, c("StartDepth","EndDepth")])) %>% 
  mutate(fishopSetEndDate = as.POSIXct(fishopSetEndDate), 
         fishopSetStartDate = as.POSIXct(fishopSetStartDate),
         fishopHaulStartDate = as.POSIXct(fishopHaulStartDate), 
         fishopHaulEndDate = as.POSIXct(fishopHaulEndDate)) %>%
  mutate(Latitude = rowMeans(.[, c("fishopSetStartLatitude","fishopSetEndLatitude")])) %>% 
  mutate(Longitude = rowMeans(.[, c("fishopSetStartLongitude","fishopSetEndLongitude")])) 

Catch = Catch %>% rowwise() %>%
  dplyr::mutate(minDepth = min(unlist(c(StartDepth,EndDepth)))) %>%
  dplyr::mutate(maxDepth = max(unlist(c(StartDepth,EndDepth))))

Catch_an = Catch %>% 
  group_by(Season,MU) %>% dplyr::summarise(CPUE=median(CPUE), CATCH=sum(Catch), 
                                           EFFORT = sum(HooksSet))
Catch_an_vessel = Catch %>% group_by(Season,vesselCode,MU) %>% 
  dplyr::summarise(CPUE=median(CPUE), CATCH=sum(Catch_Kg), EFFORT = sum(HooksSet))

# Biology 
Biology= read.csv("Output/Output_LW.csv")

n.dfBiom <- Biology %>% group_by(Season,MU)%>%
  dplyr::summarise(n=dplyr::n())

df_LFD = dplyr::left_join(Biology, n.dfBiom, by= c('Season','MU')) 

df_LFD = df_LFD %>% 
  mutate(bsLength_cm.bin=as.factor(cut_width(bsLength_cm, width = 10))) %>%
  group_by(Season, MU, bsSex, bsLength_cm.bin) %>%
  dplyr::summarise(freq=dplyr::n()/mean(n))

df_LFD$lbin = cut_borders(df_LFD$bsLength_cm.bin)$start
df_LFD$ubin = cut_borders(df_LFD$bsLength_cm.bin)$end

#Bathymetry
B=rast("C:/Users/jules/Dropbox/PostDoc_MNHN/RworkingDirectory/Rproject_Lobster/Data/Gebco_Bathymetry/gebco_2022_n-25.5762_s-60.0293_w37.2656_e104.2383.tif")

#Polygons MU
# DelCano=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="DelCano", quiet = TRUE)
# SIR=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="SIR", quiet = TRUE)
PolysMU= rbind(DelCano,SIR)#rbind(PolysLL,RefAreas)
PolysLL=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="PolysLL", quiet = TRUE)
PolysMU=PolysLL%>% dplyr::filter(name%in%c('DC','SIR'))

#CPUE --------------------------------------------------------------------------

#cpue per vessel
cpue_boxplot_vessel = 
  ggplot( as.data.frame(Catch)%>% mutate(AN=factor(Season), vesselCode=factor(vesselCode)) %>% droplevels(),
         aes(y=CPUE*1000,x=AN, colour=vesselCode)) + geom_boxplot(outlier.shape = NA) +
  facet_grid(~MU) + 
  ylim(c(0, 650)) + 
  ylab('CPUE (kg/1000 hooks)') + xlab('')+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))


ggsave(filename = paste0('Data/cpue_boxplot_vessel.tiff') , plot=cpue_boxplot_vessel, device='tiff', width = 6, height = 4 )

#cpue 
cpue_boxplot = 
  ggplot( as.data.frame(Catch)%>% mutate(AN=factor(Season), vesselCode=factor(vesselCode)) %>% droplevels(),
          aes(y=CPUE*1000,x=AN)) + geom_boxplot(outlier.shape = NA, fill='grey') +
  facet_grid(~MU) + 
  ylim(c(0, 650)) + 
  ylab('CPUE (kg/1000 hooks)') + xlab('')+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))


ggsave(filename = paste0('Data/cpue_boxplot.tiff') , plot=cpue_boxplot, device='tiff', width = 6, height = 4 )

#km/line
cpue_boxplot_km = 
  ggplot( as.data.frame(Catch)%>% mutate(AN=factor(Season), vesselCode=factor(vesselCode)) %>% droplevels(),
          aes(y=CPUE_line,x=AN)) + geom_boxplot(outlier.shape = NA, fill='grey') +
  facet_grid(~MU) + 
  
  ylab('CPUE (kg/km line)') + xlab('')+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))


ggsave(filename = paste0('Data/cpue_boxplot_km.tiff') , plot=cpue_boxplot_km, device='tiff', width = 6, height = 4 )


#Catch -------------------------------------------------------------------------
catch_barplot = 
  ggplot( Catch_an,
          aes(y=CATCH*10^(-3),x=Season)) + geom_bar(stat='identity') +
  facet_grid(~MU) + 
  ylab('Catch (t)') + xlab('')+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))


ggsave(filename = paste0('Data/catch_barplot.tiff') , plot=catch_barplot, device='tiff', width = 6, height = 4 )

#catch per vessels 
catch_vessel_barplot = 
  ggplot( Catch_an_vessel %>% mutate(vesselCode=factor(vesselCode)),
          aes(y=CATCH*10^(-3),x=Season, fill=vesselCode)) + 
  geom_bar(stat='identity',
          position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_grid(~MU) + 
  ylab('Catch (t)') + xlab('')+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))

ggsave(filename = paste0('Data/catch_vessel_barplot.tiff') , plot=catch_vessel_barplot, device='tiff', width = 6, height = 4 )

#Effort ------------------------------------------------------------------------
effort_barplot = 
  ggplot( Catch_an,
          aes(y=EFFORT,x=Season)) + geom_bar(stat='identity') +
  facet_grid(~MU) + 
  ylab('Effort (Nb. hooks)') + xlab('')+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))


ggsave(filename = paste0('Data/effort_barplot.tiff') , plot=effort_barplot, device='tiff', width = 6, height = 4 )

#effort per vessels 
effort_vessel_barplot = 
  ggplot( Catch_an_vessel %>% mutate(vesselCode=factor(vesselCode)),
          aes(y=EFFORT,x=Season, fill=vesselCode)) + 
  geom_bar(stat='identity',
           position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_grid(~MU) + 
  ylab('Effort (Nb. hooks)') + xlab('')+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))

ggsave(filename = paste0('Data/effort_vessel_barplot.tiff') , plot=effort_vessel_barplot, device='tiff', width =6, height = 4 )

#LFD ---------------------------------------------------------------------------

LFD_plot <-  ggplot(df_LFD,
                             aes(x = lbin, y = freq,  fill=bsSex)) +
  geom_bar(stat = "identity",  position = "dodge") +
  # scale_x_continuous(breaks=seq(4, 120, 1)) + 
  # scale_y_continuous(breaks=seq(-0.05,0.05, 0.02), labels=abs(seq(-0.05,0.05, 0.02))) + 

  facet_wrap( . ~ MU , ncol=2) +
  ylab("Frequency") + xlab('Length (cm)') +
  theme(axis.text.x = element_text(angle=60, vjust=0.5, size=12),
        strip.text.x = element_text(size = 12))

ggsave(filename = paste0('Data/LFD_plot.tiff') , plot=LFD_plot, device='tiff', width =6, height = 4 )

# Map --------------------------------------------------------------------------

load(file="Data/TOP_MU_map.RData")

## Effort  

TOP_effort_map = 
  TOP_MU_map + 
  geom_segment(aes(x=fishopSetStartLongitude,xend=fishopSetEndLongitude,
                   y=fishopSetStartLatitude,yend=fishopSetEndLatitude),size=0.1)+
  geom_point(aes(x=fishopSetEndLongitude, y =fishopSetEndLatitude), size=0.1, col='red') +
  geom_point(aes(x=fishopSetStartLongitude, y =fishopSetStartLatitude), size=0.1, col='blue') +
  
  facet_wrap(~Season, ncol=2)

ggsave(filename = paste0('Data/TOP_effort_map.tiff') , plot=TOP_effort_map, device='tiff', width = 12, height = 12 )


## Catch 

TOP_catch_map = 
  TOP_MU_map + 
  geom_point(aes(x=Longitude, y =Latitude, col=MU, size=Catch_Kg*10^-3), shape=1) +
  scale_size(name   = "Catch (t)",
             breaks = round(quantile(Catch$Catch_Kg*10^-3, probs=c(0.1,0.5,0.91)),2),
             labels = round(quantile(Catch$Catch_Kg*10^-3, probs=c(0.1,0.5,0.99)),2)) + 
  facet_wrap(~Season, ncol=2) #+ 
  #guides(size=guide_legend(title=""))

ggsave(filename = paste0('Data/TOP_catch_map.tiff') , plot=TOP_catch_map, device='tiff', width = 12, height = 12 )

#Catch depth frequency ---------------------------------------

# quantiles depth 
quantiles_depth_dc= quantile(Catch %>% filter(Depth<2500, MU=='DC') %>% pull(Depth), probs=c(0.025, 0.975))
quantiles_depth_sir= quantile(Catch %>% filter(Depth<2500, MU=='SIR') %>% pull(Depth), probs=c(0.025, 0.975))
depth_range_1 = c(600, 1800); depth_range_2 = c(600, 2000);

quantile_depth_range = c(round(min(c(quantiles_depth_dc[1], quantiles_depth_sir[1])),0) , 
                         round(max(c(quantiles_depth_dc[2], quantiles_depth_sir[2])),0) )

minDepth = c(depth_range_1[1],depth_range_2[1], round(min(c(quantiles_depth_dc[1], quantiles_depth_sir[1])),0) )
maxDepth = c(depth_range_1[2],depth_range_2[2], round(max(c(quantiles_depth_dc[2], quantiles_depth_sir[2])),0) )

#Loop over polygons 
BatRaster_pol=data.frame(Poly=character(), Area=numeric(), min_depth=numeric(), max_depth=numeric(), cell_depth=numeric()) #Prepare empty output
RawAr=data.frame(Poly=character(), Area=numeric(), min_depth=numeric(), max_depth=numeric()) #Prepare empty output

for (i in 1:length(PolysLL$name)){
for (j in 1:length(minDepth)){
#Take one polygon
pol=PolysLL[i,]
#Get its name
pname=PolysLL$name[i]
#Take bathymetry data that matches the extent of the polygon
Btmp=crop(B,ext(pol))
#Turn GEBCO cells that are not inside the polygon into NAs
Btmp=terra::mask(Btmp,pol)
#Turn cells outside the fishable depth into NAs
Btmp.area = classify(Btmp, cbind(-100000, -maxDepth[j], NA), right=TRUE)
Btmp.area = classify(Btmp.area, cbind(-minDepth[j], 100000, NA), right=FALSE)
#Compute the area covered by cells that are not NA
Ar=round(expanse(Btmp.area, unit="km"),2)
#Store result
BatRaster_pol=rbind(BatRaster_pol,data.frame(Poly=pname,Layer = j, Area=Ar$area,min_depth= minDepth[j], max_depth=maxDepth[j],
                                             cell_depth = as.data.frame(Btmp)[,1] ))
RawAr=rbind(RawAr,data.frame(Poly=pname,Layer = j, Area=Ar$area, min_depth= minDepth[j], max_depth=maxDepth[j]))

}
}

# plot 
df_op = Catch %>% filter(Depth<=2500) %>% group_by(MU) %>% 
  dplyr::summarise(N=dplyr::n())
bin=20

#DC
hist_depth_dc = ggplot() + 
  geom_histogram(data = Catch %>% filter(Depth<=2500, MU=='DC') ,
                 aes(x=Depth), binwidth = bin)
df_hist_depth_dc <- layer_data(hist_depth_dc)

hist_gebco_dc = ggplot() + 
  geom_histogram(data = BatRaster_pol %>% filter(cell_depth>= -2500, Poly=='DC', Layer==1) ,
                 aes(x=cell_depth), binwidth = bin)
df_hist_gebco_dc <- layer_data(hist_gebco_dc)

op_depth_barplot_dc = 
  ggplot() + 
  geom_histogram(data = Catch %>% filter(Depth<=2500, MU=='DC') ,
                 aes(x=Depth), binwidth = bin) +
  geom_freqpoly(data = BatRaster_pol %>% mutate(MU=Poly )%>% filter(cell_depth>= -2500, Layer==1, MU=='DC') ,
                 aes(x=-cell_depth, y = (..count.. )/(max(df_hist_gebco_dc$count)/max(df_hist_depth_dc$count))),
                binwidth = bin, col='green') +
  geom_vline(xintercept = quantiles_depth_dc, col='red') +
  geom_vline(xintercept = depth_range_2, col='lightblue') +
  geom_vline(xintercept = depth_range_1, col='blue') +

  geom_text(data= df_op%>% filter(MU=='DC'), aes(label = paste0("N:",N)), x = -Inf, y = Inf,  hjust = -0.5, vjust = 1.5)+
  geom_text(data= BatRaster_pol %>% mutate(MU=Poly )%>% filter(Layer==1, MU=='DC') %>% filter(row_number()==1), 
            aes(label = paste0("Area:",round(Area,0),"km²")), x = -Inf, y = Inf,  hjust = -0.25, vjust = 4, col='blue') +
  geom_text(data= BatRaster_pol %>% mutate(MU=Poly )%>% filter(Layer==2, MU=='DC') %>% filter(row_number()==1), 
            aes(label = paste0("Area:",round(Area,0),"km²")), x = -Inf, y = Inf,  hjust = -0.25, vjust = 6, col='lightblue') +
  geom_text(data= BatRaster_pol %>% mutate(MU=Poly )%>% filter(Layer==3, MU=='DC') %>% filter(row_number()==1), 
            aes(label = paste0("Area:",round(Area,0),"km²")), x = -Inf, y = Inf,  hjust = -0.25, vjust = 8, col='red') +
 # facet_wrap(~MU,scales = 'free_y') + 
  xlim(0,2500) +
  ylab('Frequency') + xlab('')+ ggtitle('DC')+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))


ggsave(filename = paste0('Data/op_depth_barplot_dc.tiff') , plot=op_depth_barplot_dc, device='tiff', width = 4, height = 4 )


#SIR
hist_depth_sir = ggplot() + 
  geom_histogram(data = Catch %>% filter(Depth<=2500, MU=='SIR') ,
                 aes(x=Depth), binwidth = bin)
df_hist_depth_sir <- layer_data(hist_depth_sir)

hist_gebco_sir = ggplot() + 
  geom_histogram(data = BatRaster_pol %>% filter(cell_depth>=-2500, Poly=='SIR', Layer==1) ,
                 aes(x=cell_depth), binwidth = bin)
df_hist_gebco_sir <- layer_data(hist_gebco_sir)

op_depth_barplot_sir = 
  ggplot() + 
  geom_histogram(data = Catch %>% filter(Depth<=2500, MU=='SIR') ,
                 aes(x=Depth), binwidth = bin) +
  geom_freqpoly(data = BatRaster_pol %>% mutate(MU=Poly )%>% filter(cell_depth>= -2500, Layer==1, MU=='SIR') ,
                aes(x=-cell_depth, y = (..count.. )/(max(df_hist_gebco_sir$count)/max(df_hist_depth_sir$count))),
                binwidth = bin, col='green') +
  geom_vline(xintercept = quantiles_depth_sir, col='red') +
  geom_vline(xintercept = depth_range_2, col='lightblue') +
  geom_vline(xintercept = depth_range_1, col='blue') +
  
  geom_text(data= df_op%>% filter(MU=='SIR'), aes(label = paste0("N:",N)), x = -Inf, y = Inf,  hjust = -0.5, vjust = 1.5)+
  geom_text(data= BatRaster_pol %>% mutate(MU=Poly )%>% filter(Layer==1, MU=='SIR') %>% filter(row_number()==1), 
            aes(label = paste0("Area:",round(Area,0),"km²")), x = -Inf, y = Inf,  hjust = -0.25, vjust = 4, col='blue') +
  geom_text(data= BatRaster_pol %>% mutate(MU=Poly )%>% filter(Layer==2, MU=='SIR') %>% filter(row_number()==1), 
            aes(label = paste0("Area:",round(Area,0),"km²")), x = -Inf, y = Inf,  hjust = -0.25, vjust = 6, col='lightblue') +
  geom_text(data= BatRaster_pol %>% mutate(MU=Poly )%>% filter(Layer==3, MU=='SIR') %>% filter(row_number()==1), 
            aes(label = paste0("Area:",round(Area,0),"km²")), x = -Inf, y = Inf,  hjust = -0.25, vjust = 8, col='red') +
  # facet_wrap(~MU,scales = 'free_y') + 
  ylab('Frequency') + xlab('')+ ggtitle('SIR')+
  xlim(0,2500) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))


ggsave(filename = paste0('Data/op_depth_barplot_sir.tiff') , plot=op_depth_barplot_sir, device='tiff', width = 4, height = 4 )



#HIMI
hist_gebco_himi = ggplot() + 
  geom_histogram(data = BatRaster_pol %>% filter(cell_depth>=-2500, cell_depth<=0, Poly=='HIMI', Layer==1) ,
                 aes(x=cell_depth), binwidth = bin)
df_hist_gebco_himi <- layer_data(hist_gebco_himi)

op_depth_barplot_himi = 
  ggplot() + 
  geom_freqpoly(data = BatRaster_pol %>% mutate(MU=Poly )%>% filter(cell_depth>= -2500,  cell_depth<=0, Layer==1, MU=='HIMI') ,
                aes(x=-cell_depth, y = (..count.. )/(max((..count.. ))) ),
                binwidth = bin, col='green') +
  geom_vline(xintercept = quantiles_depth_sir, col='red') +
  geom_vline(xintercept = depth_range_2, col='lightblue') +
  geom_vline(xintercept = depth_range_1, col='blue') +
  
  #geom_text(data= df_op%>% filter(MU=='SIR'), aes(label = paste0("N:",N)), x = -Inf, y = Inf,  hjust = -0.5, vjust = 1.5)+
  geom_text(data= BatRaster_pol %>% mutate(MU=Poly )%>% filter(Layer==1, MU=='HIMI') %>% filter(row_number()==1), 
            aes(label = paste0("Area:",round(Area,0),"km²")), x = -Inf, y = Inf,  hjust = -0.25, vjust = 4, col='blue') +
  geom_text(data= BatRaster_pol %>% mutate(MU=Poly )%>% filter(Layer==2, MU=='HIMI') %>% filter(row_number()==1), 
            aes(label = paste0("Area:",round(Area,0),"km²")), x = -Inf, y = Inf,  hjust = -0.25, vjust = 6, col='lightblue') +
  geom_text(data= BatRaster_pol %>% mutate(MU=Poly )%>% filter(Layer==3, MU=='HIMI') %>% filter(row_number()==1), 
            aes(label = paste0("Area:",round(Area,0),"km²")), x = -Inf, y = Inf,  hjust = -0.25, vjust = 8, col='red') +
  # facet_wrap(~MU,scales = 'free_y') + 
  xlim(0,2500) +
  
  ylab('Frequency') + xlab('')+ ggtitle('HIMI')+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))


ggsave(filename = paste0('Data/op_depth_barplot_himi.tiff') , plot=op_depth_barplot_himi, device='tiff', width = 4, height = 4 )



