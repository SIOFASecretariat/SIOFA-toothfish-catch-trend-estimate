#Script to fit GLM standardisation model to CPUE data on SIOFA 3b 

#Not yet working 

#Load Data----------------------------------------------------------------------
load(file="Data/TOP_MU_map.RData")

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

#filter unrealistic soaktime & distance 
CPUE = Catch %>% 
  filter(Soaktime<15 & Soaktime>0) %>% 
  filter(DistLL<10000) %>% 
  mutate(LogCPUE=log(CPUE+1))

# Fitting GLM ------------------------------------------------------------------

# predictors collinearity and versus response (CPUE)
pairs(CPUE %>% select(CPUE,Season,Depth, Soaktime, DistLL)%>%mutate(LogCPUE=log(CPUE+1)))
cor(CPUE%>%select(CPUE,Season,Depth, Soaktime, DistLL)%>%mutate(LogCPUE=log(CPUE+1))%>%filter(!is.na(Depth)))

# only first order effects
fit1 <- glm(CPUE~ factor(vesselCode) +factor(Season)+factor(Month)+Latitude+Longitude+Depth+Soaktime+DistLL,
            family=gaussian(link="identity"), data=CPUE)

# latitude x longitude interaction
fit2 <- glm(CPUE~factor(vesselCode) +factor(Season)+factor(Month)+Latitude*Longitude+Depth+Soaktime+DistLL,
            family=gaussian(link="identity"), data=CPUE)

# latitude x longitude interaction and second order effect of DEPTH (~...+x+x^2)
fit3 <- glm(CPUE~factor(vesselCode) +factor(Season)+factor(Month) + +Soaktime+DistLL + Latitude*Longitude+Depth+I(Depth^2),
            family=gaussian(link="identity"), data=CPUE)

# latitude x longitude interaction and second order effect of DEPTH (~...+x+x^2) and log link
fit4 <- glm(LogCPUE~factor(vesselCode) +factor(Season)+factor(Month)+ Depth+Soaktime+DistLL + Latitude*Longitude+Depth+I(Depth^2),
            family=gaussian(link="identity"), data=CPUE)

# only first order effects + logtransformation
fit5 <- glm(LogCPUE~factor(vesselCode)  +factor(Season)+factor(Month)+ Depth+Soaktime+DistLL + Latitude*Longitude+I(Depth^2),
            family=gaussian(link="identity"), data=CPUE)

# only first order effects
fit6 <- glm(LogCPUE~factor(vesselCode) + factor(Season)+factor(Month)+Latitude+Longitude+Depth+Soaktime+DistLL,
            family=gaussian(link="identity"), data=CPUE)

# model summary and diagnostics
AIC(fit1, fit2, fit3, fit4,fit5, fit6)

# summary statistics of the fitted models
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

# diagnostic plots
plot(fit6)

termplot(fit6, CPUE, "Season", se=TRUE)


# model prediction, including plotting results ---------------------------------
yr <- 2020

pred <- predict(fit6, type="response", data=CPUE)
plot(fit6$model[,1],pred, cex=0.8, xlab="observed", ylab="predicted")
abline(a=0,b=1,col="red")

plot(1,1,type="n",xlim=c(min(CPUE$Longitude)-0.1, max(CPUE$Longitude)+0.1), 
     ylim=c(min(CPUE$Latitude)-0.1, max(CPUE$Latitude)+0.1), 
     xlab="Longitude", ylab="Latitude", main=paste("observed CPUE",yr,sep=" - "))

CPUE_plot = CPUE  %>% filter(Season==yr)
CPUE_plot$size.pt = ( 3*pred[CPUE$Season==yr] / max(fit6$model[CPUE$Season==yr,1]) )

TOP_MU_map + geom_point(data= CPUE_plot ,
                        aes(x= Longitude , y= Latitude, cex= size.pt), shape =1, col='blue' ) 
            

# Plot:   Annual mean CPUE obs vs. pred #####
png("Plot_annualCPUE.png",width = 428, height = 428)

yr.ref <- as.numeric(names(which.max(table(CPUE$Season))))
mod.variab <- c("Latitude","Longitude","Depth","Month","Season","Soaktime","DistLL","vesselCode")
pred.grid <- CPUE[CPUE$Season==yr.ref,mod.variab]
tmp <- rep(unique(CPUE$Season),rep(dim(pred.grid)[1],length(unique(CPUE$Season))))

for(i in 1:(length(unique(CPUE$Season))-1)){
  pred.grid <- rbind(pred.grid,CPUE[CPUE$Season==yr.ref,mod.variab])
}
pred.grid$Season <- tmp

dat_test=CPUE%>%select(Latitude,Longitude,Depth,Month,Season,Soaktime,DistLL,vesselCode)%>%
  mutate(Season=factor(Season)) %>%
  droplevels()

fit7 <- glm(LogCPUE~factor(Gear) + factor(Season)+ Latitude*Longitude+Depth+Soaktime,
            family=gaussian(link="identity"), data=CPUE)
summary(fit7)
pred <- predict(fit7,dat_test , type="response", se=TRUE)


#pred <- predict(fit5, pred.grid, type="response", se=TRUE)
obs.yr <- aggregate(fit7$model[,1], 
                    list(as.numeric(levels(fit7$model[,3]))[fit7$model[,3]]), mean)

#pred.yr <- aggregate(pred$fit, list(pred.grid$Year), mean,na.rm=TRUE)
pred.yr <- aggregate(pred$fit, list(dat_test$Season), mean,na.rm=TRUE)

#pred.yr.se.up <- aggregate(pred$fit+1.96*pred$se.fit, list(pred.grid$Year), mean,na.rm=TRUE)
#pred.yr.se.lo <- aggregate(pred$fit-1.96*pred$se.fit, list(pred.grid$Year), mean,na.rm=TRUE)
pred.yr.se.up <- aggregate(pred$fit+1.96*pred$se.fit, list(dat_test$Season), mean,na.rm=TRUE)
pred.yr.se.lo <- aggregate(pred$fit-1.96*pred$se.fit, list(dat_test$Season), mean,na.rm=TRUE)

pred.yr[,1]= as.numeric(as.character(pred.yr[,1]))
pred.yr.se.up[,1]= as.numeric(as.character(pred.yr.se.up[,1]))
pred.yr.se.lo[,1]= as.numeric(as.character(pred.yr.se.lo[,1]))

plot(x=obs.yr[,1], y=obs.yr$x, type="p", ylim=c(0,0.2),
     xlab="Year", ylab="mean CPUE")
lines(x=pred.yr[,1], y=pred.yr$x, lty=1)
lines(x=pred.yr.se.up[,1], y=pred.yr.se.up$x, lty=2)
lines(x=pred.yr.se.lo[,1], y=pred.yr.se.lo$x, lty=2)

dev.off()




# Plot:   Annual mean CPUE obs vs. pred #####
png("PlotCPUE_A.png",width = 428, height = 428)

datA<-CPUE
yr.ref <- as.numeric(names(which.max(table(datA$Season))))
mod.variab <- c("Longitude","Soaktime","Latitude","Depth","Season","Gear","vesselCode")
pred.grid <- datA[datA$Season==yr.ref, mod.variab]
tmp <- rep(unique(datA$Season),rep(dim(pred.grid)[1],length(unique(datA$Season))))

for(i in 1:(length(unique(datA$Season))-1)){
  pred.grid <- rbind(pred.grid,datA[datA$Season==yr.ref,mod.variab])
}
pred.grid$Season <- tmp


fit7 <- glm(LogCPUE~ factor(Gear)+factor(Season)+ Latitude + Longitude+ Soaktime,
            family=gaussian(link="identity"), data=CPUE)

pred <- predict(fit7,pred.grid , type="response", se=TRUE)


obs.yr <- aggregate(fit7$model[,1], list(as.numeric(levels(fit7$model[,3]))[fit7$model[,3]]), mean)
pred.yr <- aggregate(pred$fit, list(pred.grid$Season), mean,na.rm=TRUE)

#pred.yr.se.up <- aggregate(pred$fit+1.96*pred$se.fit, list(pred.grid$Year), mean,na.rm=TRUE)
#pred.yr.se.lo <- aggregate(pred$fit-1.96*pred$se.fit, list(pred.grid$Year), mean,na.rm=TRUE)
pred.yr.se.up <- aggregate(pred$fit+1.96*pred$se.fit, list(pred.grid$Season), mean,na.rm=TRUE)
pred.yr.se.lo <- aggregate(pred$fit-1.96*pred$se.fit, list(pred.grid$Season), mean,na.rm=TRUE)

plot(obs.yr[,1], exp(obs.yr$x), type="p", #ylim=c(min(exp(pred.yr.se.lo$x)),max(exp(pred.yr.se.up$x)))
     xlab="Year", ylab="mean CPUE")
lines(pred.yr[,1], exp(pred.yr$x), lty=1)
lines(pred.yr.se.up[,1], exp(pred.yr.se.up$x), lty=2)
lines(pred.yr.se.lo[,1], exp(pred.yr.se.lo$x), lty=2)

dev.off()




