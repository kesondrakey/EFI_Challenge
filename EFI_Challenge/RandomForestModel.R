
library("randomForest")
library ("ggplot2")
library("ggRandomForests")
library(gridExtra)

#upload datahelllooooooo
#Morgan Monroe Data from Wenzhe
setwd("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/WenzhesFile")
FLX_US.MMS_FLUXNET2015_FULLSET_DD_1999.2014_1.3 <- read.csv("D:/Research/R_Files/Morgan_Monroe_Flux_Tower/WenzhesFile/FLX_US-MMS_FLUXNET2015_FULLSET_DD_1999-2014_1-3.csv")

MMF1 <- FLX_US.MMS_FLUXNET2015_FULLSET_DD_1999.2014_1.3
View(MMF1)

#Turn -9999 into NAs
MMF1[MMF1 == -9999] <- NA

#For ET, We need....
#LE          (W m-2): Latent heat flux
#TA             (deg C): Air temperature;                  
#PA             (kPa): Atmospheric pressure;                 
#USTAR    (m s-1): Friction velocity;                         
#H           (W m-2): Sensible heat flux
#NETRAD       (W m-2): Net radiation;                         
#VPD        (hPa): Vapor Pressure Deficit; 1hpa = 100pa
#P              (mm): Precipitation
#WS            (m s-1): Wind speed (m/s) (meters per second);  

#Variables important for ....
#LE
#air temperature, solar radiation, cloud area fraction, albedo, NDVI, surface downwelling longwave/shortwave flux in air


#ET
#LE, air temperature (TA_F_MDS; Air temperature, gapfilled using MDS method), air pressure, precip. flux, wind speed, VPD (air temp. & relative humidity
#NEE
#ET, SOM, LAI, air temp., precip., cloud area fraction


#VSWC
#precipitation, soil type/clay content?, NDVI


#random forest model (y is the thing you want to predict)
rf1 <- randomForest(NEE_CUT_05~TA_F+P_F+VPD_F+PA_F, data=MMF1, mtry=2, ntree=500, importance=TRUE)


#importance(rf1,type=2)
varImpPlot(rf1,type=2)
importanceOrder=order(rf1$importance)
names=rownames(rf1$importance)[importanceOrder][1:12]


aa <-plot(gg_vimp(rf1))+
  labs(x="",y="Variable Importance")+
  ggtitle("Ranking Variables by Importance")
aa



#predict function of NEE or other variable
rfsrc_NEE_test <- predict(rf1, newdata = MMF1 ,na.action = "na.impute",importance = TRUE)

#Should help in predicting R^2
#Save Dataframe
write.csv(rfsrc_NEE_test,file = "D:/Research/R_Files/Morgan_Monroe_Flux_Tower/WenzhesFile/rfsrc_NEE_test.csv",row.names = FALSE)

write.csv(MMF1,file = "D:/Research/R_Files/Morgan_Monroe_Flux_Tower/WenzhesFile/MMF1.csv",row.names = FALSE)






gg_vimp(rf1)


#plot this
aa <- plot(ggvimp(data))
aa

MMF1$N


gg_vimp(rf1)
library(gridExtra)
#event.marks <- c(1, 4)
#event.labels <- c(FALSE, TRUE)
#strCol <- brewer.pal(3, "Set1")[c(2,1,3)]
#vignette("randomForestSRC-Regression", package = "ggRandomForests")

rdata= read.table("F:\\LRF_GPP\\ResultMergeNineV132.txt",header=TRUE)
#radomn forest regression
rfsrc_Boston <- rfsrc(r.value~., data=rdata)

#labels
dta.labs <- data.frame(cbind(names = colnames(rdata), label = labels))
st.labs <- as.character(dta.labs$label)
#regression error
gg_e <- gg_error(rfsrc_Boston)
plot(gg_e)
#variable importance

aa <-plot(gg_vimp(rf1))+
  labs(x="",y="Variable importance")+
  ggtitle("a) Variable importance for SIF correlated to PDSI")
aa





#predict function of NEE or something
rfsrc_pbc_test <- predict(rfsrc_pbc, newdata = pbc.test,na.action = "na.impute",importance = TRUE)
