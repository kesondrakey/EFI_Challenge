setwd("C:\Users\keyke\OneDrive - Indiana University\Forecast challenge data\BART\CLEANED")

#import data 
Inc_Rn_Bart <- read.csv("C:/Users/Karmic Dreamwork.000/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/BART.incoming_rad.30m.csv")
LAI_Bart_DOY <- read.csv("C:/Users/Karmic Dreamwork.000/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/BART.LAI.DOY.csv")
NDVI_Bart_DOY <- read.csv("C:/Users/Karmic Dreamwork.000/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/BART.NDVI.DOY.csv")
Precip_Bart <- read.csv("C:/Users/Karmic Dreamwork.000/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/BART.precip.30m.csv")
Temp_Hum_Bart <- read.csv("C:/Users/Karmic Dreamwork.000/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/BART.RH.air_temp.30m.csv")
WS_Bart <- read.csv("C:/Users/Karmic Dreamwork.000/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/WS.30m.bart.csv")


###Merge dataframes with similar setup
head(Inc_Rn_Bart) #date, year, Doy, hour
head(Precip_Bart) #data, year, doy, hour
head(Temp_Hum_Bart) #date, year, doy, hour
head(WS_Bart) #date, year, doy, hour

head(LAI_Bart_DOY) #doy
head(NDVI_Bart_DOY)#doy

head(WS_Bart)

df1 <- Inc_Rn_Bart
df2 <- Precip_Bart
df3 <- Temp_Hum_Bart
df4 <- WS_Bart

df5 <- merge(df1, df2, by=c("date", "Year", "DOY", "Hour"), all.x=TRUE)
df6 <- merge(df3, df4, by=c("date", "Year", "DOY", "Hour"), all.x=TRUE)

df7 <- merge(df5, df6, by=c("date", "Year", "DOY", "Hour"), all.x=TRUE)
View(df7)
MultVariables_Bart <- df7

write.csv(MultVariables_Bart,file = "C:/Users/Karmic Dreamwork.000/Desktop",row.names=FALSE)

write.csv(MultVariables_Bart,file = "C:/Users/Karmic Dreamwork.000/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/",row.names=FALSE)


df8 <- LAI_Bart_DOY
df9 <- NDVI_Bart_DOY

df10 <- merge(df6, df7, by=c("DOY"), all.x=TRUE)
head(df10)



head(df3)
View(df3)
###


head(Temp_Hum_Bart)
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
