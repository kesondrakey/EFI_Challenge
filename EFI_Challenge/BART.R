setwd("C:\Users\keyke\OneDrive - Indiana University\Forecast challenge data\BART\CLEANED")

install.packages("caret")
?caret


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
View(df5)





df5 <- merge(df1, df2, by=c("Year", "DOY", "Hour"), all.x=TRUE)
df6 <- merge(df3, df4, by=c("Year", "DOY", "Hour"), all.x=TRUE)

df7 <- merge(df5, df6, by=c("Year", "DOY", "Hour"), all.x=TRUE)

View(df7)





MultVariables_Bart <- df7
write.table(MultVariables_Bart,file="Variables_Bart.csv",sep=" ",quote=FALSE,append=FALSE,row.names = FALSE,na="NA")

View(Variables_Bart)


df8 <- LAI_Bart_DOY
names(df8)[names(df8) == "mean"] <- "LAI_mean"
head(df8)

df9 <- NDVI_Bart_DOY
names(df9)[names(df9) == "mean"] <- "NDVI_mean"
head(df9)

df10 <- merge(df8, df9, by=c("DOY"), all.x=TRUE)
head(LAI_NDVI_Bart)

LAI_NDVI_Bart <- df10
write.table(LAI_NDVI_Bart,file="LAI_NDVI_Bart.csv",sep=" ",quote=FALSE,append=FALSE,na="NA")

#turn data that doesnt pass quality control into NAs
data[data == -9999] <- NA




#Run Model
#Variables here: MultVariables_Bart
#average DOY things herE: LAI_NDVI_Bart

View(MultVariables_Bart) #Incoming radiation, precipitation, temperature, humidity, wind speed
head(LAI_NDVI_Bart) #LAI and NDVI Averages















#random forest model code (from the morgan monroe forest test)

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
