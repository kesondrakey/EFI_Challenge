setwd("C:/Users/keyke/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/")

#import data C:\Users\keyke\OneDrive - Indiana University\Forecast challenge data\BART\CLEANED
Inc_Rn_Bart <- read.csv("C:/Users/keyke/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/BART.incoming_rad.30m.csv")
Precip_Bart <- read.csv("C:/Users/keyke/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/BART.precip.30m.csv")
Temp_Hum_Bart <- read.csv("C:/Users/keyke/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/BART.RH.air_temp.30m.csv")
WS_Bart <- read.csv("C:/Users/keyke/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/WS.30m.bart.csv")
LAI_Bart_DOY <- read.csv("C:/Users/keyke/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/BART.LAI.DOY.csv")
NDVI_Bart_DOY <- read.csv("C:/Users/keyke/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/BART.NDVI.DOY.csv")

#adding month and day columns too
library(tidyverse)
library(lubridate)
library(dplyr)
#Radiation
#adding in month and day columns
Inc_Rn_Bart1 <- Inc_Rn_Bart %>%
  mutate(startDateTime = ymd_hms(startDateTime), 
         Day = day(startDateTime),
         Month = month(startDateTime))

#Sierra's code 
Inc_Rn_Bart1$inSWMean[which(Inc_Rn_Bart1$inSWFinalQF == 1)] <- NA # NA where QF = 1 data
Inc_Rn_Bart1$inLWMean[which(Inc_Rn_Bart1$inLWFinalQF == 1)] <- NA 

library(tidyr)
View(radiation_BART_key)
radiation_BART_key <- Inc_Rn_Bart1 %>% unite("key", c(Year,DOY, Hour), sep = "_", remove = F) # add key
radiation_BART_key <- radiation_BART_key[ -c(4,6) ] # remove QF columns
radiation_BART_key <- radiation_BART_key[!is.na(radiation_BART_key$inSWMean) 
                                         & !is.na(radiation_BART_key$inLWMean),] #remove na columns
#Precipitation
Precip_Bart1 <- Precip_Bart %>%
  mutate(date = ymd(date), 
         Day = day(date),
         Month = month(date))

Precip_Bart1$secPrecipBulk[which(Precip_Bart1$secPrecipRangeQF == 1)] <- NA
precip_BART_key <- Precip_Bart1 %>% unite("key", c(Year,DOY, Hour), sep = "_", remove = F)
precip_BART_key <- precip_BART_key[ -c(7) ]

#Temperature and Humidity
Temp_Hum_Bart1 <- Temp_Hum_Bart %>%
  mutate(date = ymd(date), 
         Day = day(date),
         Month = month(date))

Temp_Hum_Bart1$tempRHMean[which(Temp_Hum_Bart1$tempRHFinalQF == 1)] <- NA
Temp_Hum_Bart1$RHMean[which(Temp_Hum_Bart1$RHFinalQF == 1)] <- NA
airtemp_RH_BART_key <- Temp_Hum_Bart1 %>% unite("key", c(Year,DOY, Hour), sep = "_", remove = F)
airtemp_RH_BART_key <- airtemp_RH_BART_key[ -c(7,9) ]
airtemp_RH_BART_key <- airtemp_RH_BART_key[!is.na(airtemp_RH_BART_key$RHMean) 
                                           & !is.na(airtemp_RH_BART_key$tempRHMean),]
View(airtemp_RH_BART_key)

#Wind Speed
WS_Bart1 <- WS_Bart %>%
  mutate(startDateTime = ymd_hms(startDateTime), 
         Day = day(startDateTime),
         Month = month(startDateTime))

WS_Bart1$windSpeedMean[which(WS_Bart1$windDirFinalQF == 1)] <- NA
windspeed_BART_key <- WS_Bart1 %>% unite("key", c(Year,DOY, Hour), sep = "_", remove = F)
windspeed_BART_key <- windspeed_BART_key[ -c(4) ]
windspeed_BART_key <- windspeed_BART_key[!is.na(windspeed_BART_key$windSpeedMean),]


###Merge dataframes with similar setup
head(Inc_Rn_Bart1) #date, year, Doy, hour, day, month
head(precip_BART_key) #data, year, doy, hour, day, month
head(airtemp_RH_BART_key) #date, year, doy, hour, day, month
head(WS_Bart1) #date, year, doy, hour, day, month

head(LAI_Bart_DOY) #doy
head(NDVI_Bart_DOY)#doy

df1 <- radiation_BART_key
df2 <- precip_BART_key
df3 <- airtemp_RH_BART_key
df4 <- windspeed_BART_key

df5 <- merge(df1, df2, by=c("Year", "DOY", "Hour"), all.x=TRUE)
df6 <- merge(df3, df4, by=c("Year", "DOY", "Hour"), all.x=TRUE)

df7 <- merge(df5, df6, by=c("Year", "DOY", "Hour"), all.x=TRUE)
View(df7)

test <- df7
#deleting excessive date columns
test1 <- test[ -c(12, 13, 15, 16) ]
test2 <- test1[ -c(13, 14, 17:20, 22:25)]
View(test2)


MultVariables_Bart <- test2
head(MultVariables_Bart)

write.csv(MultVariables_Bart, file = "C:/Users/keyke/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/Bart_Variables.csv",row.names = FALSE)
Bart_Variables <- read.csv("C:/Users/keyke/OneDrive - Indiana University/Forecast challenge data/BART/CLEANED/Bart_Variables.csv")
View(Bart_Variables)



df8 <- LAI_Bart_DOY
names(df8)[names(df8) == "mean"] <- "LAI_mean"
head(df8)

df9 <- NDVI_Bart_DOY
names(df9)[names(df9) == "mean"] <- "NDVI_mean"
head(df9)

df10 <- merge(df8, df9, by=c("DOY"), all.x=TRUE)
head(df10)

LAI_NDVI_Bart <- df10
write.table(LAI_NDVI_Bart,file="LAI_NDVI_Bart.csv",sep=" ",quote=FALSE,append=FALSE,na="NA")

LAI_NDVI_Bart <- read.csv("C:/R_Files/EFI_Challenge/LAI_NDVI_Bart.csv", sep="")
View(LAI_NDVI_Bart)

#Data with clean dates and quality controlled are:
#LAI_NDVI_Bart
#Bart_Variables

















#Run Model
#Variables here: MultVariables_Bart
#average DOY things herE: LAI_NDVI_Bart
head(Bart_multiple_Wenzhe) #Incoming radiation, precipitation, temperature, humidity, wind speed
head(LAI_NDVI_Bart) #LAI and NDVI Averages

#install.packages("caret")
library("caret")

#create time slices
Bart_Timeslice <- createTimeSlices(1:nrow(Bart_multiple_Wenzhe), initialWindow = 2880, horizon = 576, fixedWindow = TRUE, skip = 0)
View(Bart_Timeslice)


datapart <- createDataPartition(Bart_multiple_Wenzhe$Hour,Bart_multiple_Wenzhe$DOY, times = 2, p = 0.8, list = TRUE)
View(datapart)

#workflow
#data cleaning
#figure out how to fix up the data via r code
#current data looks too small to include all of the obs? 

#groups for data training
#1. add a new column
#2. assign 1 through 19 ; 54950 observations (from wenzhe's excel data)
#3. within that column, group A is 1:2880, group be 2881:.... , etc.
#4. randomly assign 4 of those groups to testing (this will be 20%)







#111504/2880 = ~38 chunks of two month time increments 
#categories 1-5
#randomly (random # gen) assign 80% of 1-38 into training and 20% into testing


#https://www.machinelearningplus.com/machine-learning/caret-package/#3datapreparationandpreprocessing
install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))





#random forest model code (from the Morgan Monroe forest test)

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
