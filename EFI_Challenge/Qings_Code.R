###2 For historical dataset
##2.1 Extract inSW and netRad radiation
##step1: put all the .csv in a dataframe
setwd("C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/NEON_rad-net/NEON_rad-net")
files1 = list.files(pattern = "NEON.D01.BART.DP1.00023.001.000.060.030.SLRNR_30min.*.csv$", full.names = TRUE, recursive = TRUE)
df1 <- read.csv(files1[1],header = TRUE)
df2 <- read.csv(files1[2],header = TRUE)
df <- rbind(df1,df2)
for ( i in 3: length(files1)){
  dfi <- read.csv(files1[i],header = TRUE)
  df <- rbind(df,dfi)
}
##step2: Extract the Date, Year, DOY
df$date <- paste0(substr(df$startDateTime,1,10))
#Get Year, DOY
#a function for get year
DF <- data.frame(Date = df$date)
DF$Date <- as.Date(as.character(DF$Date),"%Y-%m-%d")  # %Y/%m/%d
Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year")))
DF_trans <- transform(DF, NumDays = Diff((Date + 1), Date),TotalDays = Diff(Date, Date[1]),Year = as.numeric(format(Date,'%Y'))) 
#add columns to df
df$Year[1:nrow(df)] <- as.numeric(DF_trans$Year)
df$DOY[1:nrow(df)] <- as.numeric(DF_trans$NumDays)
df$Hour[1:nrow(df)] <- rep(seq(0,23.5,by=0.5),(nrow(df)/48))
unique(df$Year)
unique(df$DOY)
write.csv(df,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/radiation.30m.bart.csv",row.names=FALSE)
##step3: Get daily average 
df_DD <- as.data.table(df)[, lapply(.SD, function(x) mean(x, na.rm=TRUE)), by = c("Year","DOY")]
#calculate net radiation
df_DD$Rg <- (df_DD$inSWMean - df_DD$outSWMean) - (df_DD$outLWMean - df_DD$inLWMean)
##plot
plot(df_DD$Rg,df_DD$inSWMean)
write.csv(df_DD,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/radiation.DD.bart.csv",row.names=FALSE)
##2.3 Extract relative humidity
##step1: put all the .csv in a dataframe
setwd("C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/NEON_rel-humidity")
files1 = list.files(pattern = "NEON.D01.BART.DP1.00098.001.000.060.030.RH_30min.*.csv$", full.names = TRUE, recursive = TRUE)
df1 <- read.csv(files1[1],header = TRUE)
df2 <- read.csv(files1[2],header = TRUE)
df <- rbind(df1,df2)
for ( i in 3: length(files1)){
  dfi <- read.csv(files1[i],header = TRUE)
  df <- rbind(df,dfi)
}
##step2: Extract the Date, Year, DOY
df$date <- paste0(substr(df$startDateTime,1,10))
#Get Year, DOY
#a function for get year
DF <- data.frame(Date = df$date)
DF$Date <- as.Date(as.character(DF$Date),"%Y-%m-%d")  # %Y/%m/%d
Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year")))
DF_trans <- transform(DF, NumDays = Diff((Date + 1), Date),TotalDays = Diff(Date, Date[1]),Year = as.numeric(format(Date,'%Y'))) 
#add columns to df
df$Year[1:nrow(df)] <- as.numeric(DF_trans$Year)
df$DOY[1:nrow(df)] <- as.numeric(DF_trans$NumDays)
df$Hour[1:nrow(df)] <- rep(seq(0,23.5,by=0.5),(nrow(df)/48))
unique(df$Year)
unique(df$DOY)
write.csv(df,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/RH.30m.bart.csv",row.names=FALSE)

##step3: Get daily average 
df_DD <- as.data.table(df)[, lapply(.SD, function(x) mean(x, na.rm=TRUE)), by = c("Year","DOY")]
##plot
plot(df_DD$RHMean)
write.csv(df_DD,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/RH.DD.bart.csv",row.names=FALSE)

##2.4 Extract 2D wind speed
##step1: put all the .csv in a dataframe
setwd("C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/NEON_wind-2d/NEON_wind-2d")
files1 = list.files(pattern = "NEON.D01.BART.DP1.00001.001.000.010.030.2DWSD_30min.*.csv$", full.names = TRUE, recursive = TRUE)
df1 <- read.csv(files1[1],header = TRUE)
df2 <- read.csv(files1[2],header = TRUE)
df <- rbind(df1,df2)
for ( i in 3: length(files1)){
  dfi <- read.csv(files1[i],header = TRUE)
  df <- rbind(df,dfi)
}
##step2: Extract the Date, Year, DOY
df$date <- paste0(substr(df$startDateTime,1,10))
#Get Year, DOY
#a function for get year
DF <- data.frame(Date = df$date)
DF$Date <- as.Date(as.character(DF$Date),"%Y-%m-%d")  # %Y/%m/%d
Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year")))
DF_trans <- transform(DF, NumDays = Diff((Date + 1), Date),TotalDays = Diff(Date, Date[1]),Year = as.numeric(format(Date,'%Y'))) 
#add columns to df
df$Year[1:nrow(df)] <- as.numeric(DF_trans$Year)
df$DOY[1:nrow(df)] <- as.numeric(DF_trans$NumDays)
df$Hour[1:nrow(df)] <- rep(seq(0,23.5,by=0.5),(nrow(df)/48))
unique(df$Year)
unique(df$DOY)
write.csv(df,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/WS.30m.bart.csv",row.names=FALSE)

##step3: Get daily average 
df_DD <- as.data.table(df)[, lapply(.SD, function(x) mean(x, na.rm=TRUE)), by = c("Year","DOY")]
##plot
plot(df_DD$windSpeedMean)
write.csv(df_DD,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/WS.DD.bart.csv",row.names=FALSE)

##3 Extract from .h5: temperature dataset, and water flux
##step1 decompress .h5.gz and save in unzip folder
library(R.utils)
#gunzip("file.gz", remove = `TRUE`)
setwd("C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/NEON_eddy-flux/NEON_eddy-flux")
files2 = list.files(pattern = "*.h5.gz$", full.names = TRUE, recursive = TRUE)
#for (i in 1 : length(files2)) {
#  uzpi <- paste0(in_dir,substr(files2[i],2,nchar(files2[i])))
#  gunzip(uzpi, remove = FALSE,overwrite = TRUE)
#}
setwd("C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/NEON_eddy-flux/unzip")
files2 = list.files(pattern = "*.h5$", full.names = TRUE, recursive = TRUE)
##step2: read .h5 files and put temperature in a dataframe
df <- data.frame()
df_fluxh2o <-data.frame()
for ( i in 1: length(files2)){
  h5f = H5Fopen(files2[i])
  #h5f
  aa <- h5f$BART #name
  dfi <- aa$dp01$data$tempAirTop$"000_060_30m"$temp
  df <- rbind(df,dfi)
  df_fluxh2oi <- aa$dp04$data$fluxH2o$nsae
  df_fluxh2o <- rbind(df_fluxh2o,df_fluxh2oi)
}
write.csv(df,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/temp.30m.ori.csv",row.names=FALSE)
write.csv(df_fluxh2o,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/fluxwater.30m.ori.csv",row.names=FALSE)

##step3: Extract the Date, Year, DOY for temperature
df <- read.csv("C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/temp.30m.ori.csv",header = TRUE)
df$date <- paste0(substr(df$timeBgn,1,10))
#Get Year, DOY
#a function for get year
DF <- data.frame(Date = df$date)
DF$Date <- as.Date(as.character(DF$Date),"%Y-%m-%d")  # %Y/%m/%d
Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year")))
DF_trans <- transform(DF, NumDays = Diff((Date + 1), Date),TotalDays = Diff(Date, Date[1]),Year = as.numeric(format(Date,'%Y'))) 
#add columns to df
df$Year[1:nrow(df)] <- as.numeric(DF_trans$Year)
df$DOY[1:nrow(df)] <- as.numeric(DF_trans$NumDays)
df$Hour[1:nrow(df)] <- rep(seq(0,23.5,by=0.5),(nrow(df)/48))
unique(df$Year)
unique(df$DOY)
write.csv(df,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/temp.30m.bart.csv",row.names=FALSE)
##step3: Get daily average 
df_DD <- as.data.table(df)[, lapply(.SD, function(x) mean(x, na.rm=TRUE)), by = c("Year","DOY")]
df_DD$Hour <- NA
##plot
plot(df_DD$mean)
write.csv(df_DD,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/temp.DD.bart.csv",row.names=FALSE)

##step4: Extract the Date, Year, DOY for water flux
df <- read.csv("C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/fluxwater.30m.ori.csv",header = TRUE)
df$date <- paste0(substr(df$timeBgn,1,10))
#Get Year, DOY
#a function for get year
DF <- data.frame(Date = df$date)
DF$Date <- as.Date(as.character(DF$Date),"%Y-%m-%d")  # %Y/%m/%d
Diff <- function(x, start) as.numeric(x - as.Date(cut(start, "year")))
DF_trans <- transform(DF, NumDays = Diff((Date + 1), Date),TotalDays = Diff(Date, Date[1]),Year = as.numeric(format(Date,'%Y'))) 
#add columns to df
df$Year[1:nrow(df)] <- as.numeric(DF_trans$Year)
df$DOY[1:nrow(df)] <- as.numeric(DF_trans$NumDays)
df$Hour[1:nrow(df)] <- rep(seq(0,23.5,by=0.5),(nrow(df)/48))
unique(df$Year)
unique(df$DOY)
write.csv(df,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/fluxwater.30m.bart.csv",row.names=FALSE)
##step3: Get daily average 
df_DD <- as.data.table(df)[, lapply(.SD, function(x) mean(x, na.rm=TRUE)), by = c("Year","DOY")]
df_DD$Hour <- NA
##plot
plot(df_DD$flux)
write.csv(df_DD,file = "C:/zIUwork/courses/2021spring/reading class/EFI/neon data/BART/bartoutput/fluxwater.DD.bart.csv",row.names=FALSE)