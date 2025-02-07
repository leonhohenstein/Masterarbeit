library(ggplot2)
library(tidyverse)
library(SPEI)
library(lubridate)
library(Evapotranspiration)
# library(EcoHydRology)

rm(list=ls())

file <- "data/tauchenbach/Predictors_Tauchenbach.csv"
raw_data <- read.csv2(file,sep = ",", header=T, skip=0, na.strings = ("NA")) #na.strings = ("L\374cke") if there is Lücke instead of NA 
# raw_data[, 1] <- gsub("T|\\+00:00", " ", raw_data[,1])
raw_data$date <- as.POSIXct(raw_data$date,"%Y-%m-%d", tz="UTC")
colnames(raw_data) <- c("precipitation","Date", "Tmin", "Tmax","sunshine","snowcover")
attach(raw_data)
raw_data$precipitation <- as.numeric(precipitation)
raw_data$sunshine <- as.numeric(sunshine)
raw_data$Tmin <- as.numeric(Tmin)
raw_data$Tmax <- as.numeric(Tmax)
raw_data$snowcover <- as.numeric(snowcover)

data <- raw_data

# ####### Calculate Monthly Means #########
# start_date <- pmax(precipitation[1,1],data[1,1]) #select the start of both time series (only take the greater value as a start date)
# precipitation <- precipitation[precipitation$Date>=start_date,]
# data <- data[data$Date>=start_date,]
# 
# end_date <- pmin(max(precipitation[,1]),
#                    max(data[,1])) #select the end date of both time series (only take the greater value as a start date)
# precipitation <- precipitation[precipitation$Date<end_date,]
# data <- data[data$Date<end_date,]

# #now that both dataframe have the same length, they can be joint
# data$precipitation <- precipitation$prec_daily

#first for the spartacus data

data$Year <- as.numeric(format(data$Date, "%Y"))
data$Month <- as.numeric(format(data$Date, "%m"))
data$Day <- as.numeric(format(data$Date, "%d"))
data$year_month <- paste0(data$Year,"-",data$Month)
data$year_month <- as.Date(data$year_month,"%Y-%m")
data$year_month <- paste0(data$Year,"-",data$Month,"-15") %>% #just alternative way of the same from above
 as.POSIXct(., "%Y-%m-%d", tz = "UTC")
#then the same for precipitation data
data_daily <- data

#using the tidyverse package functions group_by and summarizese work with one categorial and one continuous variable
data_Monthly <- data %>% 
  group_by(year_month) %>% 
  dplyr::summarize(Tmin_mean = mean(Tmin), #first define the desired variable and than the function with input variable
                   Tmax_mean = mean(Tmax),
                   snowcover_sum = sum(snowcover),
                   precipitation_sum = sum(precipitation),
                   
                   sunshine_mean = mean(sunshine),) %>% 
  as.data.frame()


### Calculate Potential EvapoTranspiration by Hargreaves method ############+
# latitide <- raw_data$lat[1] %>% 
latitide <- 47.34697 %>% 
  as.numeric()

data_Monthly$ETP <- hargreaves(data_Monthly$Tmin_mean, 
                                     data_Monthly$Tmax_mean, 
                                           Ra = NULL, 
                                           lat = latitide, 
                                           Pre = NULL, na.rm = T, 
                                           verbose=TRUE)


######## Assemble final Meteo Dataset ##########
# Monthly values

Meteo_Monthly <- data_Monthly
Meteo_Monthly$ETP <- data_Monthly$ETP
Meteo_Monthly$year_month <- Meteo_Monthly$year_month %>% 
  as.POSIXct(.,"%Y-%m")
save(Meteo_Monthly,file = "data/tauchenbach/Meteo_Variables_Monthly_Tauchenbach.RData")

##############################################################################################
##############################################################################################
##############################################################################################

##############################################################################################
##############################################################################################
##############################################################################################

##############################################################################################
##############################################################################################
##############################################################################################

#Trying to get daily ET

#now the calculation of ET by Hargreaves-Samani 
constants <- list()
constants$elev <- 246.96
constants$lambda <- 2.45
constants$lat_rad <- 47.34697
constants$Gsc <- 0.0820
 
data <- data.frame(raw_data$Tmax, raw_data$Tmin)
constants()

data_test <- data(climatedata)
constants <- data(constants)

ET_daily <- ET.HargreavesSamani(data_test, 
   constants, 
   ts="monthly",
   message="yes",
   AdditionalStats="yes"
   )



