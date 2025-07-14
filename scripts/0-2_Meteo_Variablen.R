library(ggplot2)
library(tidyverse)
library(SPEI)
library(lubridate)
library(Evapotranspiration)
# library(EcoHydRology)

rm(list=ls())

# file <- "data/tauchenbach/Predictors_Tauchenbach.csv"
load( file = "data/climate/spartacus_clipped_SN_TN.RData")
load( file = "data/climate/spartacus_clipped_TX_RR.RData")
load( file = "data/climate/spartacus_SR.RData")

stations_metadata <- data.frame(station_name = c("Tauchenbach","Kienstock","Flattach","Uttendorf"),
                                catchment_size = c(175,95970,705,128),
                                elevation = c(247,194,687,789),
                                hzb_nr = c(210252,207357,213124,203554),
                                lat = c(47.34697, 48.38217,46.933330,47.26654),
                                lon = c(16.27815,15.46268,13.13712,12.56822))


df <- imap_dfr(results_final, ~ {  
  
    param <- .y
    
    imap_dfr(.x, ~ {
      
      station <- .y
      
      df <- .x
      
      df %>% rename(value = 2) %>% 
        mutate(param = param, station = station)
      
    })
  })

df <- imap_dfr(results_final_SA_TN, ~ {  
  
        param <- .y
        
        imap_dfr(.x, ~ {
          
          station <- .y
          
          df <- .x
          
          df %>% rename(value = 2) %>% 
            mutate(param = param, station = station)
          
        })
      }) %>% 
             rbind(df,.)

spartacus_SR <- spartacus_SR %>% pivot_longer(cols = (c("Flattach","Kienstock","Kienstock_Catchment","Uttendorf","Tauchenbach")),
                                      values_to = "value",
                                      names_to = "station")

spartacus_SR <- spartacus_SR %>% rename(date = time) %>% 
  mutate(param = "SR")

df <- spartacus_SR %>% select("date","value","param","station") %>% 
  rbind(df,.)


for (s in unique(stations_metadata$station_name)) {
  # s <- "Tauchenbach"
df_temp <- df %>% filter(station == s)
df_temp <- df_temp %>% pivot_wider(names_from = "param",values_from = "value")  

# raw_data <- read.csv2(file,sep = ",", header=T, skip=0, na.strings = ("NA")) #na.strings = ("L\374cke") if there is Lücke instead of NA 
# raw_data[, 1] <- gsub("T|\\+00:00", " ", raw_data[,1])


df_temp$date <- as.POSIXct(df_temp$date,"%Y-%m-%d", tz="UTC")
colnames(df_temp) <- c("Date","station","precipitation",  "Tmax","Tmin","snowcover","sunshine")

data <- df_temp
# 
# data$date <- as.Date(data$Date)
# ggplot() +
#   geom_line(data = df %>% filter(param == "RR" & lubridate::year(date) == 2015), aes(x=date, y= value))+
#   geom_line(data = data %>% filter(lubridate::year(date) == 2015), aes(x=date, y= precipitation),color = "steelblue")

# test_data <- data %>% select(date,precipitation)
# test_df <- df %>% filter(param == "RR" & station =="Tauchenbach") %>%  select(date,value)
# test <- left_join(test_data,test_df,by = "date")
# test <- test %>% filter(lubridate::year(date) < 2024)
# sum(test$precipitation)
# sum(test$value, na.rm = T)
# sum(is.na(test$value))
# sum(is.na(test$precipitation))
# 
# test[which(is.na(test$value) == T ),]

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
latitide <- stations_metadata %>% filter(station_name == s) %>% 
  select("lat") %>% as.numeric()

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




