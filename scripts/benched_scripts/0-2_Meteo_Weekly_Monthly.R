library(ggplot2)
library(tidyverse)
library(data.table)
require(rsoi)
library(zoo)

# library(SPEI)
# library(lubridate)
# library(Evapotranspiration)

rm(list=ls())

file <- "data/tauchenbach/Predictors_Tauchenbach.csv"

# df <- read.csv2(file,sep = ",", header=T, skip=0, na.strings = ("NA")) #na.strings = ("L\374cke") if there is Lücke instead of NA 
# df[, 1] <- gsub("T|\\+00:00", " ", df[,1])

# df$date <- as.POSIXct(df$date,"%Y-%m-%d", tz="UTC")


load( file = "data/climate/spartacus_clipped_SN_TN.RData")
load( file = "data/climate/spartacus_clipped_TX_RR.RData")
load( file = "data/climate/spartacus_SR.RData")
load( file = "data/climate/spartacus_results_SA_TN_TX_RR_catchment_kienstock.RData")


stations_metadata <- data.frame(station_name = c("tauchenbach","kienstock","flattach","uttendorf"),
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

df <- df %>% na.omit()
unique(df$param)

catchment <- imap_dfr(results_SA_TN_TX_RR_catchment_kienstock,~ {
  
  param <- .y
  
  imap_dfr(.x, ~ {
    
    station <- .y
    
    df <- .x
    
    df %>%
      rename(value = 2) %>%
      mutate(param = param, station = station)
      

  })
  
})

df <- catchment %>% select(-"n_NAs") %>% rbind(df,.)
  
spartacus_SR <- spartacus_SR %>% pivot_longer(cols = (c("Flattach","Kienstock","Kienstock_Catchment","Uttendorf","Tauchenbach")),
                                              values_to = "value",
                                              names_to = "station")

spartacus_SR <- spartacus_SR %>% rename(date = time) %>% 
  mutate(param = "SR")

df <- spartacus_SR %>% select("date","value","param","station") %>% 
  rbind(df,.)
df <- df %>% filter(station != "Tauchenbach")

file <- "data/tauchenbach/Predictors_Tauchenbach.csv"
raw_data <- read.csv2(file,sep = ",", header=T, skip=0, na.strings = ("NA")) #na.strings = ("L\374cke") if there is Lücke instead of NA
# raw_data[, 1] <- gsub("T|\\+00:00", " ", raw_data[,1])
raw_data$date <- as.Date(raw_data$date) 
raw_data <- raw_data %>% rename(RR = rr, TN = tn, TX = tx, sa = SA, )

# df <- df %>%
#   mutate(station = recode(station,
#                               "Tauchenbach" = "tauchenbach",
#                               "Kienstock" = "kienstock",
#                               "Flattach" = "flattach",
#                               "Uttendorf" = "uttendorf",
#                           "Kienstock_Catchment" = "catchment_kienstock"))
# df <- df %>% setDT() %>% .[, year := lubridate::year(date)]
# ann_prec <- df[param == "RR", .(ann_prec = sum(value)), by = .(year, station)]
# ann_prec <- ann_prec[, .(ann_prec = mean(ann_prec)), by = (station)]
# 
# 
# test <- df %>% setDT() %>% .[param == "RR", .(n = .N), by = .(station, year)]


for (s in c("tauchenbach","kienstock","flattach","uttendorf","catchment_kienstock")) {
  # s <- "tauchenbach"
  df_temp <- df %>% filter(station == s)
  df_temp <- df_temp %>% pivot_wider(names_from = "param",values_from = "value")  
  
  # raw_data <- read.csv2(file,sep = ",", header=T, skip=0, na.strings = ("NA")) #na.strings = ("L\374cke") if there is Lücke instead of NA
  # raw_data[, 1] <- gsub("T|\\+00:00", " ", raw_data[,1])
  
  
  df_temp$date <- as.POSIXct(df_temp$date,"%Y-%m-%d", tz="UTC")
  colnames(df_temp) <- c("Date","station","precipitation",  "Tmax","Tmin","snowcover","sunshine")
  
  
latitude <- stations_metadata %>% filter(station_name == s) %>% 
  select("lat") %>% as.numeric()
if(s == "catchment_kienstock")
  {
  latitude <- stations_metadata %>% filter(station_name == "kienstock") %>% 
    select("lat") %>% as.numeric()
  }
df_temp$Year <- as.numeric(format(df_temp$Date, "%Y"))
df_temp$Month <- as.numeric(format(df_temp$Date, "%m"))
df_temp$Day <- as.numeric(format(df_temp$Date, "%d"))
df_temp$year_month <- paste0(df_temp$Year,"-",df_temp$Month)
df_temp$year_month <- as.Date(df_temp$year_month,"%Y-%m")
df_temp$year_month <- paste0(df_temp$Year,"-",df_temp$Month,"-15") %>% #just alternative way of the same from above
  as.POSIXct(., "%Y-%m-%d", tz = "UTC")
#then the same for precipitation data
data_daily <- df_temp


#load data from National Oceanic and Atmospheric Administration
# at: https://ftp.cpc.ncep.noaa.gov/cwlinks/
enso = rsoi::download_enso()

raw_data <- read.csv2(file = "data/climate/norm.daily.ao.cdas.z1000.19500101_current.csv", 
                      header=T, 
                      skip=0,dec = ".", 
                      na.strings = c("Lücke", "NA"), 
                      sep = ",",
                      stringsAsFactors = F)
raw_data$date <- make_date(year = raw_data$year, month = raw_data$month, day = raw_data$day)

oscillation <- raw_data 

raw_data <- read.csv2(file = "data/climate/norm.daily.pna.cdas.z500.19500101_current.csv", 
                      header=T, 
                      skip=0,dec = ".", 
                      na.strings = c("Lücke", "NA"), 
                      sep = ",",
                      stringsAsFactors = F)

raw_data$date <- make_date(year = raw_data$year, month = raw_data$month, day = raw_data$day)

oscillation <- raw_data %>%  select(c("date","pna_index_cdas")) %>% left_join(oscillation, ., by = "date")


raw_data <- read.csv2(file = "data/climate/norm.daily.nao.cdas.z500.19500101_current.csv", 
                      header=T, 
                      skip=0,dec = ".", 
                      na.strings = c("Lücke", "NA"), 
                      sep = ",",
                      stringsAsFactors = F)

raw_data$date <- make_date(year = raw_data$year, month = raw_data$month, day = raw_data$day)

oscillation <- raw_data %>%  select(c("date","nao_index_cdas")) %>% left_join(oscillation, ., by = "date")


oscillation <- enso %>% rename(date = Date) %>% select(c("ONI","SOI","date",)) %>% 
  left_join(oscillation, ., by = "date")

oscillation$ONI <- zoo::na.spline(oscillation$ONI)
oscillation$SOI <- zoo::na.spline(oscillation$SOI)

data_daily <- oscillation %>%  rename(Date = date) %>% 
  left_join(data_daily, ., by = "Date")

#calculating monthly data ----
#using the tidyverse package functions group_by and summarizese work with one categorial and one continuous variable
data_Monthly <- data_daily %>% 
  group_by(year_month) %>% 
  dplyr::summarize(Tmin_mean = mean(Tmin), #first define the desired variable and than the function with input variable
                   Tmax_mean = mean(Tmax),
                   snowcover_sum = sum(snowcover),
                   precipitation_sum = sum(precipitation),
                   
                   sunshine_mean = mean(sunshine),
                   AO_last = last(ao_index_cdas),
                   AO_mean = mean(ao_index_cdas),
                   PNA_last = last(pna_index_cdas),
                   PNA_mean = mean(pna_index_cdas),
                   ONI_last = last(ONI),
                   ONI_mean = mean(ONI),
                   SOI_last = last(SOI),
                   SOI_mean = mean(SOI),
                   NAO_last = last(nao_index_cdas),
                   NAO_mean = mean(nao_index_cdas)
                   ) %>% 
  as.data.frame()

Meteo_Monthly <- data_Monthly
Meteo_Monthly$ETP <- data_Monthly$ETP
Meteo_Monthly$year_month <- Meteo_Monthly$year_month %>% 
  as.POSIXct(.,"%Y-%m")
save(Meteo_Monthly,file = paste0("data/tauchenbach/Meteo_Variables_Monthly_",".RData"))


#calculating weekly data ----

#### computing reference EvapoTranspiration ----

computing_external_radiation <- function(lat, J) # J is doy
{
  # solar declination, rad (1 rad = 57.2957795 deg)
  delta <- 0.409 * sin(0.0172 * J - 1.39)
  # relative distance Earth-Sun, []
  dr <- 1 + 0.033 * cos(0.0172 * J)
  # sunset hour angle, rad
  latr <- lat / 57.2957795
  sset <- -tan(latr) * tan(delta)
  omegas <- sset * 0
  omegas[abs(sset) <= 1] <- acos(sset[abs(sset) <= 1])
  # correction for high latitudes
  omegas[sset < (-1)] <- max(omegas)
  # Ra, MJ m-2 d-1
  Ra <- 37.6 * dr *
    (omegas * sin(latr) * sin(delta) + cos(latr) * cos(delta) * sin(omegas))
  Ra <- ifelse(Ra < 0, 0, Ra)
  Ra
}

computing_hargreaves <- function(Tmax, Tmin, lat, Prec, J)
{
  Tmean <- (Tmin + Tmax)/2
  Tr <- Tmax - Tmin
  Tr <- pmax(0,Tr)
  ET0 <- Tmin*NA
  Ra <- computing_external_radiation(lat = lat, J = J)
  ab <- Tr - 0.0123*Prec
  ET0 <- 0.0013 * 0.408 * Ra * (Tmean + 17.0) * ab^0.76
  ET0[is.nan(ab^0.76)] <- 0
  ET0 <- ifelse(ET0 < 0, 0, ET0)
  ET0
}

df_temp$doy <- lubridate::yday(df_temp$Date)
df_temp$Ra <- computing_external_radiation(lat = latitude, df_temp$doy)
df_temp$rET <-
  computing_hargreaves(
    Tmax = df_temp$Tmax,
    Tmin = df_temp$Tmin,
    lat = latitude,
    Prec = df_temp$precipitation,
    J = df_temp$doy)

df_temp$week_year <- isoweek(df_temp$Date)
start_week<- min(which(df_temp$week_year == 2))
df_temp$week_tot <- c(rep(1,(start_week-1)),rep(2:(2 + ceiling(nrow(df_temp) / 7) - 1), each = 7)[1:(nrow(df_temp)-start_week+1)])

data_daily <- df_temp %>%  select(c("Date","rET","week_tot","week_year","Ra")) %>%  
  left_join(data_daily, ., by = "Date")

Meteo_Weekly <- data_daily %>% setDT() %>% 
  .[,.(date = last(Date),
    precipitation = sum(precipitation),
    Tmin = mean(Tmin),
    Tmax = mean(Tmax),
    sunshine = mean(sunshine), 
    snowcover = sum(snowcover),
    Ra = sum(Ra),
    rET = sum(rET),
    year = last(Year),
    month = last(Month),
    week_year = last(week_year),
    AO_last = last(ao_index_cdas),
    AO_mean = mean(ao_index_cdas, na.rm = T),
    PNA_last = last(pna_index_cdas),
    PNA_mean = mean(pna_index_cdas, na.rm = T),
    ONI_last = last(ONI),
    ONI_mean = mean(ONI, na.rm = T),
    SOI_last = last(SOI),
    SOI_mean = mean(SOI, na.rm = T),
    NAO_last = last(nao_index_cdas),
    NAO_mean = mean(nao_index_cdas), na.rm = T)
    , by = week_tot]

# Meteo_Weekly[which(Meteo_Weekly, is.na = T),]

# Meteo_Weekly$date[!complete.cases(Meteo_Weekly)]

test <- Meteo_Weekly %>%  filter(year < 2024)
test$date[!complete.cases(test)]
  
  if(s == "catchment_kienstock")
  {
    save(Meteo_Weekly,file = paste0("data/","kienstock","/Meteo_Variables_Weekly_","catchment_kienstock",".RData"))
  }

  else
  {
  save(Meteo_Weekly,file = paste0("data/",s,"/Meteo_Variables_Weekly_",s,".RData"))
  }

}
