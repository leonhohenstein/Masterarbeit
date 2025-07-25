library(ggplot2)
library(tidyverse)
library(data.table)
require(rsoi)
library(zoo)


rm(list=ls())

file <- "data/Tauchenbach/Predictors_Tauchenbach.csv"

load( file = "data/climate/spartacus_clipped_SN_TN.RData")
load( file = "data/climate/spartacus_clipped_TX_RR.RData")
load( file = "data/climate/spartacus_SR.RData")
load( file = "data/climate/spartacus_results_SA_TN_TX_RR_catchment_Kienstock.RData")
load(file = "data/climate/spartacus_results_snow_stations.RData")
load(file = "data/climate/spartacus_results_snow_catchment_Kienstock.RData")

stations_metadata <- data.frame(station_name = c("Tauchenbach","Kienstock","Flattach","Uttendorf"),
                                catchment_size = c(175,95970,705,128),
                                elevation = c(247,194,687,789),
                                hzb_nr = c(210252,207357,213124,203554),
                                lat = c(47.34697, 48.38217,46.933330,47.26654),
                                lon = c(16.27815,15.46268,13.13712,12.56822))

results_SA_TN_TX_RR_catchment_kienstock[["snow_depth"]] <- results_snow_catchment_kienstock[["snow_depth"]]
results_SA_TN_TX_RR_catchment_kienstock[["swe_tot"]] <- results_snow_catchment_kienstock[["swe_tot"]]





df <- imap_dfr(results_snow_stations, ~ {  
  
  station <- .y
  
  imap_dfr(.x, ~ {
    
    param <- .y
    
    df <- .x
    
    df %>% rename(value = 2) %>% 
      mutate(param = param, station = station)
    
  })
}) 



df <- imap_dfr(results_final, ~ {  
  
  param <- .y
  
  imap_dfr(.x, ~ {
    
    station <- .y
    
    df <- .x
    
    df %>% rename(value = 2) %>% 
      mutate(param = param, station = station)
    
  })
}) %>% 
  rbind(df,.)




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

catchment$station <- "Kienstock_Catchment"
df$station[which(df$station == "Kienstock")] <- "Kienstock"
df <- catchment %>% select(-"n_NAs") %>% rbind(df,.)

spartacus_SR <- spartacus_SR %>% pivot_longer(cols = (c("Flattach","Kienstock","Kienstock_Catchment","Uttendorf","Tauchenbach")),
                                              values_to = "value",
                                              names_to = "station")

spartacus_SR <- spartacus_SR %>% rename(date = time) %>% 
  mutate(param = "SR")

df <- spartacus_SR %>% select("date","value","param","station") %>% 
  rbind(df,.)

# file <- "data/Tauchenbach/Predictors_Tauchenbach.csv"
# raw_data <- read.csv2(file,sep = ",", header=T, skip=0, na.strings = ("NA")) #na.strings = ("L\374cke") if there is Lücke instead of NA
# # raw_data[, 1] <- gsub("T|\\+00:00", " ", raw_data[,1])
# raw_data$date <- as.Date(raw_data$date) 
# raw_data <- raw_data %>% rename(RR = rr, TN = tn, TX = tx, SA = sa, snow_depth = snow)

# df <- df %>% filter(station != "Tauchenbach")

# raw_data <- raw_data %>% pivot_longer(cols = c("RR","TN","TX","SA","snow_depth"), values_to = "value",
#                                       names_to = "param")
# raw_data$value <- as.numeric(raw_data$value)
# raw_data$station <- "Tauchenbach_Joh"

# raw_data$date <- as.Date(raw_data$date)
# raw_data <- df %>% filter(station == "Tauchenbach") %>% 
#   rbind(raw_data, .)


df <- df %>% mutate(value = as.numeric(value))

#load data from National Oceanic and Atmospheric Administration
# at: https://ftp.cpc.ncep.noaa.gov/cwlinks/
enso = rsoi::download_enso()

for (s in c("Tauchenbach","Kienstock","Flattach","Uttendorf","Kienstock_Catchment")) {
  
  # s <- "catchment_Kienstock"
  df_temp <- df %>% filter(station == s)
  df_temp <- df_temp %>%  distinct()
  df_temp <- df_temp %>% pivot_wider(names_from = "param",values_from = "value")  
  df_temp$date <- as.POSIXct(df_temp$date,"%Y-%m-%d", tz="UTC")
  df_temp <- df_temp %>% rename(precipitation = RR, 
                                Tmin = TN,
                                Tmax = TX,
                                sunshine = SA)
  # colnames(df_temp) <- c("Date","station","precipitation",  "Tmax","Tmin","snowcover","sunshine")
  
  latitude <- stations_metadata %>% filter(station_name == s) %>% 
    select("lat") %>% as.numeric()
  if(s == "Kienstock_Catchment")
  {
    latitude <- stations_metadata %>% filter(station_name == "Kienstock") %>% 
      select("lat") %>% as.numeric()
  }
  df_temp$Year <- as.numeric(format(df_temp$date, "%Y"))
  df_temp$Month <- as.numeric(format(df_temp$date, "%m"))
  df_temp$Day <- as.numeric(format(df_temp$date, "%d"))
  df_temp$year_month <- paste0(df_temp$Year,"-",df_temp$Month)
  df_temp$year_month <- as.Date(df_temp$year_month,"%Y-%m")
  df_temp$year_month <- paste0(df_temp$Year,"-",df_temp$Month,"-15") %>% #just alternative way of the same from above
    as.POSIXct(., "%Y-%m-%d", tz = "UTC")
  #then the same for precipitation data
  data_daily <- df_temp
  
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
  
  data_daily <- oscillation %>% 
    left_join(data_daily, ., by = "date")
  
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
  
  df_temp$doy <- lubridate::yday(df_temp$date)
  df_temp$Ra <- computing_external_radiation(lat = latitude, df_temp$doy)
  df_temp$rET <-
    computing_hargreaves(
      Tmax = df_temp$Tmax,
      Tmin = df_temp$Tmin,
      lat = latitude,
      Prec = df_temp$precipitation,
      J = df_temp$doy)
  
  df_temp$week_year <- isoweek(df_temp$date)
  start_week<- min(which(df_temp$week_year == 2))
  df_temp$week_tot <- c(rep(1,(start_week-1)),rep(2:(2 + ceiling(nrow(df_temp) / 7) - 1), each = 7)[1:(nrow(df_temp)-start_week+1)])
  
  data_daily <- df_temp %>%  select(c("date","rET","week_tot","week_year","Ra")) %>%  
    left_join(data_daily, ., by = "date")
  
  data_daily <- data_daily %>% mutate(snow_depth_diff = c(0,diff(snow_depth)),
                                               swe_tot_diff = c(0,diff(swe_tot)),
                                      snow_added = ifelse(snow_depth_diff > 0, snow_depth_diff, 0),
                                      snow_melted = ifelse(snow_depth_diff < 0, snow_depth_diff, 0),
                                      swe_added = ifelse(swe_tot_diff > 0, swe_tot_diff, 0),
                                      swe_melted = ifelse(swe_tot_diff < 0, swe_tot_diff, 0)
  )
  
 
  
  Meteo_Weekly <- data_daily %>% setDT() %>% 
    .[,.(date = last(date),
         precipitation = sum(precipitation),
         Tmin = mean(Tmin),
         Tmax = mean(Tmax),
         sunshine = mean(sunshine), 
         snow_depth = sum(snow_depth),
         swe_tot = sum(swe_tot),
         snow_added = sum(snow_added),
         snow_melted = sum(snow_melted),
         swe_added = sum(swe_added),
         swe_melted = sum(swe_melted),
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
  
  Meteo_Weekly <- Meteo_Weekly %>% mutate(snow_added_agg_4 =rollsum(snow_added, 4, align = "right", na.pad = T),
                                          snow_melted_agg_4 = rollsum(snow_added, 4, align = "right", na.pad = T),
                                          swe_added_agg_4 = rollsum(snow_added, 4, align = "right", na.pad = T),
                                          swe_melted_agg_4 = rollsum(snow_added, 4, align = "right", na.pad = T),
                                          snow_added_agg_8 =rollsum(snow_added, 8, align = "right", na.pad = T),
                                          snow_melted_agg_8 = rollsum(snow_added, 8, align = "right", na.pad = T),
                                          swe_added_agg_8 = rollsum(snow_added, 8, align = "right", na.pad = T),
                                          swe_melted_agg_8 = rollsum(snow_added, 8, align = "right", na.pad = T))
  
  
  # Meteo_Weekly[which(Meteo_Weekly, is.na = T),]
  
  # Meteo_Weekly$date[!complete.cases(Meteo_Weekly)]
  
  if(s == "Kienstock_Catchment")
  {
    save(Meteo_Weekly,file = paste0("data/","Kienstock","/Meteo_Variables_Weekly_","Kienstock_Catchment",".RData"))
  }
  
  else
  {
    save(Meteo_Weekly,file = paste0("data/",s,"/Meteo_Variables_Weekly_",s,".RData"))
  }
  
}

