library(ggplot2)
library(tidyverse)
library(data.table)
require(rsoi)

# library(SPEI)
# library(lubridate)
# library(Evapotranspiration)

rm(list=ls())

file <- "data/tauchenbach/Predictors_Tauchenbach.csv"

df <- read.csv2(file,sep = ",", header=T, skip=0, na.strings = ("NA")) #na.strings = ("L\374cke") if there is Lücke instead of NA 
# df[, 1] <- gsub("T|\\+00:00", " ", df[,1])

df$date <- as.POSIXct(df$date,"%Y-%m-%d", tz="UTC")

colnames(df) <- c("precipitation","Date", "Tmin", "Tmax","sunshine","snowcover")
attach(df)
df$precipitation <- as.numeric(precipitation)

df$sunshine <- as.numeric(sunshine)

df$Tmin <- as.numeric(Tmin)

df$Tmax <- as.numeric(Tmax)

df$snowcover <- as.numeric(snowcover)


#first for the spartacus data

df$Year <- as.numeric(format(df$Date, "%Y"))
df$Month <- as.numeric(format(df$Date, "%m"))
df$Day <- as.numeric(format(df$Date, "%d"))
df$year_month <- paste0(df$Year,"-",df$Month)
df$year_month <- as.Date(df$year_month,"%Y-%m")
df$year_month <- paste0(df$Year,"-",df$Month,"-15") %>% #just alternative way of the same from above
  as.POSIXct(., "%Y-%m-%d", tz = "UTC")
#then the same for precipitation data
data_daily <- df


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

oscillation <- enso %>% rename(date = Date) %>% select(c("ONI","SOI","NPGO","date")) %>% 
  left_join(oscillation, ., by = "date")

oscillation$ONI <- zoo::na.spline(oscillation$ONI)
oscillation$NPGO <- zoo::na.spline(oscillation$NPGO)
oscillation$SOI <- zoo::na.spline(oscillation$SOI)

oscillation %>% 
  filter( year == 2015) %>% 
  ggplot()+
  geom_line(aes(x=date, y = ONI), color = "green")+
  geom_line(aes(x=date, y = NPGO), color = "blue")+
  geom_line(aes(x=date, y = SOI), color = "red")+
  lims(y = c(-10,10))
  
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
                   ao_last = last(ao_index_cdas),
                   ao_mean = mean(ao_index_cdas),
                   pna_last = last(pna_index_cdas),
                   pna_mean = mean(pna_index_cdas),
                   ONI_last = last(ONI),
                   ONI_mean = mean(ONI),
                   SOI_last = last(SOI),
                   SOI_mean = mean(SOI),
                   NPGO_last = last(NPGO),
                   NPGO_mean = mean(NPGO)) %>% 
  as.data.frame()

Meteo_Monthly <- data_Monthly
Meteo_Monthly$ETP <- data_Monthly$ETP
Meteo_Monthly$year_month <- Meteo_Monthly$year_month %>% 
  as.POSIXct(.,"%Y-%m")
save(Meteo_Monthly,file = "data/tauchenbach/Meteo_Variables_Monthly_Tauchenbach.RData")


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

df$doy <- lubridate::yday(df$Date)
df$Ra <- computing_external_radiation(lat = 47.34697, df$doy)
df$rET <-
  computing_hargreaves(
    Tmax = df$Tmax,
    Tmin = df$Tmin,
    lat = 47.34697,
    Prec = df$precipitation,
    J = df$doy)

df$week_year <- isoweek(df$Date)
start_week<- min(which(df$week_year == 2))
df$week_tot <- c(rep(1,(start_week-1)),rep(2:(2 + ceiling(nrow(df) / 7) - 1), each = 7)[1:(nrow(df)-start_week+1)])

data_daily <- df %>%  select(c("Date","rET","week_tot","week_year","Ra")) %>%  
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
    ao_last = last(ao_index_cdas),
    ao_mean = mean(ao_index_cdas, na.rm = T),
    pna_last = last(pna_index_cdas),
    pna_mean = mean(pna_index_cdas, na.rm = T),
    ONI_last = last(ONI),
    ONI_mean = mean(ONI, na.rm = T),
    SOI_last = last(SOI),
    SOI_mean = mean(SOI, na.rm = T),
    NPGO_last = last(NPGO),
    NPGO_mean = mean(NPGO), na.rm = T), by = week_tot]

Meteo_Weekly[which(Meteo_Weekly, is.na = T),]

Meteo_Weekly$date[!complete.cases(Meteo_Weekly)]

test <- Meteo_Weekly %>%  filter(year < 2024)
test$date[!complete.cases(test)]


save(Meteo_Weekly,file = "data/tauchenbach/Meteo_Variables_Weekly_Tauchenbach.RData")

