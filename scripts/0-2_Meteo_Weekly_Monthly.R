library(ggplot2)
library(tidyverse)
library(data.table)
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

#calculating monthly data ----
#using the tidyverse package functions group_by and summarizese work with one categorial and one continuous variable
data_Monthly <- df %>% 
  group_by(year_month) %>% 
  dplyr::summarize(Tmin_mean = mean(Tmin), #first define the desired variable and than the function with input variable
                   Tmax_mean = mean(Tmax),
                   snowcover_sum = sum(snowcover),
                   precipitation_sum = sum(precipitation),
                   
                   sunshine_mean = mean(sunshine),) %>% 
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

Hydro_Weekly <- df %>% setDT() %>% 
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
    week_year = last(week_year)), by = week_tot]

save(Meteo_Weekly,file = "data/tauchenbach/Meteo_Variables_Weekly_Tauchenbach.RData")

