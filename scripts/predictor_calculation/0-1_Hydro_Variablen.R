library(lfstat)
library(ggplot2)
library(tidyverse)
library(data.table)
library(zoo)
########### load data ##############
getwd()
rm(list=ls())

stations <- c("tauchenbach","kienstock","flattach","uttendorf")
flow_quantiles_list <- list()
for (station_name in stations) {

  # station_name <- "kienstock"

file <- paste0("data/",station_name,"/daily_flow_",station_name,".csv")
raw_data <- read.csv2(file, 
                      header=F, 
                      skip=22,dec = ",", 
                      na.strings = c("Lücke", "NA"), 
                      sep = ";",
                      stringsAsFactors = F) #na.strings = ("L\374cke") if there is Lücke instead of NA 
raw_data[, 1] <-  as.POSIXct(raw_data[, 1], format = "%d.%m.%Y %H:%M", tz = "UTC")

raw_data[, 2] <- trimws(raw_data[, 2])

raw_data[, 2] <- as.numeric(raw_data[, 2])

raw_data <- data.frame(date=raw_data[, 1], 
           day = as.numeric(format(raw_data[, 1], "%d")), 
           month = as.numeric(format(raw_data[, 1], "%m")), 
           year = as.numeric(format(raw_data[,1], "%Y")), 
           flow = raw_data[, 2])#raw_dates <- as.POSIXct(paste(raw_data$Date, raw_data$Year, sep = "-"), format = "%Y-%m-%d"))# in case if there is date and year to merge
# raw_data$flow <- raw_data$flow %>% 
#   gsub(",",".",.)
# raw_data$flow <- as.numeric(raw_data$flow)

start_day <- raw_data$date[1]

data_lfobj <- createlfobj(raw_data,
            hyearstart = 11, #hydrological year in Austria and Germany starts at 1st November(11), ends 31st of October
            baseflow = T,  # logical, should the base flow curve be calculated? Needed, if you want to apply functions 'bfplot' or 'BFI' later on.
            #startdate = start_day,  # start of the time-series  
            #dateformat = "%Y-%m-%d" #Format of the start date) 
            )
plot(raw_data$date,data_lfobj$flow,  type = "l") 
lines(raw_data$date,data_lfobj$baseflow, col = 2)

# BASEFLOW INDEX
# BFI_vector_seasonal <- BFI(data_lfobj, 
#                            year = "any",
#                            breakdays = c("01/11","01/05"), 
#                            yearly = T)
# 
# BFI_vector_annually <- BFI(data_lfobj, 
#                            year = "any",
#                            #breakdays = c("01/11","01/05"), 
#                            yearly = T,
#                           startdate = start_day
#                            )
# #BFI_vector_annually <- BFI_vector_annually[-length(BFI_vector_annually)]
# # rm(BFI_df)
# BFI_df <- data.frame(BFI_vector_annually,unique(data_lfobj$year))
# for (i in 1:(length(BFI_vector_seasonal))-4) {
#   BFI_df$BFI_summer[i] <- BFI_vector_seasonal[2*i]
#   BFI_df$winter[i] <- BFI_vector_seasonal[i+2*i-2]
#   
# }
# 
# BFI(data_lfobj)
# bfplot(data_lfobj, year = "any", col = "green", bfcol = "blue", ylog = FALSE)
# data_lfobj$flow
# unique(data_lfobj$year)
# bfplot(data_lfobj, year = "any")
# str(data_lfobj)

#------------------------# SEASONALITY RATIO # ----------------------------------------------------------
#SR = Q95summer/Q95winter,where Q95summer is the 0.05 quantile of daily discharge for the summer period (May to 174 November), 
#and Q95winter the corresponding 0.05 quantile for the winter period of the 175 respective station.
#A SR below 0.8 indicates a summer regime, a SR above 1.25 (1/0.8) 176 determines a winter regime, 
#and a SR between 0.8 and 1.25 is defined as a mixed regime.

years <- as.numeric(unique(raw_data$year))
summer_Q95 <- vector("numeric", length = length(years))
winter_Q95 <- vector("numeric")

for (i in 1:length(years)) {  
  data <- raw_data #get a "fresh" version of the main data set before each iteration of the loop

  #i <- 1 # to test loop
  
  year <- years[i]
  winter_start <- as.Date(paste0(year,"-11-01"), "%Y-%m-%d")
  winter_end <- as.Date(paste0(year+1,"-04-30"), "%Y-%m-%d")
  winter_data <- subset(data, date >= winter_start & 
                            date <= winter_end)
  winter_Q95[i] <- quantile(winter_data$flow, probs = 0.05, na.rm = T)
  
  summer_start <- as.Date(paste0(year,"-05-01"), "%Y-%m-%d")
  summer_end <- as.Date(paste0(year+1,"-10-31"), "%Y-%m-%d")
  summer_data <- subset(data, date >= summer_start & 
                          date <= summer_end)
  summer_Q95[i] <- quantile(summer_data$flow, probs = 0.05, na.rm = T)
  
}

seasonality_df <- data.frame(years,winter_Q95,summer_Q95)
seasonality_df$SR_yearly <- seasonality_df$summer_Q95/seasonality_df$winter_Q95
# 
# ggplot(seasonality_df,) +
#   geom_point(aes(x = years, y = winter_Q95, colour = "red"))+
#   geom_line(aes(x = years, y = winter_Q95, colour = "red"))+
#   
#   geom_point(aes(x = years, y = summer_Q95, colour = "blue"))+
#   geom_line(aes(x = years, y = summer_Q95, colour = "blue"))+
#   
#   geom_point(aes(x = years, y = SR_yearly, colour = "green"))+
#   geom_line(aes(x = years, y = SR_yearly, colour = "green"))+
#   
#   geom_hline(aes(yintercept = 0.8))+#indicating threshhold for summer regine
#   geom_hline(aes(yintercept = 1.25))+#indicating threshhold for summer regine
#   labs(colour = "Legend")+
#   theme(legend.position="right")+
#   scale_shape_discrete(name  ="Payer",
#                        breaks=c("Female", "Male","ratio"),
#                        labels=c("Woman", "Man","summer"))
  
#using the tidyverse package functions group_by and summarizese work with one categorial and one continuous variable
#first create a year_month column to group the data
data$year_month <- paste0(data$year,"-",data$month)
data$year_month <- as.Date(data$date,format = "%Y-%m")
data$year_month <- paste0(data$year,"-",data$month,"-15") %>% #just alternative way of the same from above
  as.POSIXct(., "%Y-%m-%d", tz = "UTC")
data$baseflow <- data_lfobj$baseflow

data_Monthly <- data %>% 
  group_by(year_month) %>% 
  dplyr::summarize(flow_mean = min(flow),                   #first define the desired variable and than the function with input variable
                   baseflow_mean = min(baseflow),
                   baseflow_mean = mean(baseflow),
                   baseflow_max = max(baseflow)
                   ) %>% 
  as.data.frame()


save(data_Monthly,file = paste0("data/",station_name,"/Hydro_Variables_",station_name,"_Monthly.RData"))


#calculating weekly flow variables -----

dates_all <- data.frame(date = seq(as.Date("1961-01-01"), as.Date("2021-12-31"), by = "day"))
dates_all$week_year <- lubridate::isoweek(dates_all$date)
setDT(dates_all) 
dates_all[, year := lubridate::isoyear(date)]

dates_all[, week_tot := .GRP, by = .(week_year,year)]

# dates_all$week_tot <- c(1,rep(2:(2 + ceiling(nrow(dates_all) / 7) - 1), each = 7)[1:(nrow(dates_all)-1)])

df <- data
df <- dates_all %>% select(c("date","week_tot","week_year")) %>% left_join(., df, by = "date")
df <- subset(df, lubridate::isoyear(date) >= 1961)

# df$week_year <- isoweek(df$date)

Hydro_Weekly <- df %>% setDT() %>% 
  .[,.(date = last(date),
       flow_mean = mean(flow),
       year = last(year),
       month = last(month),
       week_year = last(week_year)), by = week_tot]

#Calculating Aggregates of hydrographs ----

Hydro_Weekly <- Hydro_Weekly %>%
  arrange(date) %>%
  mutate(flow_ma1 = rollmean(flow_mean, k = 1, align = "right", fill = NA),
         diff_ma1 = c(NA, diff(flow_ma1)),
         flow_ma2 = rollmean(flow_mean, k = 2, align = "right", fill = NA),
         diff_ma2 = c(NA, diff(flow_ma2)),
         flow_ma4 = rollmean(flow_mean, k = 4, align = "right", fill = NA),
         diff_ma4 = c(NA, diff(flow_ma4)),
         flow_ma8 = rollmean(flow_mean, k = 8, align = "right", fill = NA),
         diff_ma8 = c(NA, diff(flow_ma8)))


#Calculating Slope of hydrographs ----

compute_trailing_spline <- function(x, y, window_size = 30, spar = NULL, degfree = NULL) {
  
  n <- length(y)
  deriv_1 <- rep(NA_real_, n)
  deriv_2 <- rep(NA_real_, n)
  flow_spline <- rep(NA_real_, n)
  
  for (i in seq_along(y)) {
    if (i < window_size) next  # Not enough past data
    
    idx_window <- (i - window_size + 1):i
    x_win <- x[idx_window]
    y_win <- y[idx_window]
    
    # Fit smoothing spline with either spar or df
    fit <- if (!is.null(spar)) {
      smooth.spline(x_win, y_win, spar = spar)
    } else if (!is.null(degfree)) {
      smooth.spline(x_win, y_win, df = degfree)
    } else {
      stop("Specify either spar or degfree for smoothing")
    }
    
    # Predict at the last point (current i)
    deriv_1[i] <- predict(fit, x[i], deriv = 1)$y
    deriv_2[i] <- predict(fit, x[i], deriv = 2)$y
    flow_spline[i] <- predict(fit, x[i])$y
  }
  
  results <- data.frame(deriv_1 = deriv_1, deriv_2 = deriv_2, flow_spline = flow_spline)
  
  return(results)
}

Hydro_Weekly <- Hydro_Weekly[which(is.na(Hydro_Weekly$flow_mean)==F)[1]:nrow(Hydro_Weekly),]
results_spline <- compute_trailing_spline(x = as.numeric(Hydro_Weekly$date),
                                          y = Hydro_Weekly$flow_mean, window = 10, spar = 0.99, degfree = NULL) #second version
sum(is.na(Hydro_Weekly$flow_mean))
# Apply to your vector
# results_spline <- smooth_trailing_spline(df$flow, window = 30) #first version


# rm(df_test)
Hydro_Weekly <- cbind(Hydro_Weekly, results_spline)

# 
# df_test %>%  
#   filter(year == 2018) %>% 
#   ggplot()+
#   geom_point(aes(x=date, y = flow_mean)) +
#   geom_line(aes(x=date, y = flow_spline), color = "steelblue")+
#   geom_line(aes(x=date, y = deriv_1), color = "darkred")+
#   geom_line(aes(x=date, y = deriv_2), color = "forestgreen")
#   
  
save(Hydro_Weekly,file = paste0("data/",station_name,"/Hydro_Variables_",station_name,"_Weekly.RData"))

##### calculate Q95 and Q94 and Q96
quantiles <- vector(mode = "numeric", length = 100)

for(i in 1:100){
  quantiles[i] <- quantile(raw_data$flow, probs = i/100, na.rm = T)
  
}

paste0("test",1:10)

flow_quantiles <- data.frame(flow = quantiles, quantiles = c(1:100))

# q96 <- quantile(raw_data$flow, probs = 0.04, na.rm = T)
# q95 <- quantile(raw_data$flow, probs = 0.05, na.rm = T)
# q94 <- quantile(raw_data$flow, probs = 0.06, na.rm = T)
# q90 <- quantile(raw_data$flow, probs = 0.1, na.rm = T)
# q50 <- quantile(raw_data$flow, probs = 0.5, na.rm = T)

# q95_winter <- mean(seasonality_df$winter_Q95)
# q95_summer <- mean(seasonality_df$summer_Q95) 
# SR_mean <- mean(seasonality_df$SR_yearly)
# SR_SD <- sd(seasonality_df$SR_yearly)
# MQ <- quantile(raw_data$flow, probs = 0.5, na.rm = T)
# flow_quantiles <- data.frame(station_name,q96,q95,q94,q95_winter,q95_summer,q90,q50,MQ,SR_mean, SR_SD)
# colnames(flow_quantiles) <- c("station","Q96","Q95","Q94","Q95_winter","Q95_summer","Q90","Q50","MQ","SR_mean","SR_SD")

flow_quantiles_list[[paste0(station_name)]] <- flow_quantiles
}
save(flow_quantiles_list,file = "data/flow_quantiles_list.RData")

