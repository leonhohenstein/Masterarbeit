library(lfstat)
library(ggplot2)
library(tidyverse)

########### load data ##############
getwd()
rm(list=ls())
file <- "data/Tauchenbach/Daily_Flow_Tauchenbach.csv"
raw_data <- read.csv2(file, header=T, skip=0,dec = ".", na.strings = c("Lücke", "NA"), sep = ",") #na.strings = ("L\374cke") if there is Lücke instead of NA 
raw_data[, 1] <- as.POSIXct(raw_data[,1],"%Y-%m-%d", tz="UTC")


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
test <- data_lfobj$baseflow
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

ggplot(seasonality_df,) +
  geom_point(aes(x = years, y = winter_Q95, colour = "red"))+
  geom_line(aes(x = years, y = winter_Q95, colour = "red"))+
  
  geom_point(aes(x = years, y = summer_Q95, colour = "blue"))+
  geom_line(aes(x = years, y = summer_Q95, colour = "blue"))+
  
  geom_point(aes(x = years, y = SR_yearly, colour = "green"))+
  geom_line(aes(x = years, y = SR_yearly, colour = "green"))+
  
  geom_hline(aes(yintercept = 0.8))+#indicating threshhold for summer regine
  geom_hline(aes(yintercept = 1.25))+#indicating threshhold for summer regine
  labs(colour = "Legend")+
  theme(legend.position="right")+
  scale_shape_discrete(name  ="Payer",
                       breaks=c("Female", "Male","ratio"),
                       labels=c("Woman", "Man","summer"))
  
#using the tidyverse package functions group_by and summarizese work with one categorial and one continuous variable
#first create a year_month column to group the data
data$year_month <- paste0(data$year,"-",data$month)
data$year_month <- as.Date(data$date,"%Y-%m")
data$year_month <- paste0(data$year,"-",data$month,"-15") %>% #just alternative way of the same from above
  as.POSIXct(., "%Y-%m-%d", tz = "UTC")
data$baseflow <- data_lfobj$baseflow

data_Monthly <- data %>% 
  group_by(year_month) %>% 
  dplyr::summarize(Flow_min = min(flow),                   #first define the desired variable and than the function with input variable
                   baseflow_min = min(baseflow),
                   baseflow_mean = mean(baseflow),
                   baseflow_max = max(baseflow)
                   ) %>% 
  as.data.frame()


save(data_Monthly,file = "data/tauchenbach/Hydro_Variables_Tauchenbach_Monthly.RData")
