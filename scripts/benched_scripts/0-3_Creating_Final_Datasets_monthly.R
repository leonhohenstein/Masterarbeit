library(tidyverse)
library(ggplot2)
rm(list=ls())

######## DEFINE MODEL NAME AND CATCHMENT NAME HERE ############
model_name <- "RR1_lagged"
catchment <- "TB" #TB = Tauchenbach

#Read in pre-prepared meteorological data

meteo <- load(file = "data/tauchenbach/Meteo_Variables_Monthly_Tauchenbach.RData")
hydro <- load(file = "data/tauchenbach/Hydro_Variables_Tauchenbach_Monthly.RData")
data <- merge(Meteo_Monthly,data_Monthly, by = "year_month")
data$date <- paste0(data$year_month,"-15")
data$date <- as.POSIXct(data$date, format = "%Y-%m-%d", tz = "GMT")

############# WATERBALANCE AND SPEI / SPI ##################################

## Calculate water balance (P - ET) ##

data$WB_month <- data$precipitation_sum-data$ETP
WB_month <- data$precipitation_sum-data$ETP
## Calculate WB for differnt accumulation periods ##

# WB_week <- vector("numeric", length = length(data$WB_month))
# WB_2week <- vector("numeric", length = length(data$WB_month))
WB_2month <- vector("numeric", length = length(data$WB_month))
WB_3month <- vector("numeric", length = length(data$WB_month))
WB_6month <- vector("numeric", length = length(data$WB_month))
WB_12month <- vector("numeric", length = length(data$WB_month))
acc_values <- vector("numeric", length = length(data$WB_month))

acc_per_names <- c("WB_1month","WB_2month", "WB_3month", "WB_6month", "WB_12month") #defines the names of 
acc_per_value <- c(1,2,3,6,12)#defines the values in days for the accumulation periods whichis later used in the loop
WB_list <- list()

################### to iterate over all different acc periods: 
for (n in 1:length(acc_per_names)) {
  # n <- 2
  acc_per_names_n <- acc_per_names[n]#save the name of the accumulation period as variable
  acc_per_value_n <- acc_per_value[n]#save the value in days of the accumulation period as variable
  
  for (i in seq(acc_per_value_n, length(WB_month))) {
    
    acc_values[i] <- sum(WB_month[(i - acc_per_value_n):i]) #calculates the summed values of over the aggregation period
    
  }
  #paste(acc_per_i) <- acc_values #what i wanted originally
  #cwb_yearly_min_extract_long$district <- paste0(tolower(sub(" .*", "", acc_per_i[i])))
  
  acc_values[acc_values == 0] <- NA
  
  acc_values_relative <- acc_values/acc_per_value_n
  
  results_data <- data.frame(data$date,paste(acc_per_names_n),acc_values,acc_values_relative)
  colnames(results_data) <- c("Date", "Acc_Per", "WB_abs", "WB_rel")
  
  #cwb_yearly_min_extract_long$acc_per <- paste0(acc_per_i[i])
  
  WB_list[[acc_per_names_n]] <- results_data #implementationof that with lists
  
  
  acc_values <- vector("numeric", length = length(data$WB_day)) #create a "fresh" empty vector to avoid copying values from the previous iteration in the following one
  
  print("finished")  
  
}

WB_table_long <- do.call(rbind, WB_list)
WB_table_wide <- do.call(cbind, WB_list)
# WB_table_wide$paste0(WB_3month.WB_abs)


# WB_table_wide <- pivot_wider(WB_table_long, 
#                              #cols = "Acc_Per",
#                              names_from = "Date",
#                              values_from = c("WB_abs","WB_rel"))


ggplot(WB_table_long,) +
  #geom_line(aes(x = Date, y = WB_abs),colour = "red")+
  geom_line(aes(x = Date, y = WB_rel), colour = "green")+
  
  facet_wrap("Acc_Per")
###


# WB_table_long %>% 
#   select(paste0("Acc_Per")=="WB_3month")

########## Arrange the final Data set by removing surplus date columns and adding other variable columns ###

final_df <- WB_table_wide %>% #removing surplus date columns 
  select(-contains("Date"))
final_df <- final_df %>% #removing surplus columns that define the aggregation period
  select(-contains("Acc_Per"))

final_df$Date <- data$date

final_df$month <- format(data$date,"%m", tz = "GMT")
final_df$year <- format(data$date,"%Y", tz = "GMT")


final_df$T_min_mean_monthly <- data$Tmin_mean
final_df$T_max_mean_monthly <- data$Tmax_mean
final_df$snowcover_sum_monthly <- data$snowcover_sum
final_df$precipitation_sum_monthly <- data$precipitation_sum
final_df$sunshine_mean_monthly <- data$sunshine_mean
final_df$ETP_sum_monthly <- data$ETP
final_df$Flow_min_monthly <- data$Flow_min
final_df$BF_min_monthly <- data$baseflow_min
final_df$BF_max_monthly <- data$baseflow_max
final_df$BF_mean_monthly <- data$baseflow_mean


if(model_name == "RR1_lagged"){
  for(i in 2: (nrow(final_df))){
    # final_df$Flow_min_monthly_lag1 <- NA
    final_df$Flow_min_monthly_lag1[i] <-  final_df$Flow_min_monthly[i-1]
    
    
  }
  
  print("lag = 1 was calculated")   
}

if(model_name == "RR1_lagged"){
  for(i in 3: (nrow(final_df))){
    # final_df$Flow_min_monthly_lag1 <- NA
    final_df$Flow_min_monthly_lag2[i] <-  final_df$Flow_min_monthly[i-2]
    
    
  }
   print("lag = 2 was calculated")   
   print("Model is lagged model, therefore lagged flow variables were calculated and added to the final dataset")
}


df <- final_df
df %>% 
  select(-contains(".WB_rel")) %>%  # Remove columns containing ".rel"
  rename_with(~ gsub("\\.WB_abs$", "", .x), ends_with(".WB_abs"))
save(df,file = paste0("data/tauchenbach/models/",model_name,"_",catchment,"_monthly_data.RData"))





















