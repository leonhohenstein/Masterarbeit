library(tidyverse)
library(ggplot2)
rm(list=ls())

##### ### WEEKLY DATA ############
# model_name <- "RR1_lagged"
# catchment <- "TB" #TB = Tauchenbach

stations_list <- c(
  "tauchenbach",
                   "flattach",
                   "kienstock",
                   "uttendorf",
  "catchment_kienstock")
for (station in stations_list) {
  
  station <- "kienstock"
  


load(file = paste0("data/",station,"/Meteo_Variables_Weekly_",station,".RData")) %>% 
  as_tibble()
load(file = paste0("data/",station,"/Hydro_Variables_",station,"_Weekly.RData"))
Hydro_Weekly <- Hydro_Weekly %>% 
  select(-c("month","year","week_tot","week_year"))

data <- left_join(Hydro_Weekly,Meteo_Weekly, by = "date")

# test <- Meteo_Weekly %>% filter(lubridate::year(date) > 1978)

############# WATERBALANCE AND SPEI / SPI ##################################

## Calculate water balance (P - ET) ##

# data$WB_week <- data$precipitation_sum-data$rET
WB_week <- data$precipitation-data$rET
WB_2week <- vector("numeric", length = length(data$WB_week))
WB_3week <- vector("numeric", length = length(data$WB_week))
WB_6week <- vector("numeric", length = length(data$WB_week))
WB_12week <- vector("numeric", length = length(data$WB_week))
acc_values <- vector("numeric", length = length(data$WB_week))

acc_per_names <- c("WB_1week","WB_2week", "WB_3week", "WB_6week", "WB_12week", "WB_24week","WB_52week") #defines the names of 
acc_per_value <- c(1,2,3,6,12, 24, 52)#defines the values in days for the accumulation periods whichis later used in the loop
WB_list <- list()

################### to iterate over all different acc periods: 
for (n in 1:length(acc_per_names)) {
  # n <- 1
  acc_per_names_n <- acc_per_names[n]#save the name of the accumulation period as variable
  acc_per_value_n <- acc_per_value[n]#save the value in days of the accumulation period as variable
  
  for (i in seq(acc_per_value_n, length(WB_week))) {
    
    acc_values[i] <- sum(WB_week[(i - acc_per_value_n):i]) #calculates the summed values of over the aggregation period
    
  }
  
  acc_values[acc_values == 0] <- NA
  
  acc_values_relative <- acc_values/acc_per_value_n
  
  results_data <- data.frame(data$date,paste(acc_per_names_n),acc_values,acc_values_relative)
  colnames(results_data) <- c("Date", "Acc_Per", "WB_abs", "WB_rel")
  
  WB_list[[acc_per_names_n]] <- results_data #implementationof that with lists
  
  acc_values <- vector("numeric", length = length(data$WB_day)) #create a "fresh" empty vector to avoid copying values from the previous iteration in the following one
  
  print("finished")  
  
}

WB_table_long <- do.call(rbind, WB_list)
WB_table_wide <- do.call(cbind, WB_list)


########## Arrange the final Data set by removing surplus date columns and adding other variable columns ###
dates <- WB_table_wide$WB_1week.Date
df <- WB_table_wide %>% #removing surplus date columns 
  select(-contains("Date"))
df$Date <- dates
df <- df %>% #removing surplus columns that define the aggregation period
  select(-contains("Acc_Per"))

data <- rename(data,Date = date)

# df$Date <- data$date

# final_df$T_min_mean_weekly <- data$Tmin_mean
# final_df$T_max_mean_weekly <- data$Tmax_mean
# final_df$snowcover_sum_weekly <- data$snowcover_sum
# final_df$precipitation_sum_weekly <- data$precipitation_sum
# final_df$sunshine_mean_weekly <- data$sunshine_mean
# final_df$ETP_sum_weekly <- data$ETP
# final_df$Flow_min_weekly <- data$Flow_min
# final_df$BF_min_weekly <- data$baseflow_min
# final_df$BF_max_weekly <- data$baseflow_max
# final_df$BF_mean_weekly <- data$baseflow_mean


df <- df %>% 
  select(-contains(".WB_rel")) %>%  # Remove columns containing ".rel"
  rename_with(~ gsub("\\.WB_abs$", "", .x), ends_with(".WB_abs"))

df <- merge(df,data,by="Date")

save(df,file = paste0("data/",station,"/Final_df_",station,"_weekly.RData"))

}



#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 
#### for calculating CWB of the Kienstock Catchment Only ----#### 
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

  load(file = paste0("data/","kienstock","/Meteo_Variables_Weekly_","catchment_kienstock",".RData")) %>% 
    as_tibble()
data <- Meteo_Weekly
 
  WB_week <- data$precipitation-data$rET
  WB_2week <- vector("numeric", length = length(data$WB_week))
  WB_3week <- vector("numeric", length = length(data$WB_week))
  WB_6week <- vector("numeric", length = length(data$WB_week))
  WB_12week <- vector("numeric", length = length(data$WB_week))
  acc_values <- vector("numeric", length = length(data$WB_week))
  
  acc_per_names <- c("WB_1week","WB_2week", "WB_3week", "WB_6week", "WB_12week", "WB_24week","WB_52week") #defines the names of 
  acc_per_value <- c(1,2,3,6,12, 24, 52)#defines the values in days for the accumulation periods whichis later used in the loop
  WB_list <- list()
  
  ################### to iterate over all different acc periods: 
  for (n in 1:length(acc_per_names)) {
    # n <- 1
    acc_per_names_n <- acc_per_names[n]#save the name of the accumulation period as variable
    acc_per_value_n <- acc_per_value[n]#save the value in days of the accumulation period as variable
    
    for (i in seq(acc_per_value_n, length(WB_week))) {
      
      acc_values[i] <- sum(WB_week[(i - acc_per_value_n):i]) #calculates the summed values of over the aggregation period
      
    }
    
    acc_values[acc_values == 0] <- NA
    
    acc_values_relative <- acc_values/acc_per_value_n
    
    results_data <- data.frame(data$date,paste(acc_per_names_n),acc_values,acc_values_relative)
    colnames(results_data) <- c("Date", "Acc_Per", "WB_abs", "WB_rel")
    
    WB_list[[acc_per_names_n]] <- results_data #implementationof that with lists
    
    acc_values <- vector("numeric", length = length(data$WB_day)) #create a "fresh" empty vector to avoid copying values from the previous iteration in the following one
    
    print("finished")  
    
  }
  
  WB_table_long <- do.call(rbind, WB_list)
  WB_table_wide <- do.call(cbind, WB_list)
  
  
  ########## Arrange the final Data set by removing surplus date columns and adding other variable columns ###
  dates <- WB_table_wide$WB_1week.Date
  df <- WB_table_wide %>% #removing surplus date columns 
    select(-contains("Date"))
  df$Date <- dates
  df <- df %>% #removing surplus columns that define the aggregation period
    select(-contains("Acc_Per"))
  
  data <- rename(data,Date = date)
  
  
  df <- df %>% 
    select(-contains(".WB_rel")) %>%  # Remove columns containing ".rel"
    rename_with(~ gsub("\\.WB_abs$", "", .x), ends_with(".WB_abs"))
  
  df <- merge(df,data,by="Date")
  
  save(df,file = paste0("data/","kienstock/Final_df_","catchment_kienstock","_weekly.RData"))
  




















