#### Test models on forecasting on Leons data

library(tidyverse)

library(glmnet)

library(Metrics)

library(caret)

rm(list = ls())

stations_list <- c(
  "tauchenbach",
  "kienstock",
  "flattach",
  "uttendorf")

dataset <- "lagged_TB" #TB = Tauchenbach
# load(file = paste0("data/tauchenbach/models/",dataset,"_weekly_data.RData"))

#create empty lists to store the results of all catchments
results <- list()
forecasts_list <- list()
final_model_list <- list()
n_coefs_list <- list()
coefs_list <- list()
GOF_list <- list()
data <- NULL

for(s in stations_list){
  
  # station <- "tauchenbach"
  
  if(s == "kienstock"){
    load(file =  paste0("data/",s,"/Final_df_","catchment_kienstock","_weekly.RData"))
    df_cat <- df  %>% select(c("Date",          "WB_1week",      "WB_2week",      "WB_3week",      "WB_6week",      "WB_12week",     "WB_24week",    
                               "WB_52week",     "precipitation", "Tmin",          "Tmax",          "sunshine" ,     "snowcover",    
                               "rET"))
    
    colnames(df_cat) <- c("date", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12","cwb24","cwb52",  "prec",
                          
                          "Tmin", "Tmax", "sun", "snow","rET")
    
    df_cat <- df_cat %>%  rename_with(~ paste0(.x, "_cat"), .cols = -date) 
  }
  
  
  load(file =  paste0("data/",s,"/Final_df_",s,"_weekly.RData"))
  
  
  
  
  #### Test models on forecasting on Leons data
  
  # laoding data ----
  
  x <- as_tibble(df)
  
  
  x <- x %>% mutate(year = as.integer(year), month = as.integer(month), 
                    
                    week_year = as.integer(week_year), week_tot = as.integer(week_tot),
                    
                    sin_month = sin(month), cos_month = cos(month),
                    
                    sin_week = sin(week_year), cos_week = cos(week_year))
  
  x <- x %>% dplyr::select(all_of(c("Date","flow_mean", "WB_1week", "WB_2week", "WB_3week", 
                                    
                                    "WB_6week", "WB_12week","WB_24week","WB_52week", "month", "year", "week_year",
                                    
                                    "week_tot","Tmin", "Tmax", "snowcover", "precipitation", "sunshine", "rET", 
                                    
                                    "sin_month", "cos_month", "sin_week", "cos_week" ,"AO_last",       "AO_mean",      
                                    "PNA_last",      "PNA_mean",      "ONI_last",      "ONI_mean",      "SOI_last",      "SOI_mean",      "NAO_last",    
                                    "NAO_mean",        "flow_ma2",      "diff_ma2" ,    
                                    "flow_ma4",      "diff_ma4",      "flow_ma8",      "diff_ma8",      "deriv_1",       "deriv_2",       "flow_spline")))
  
  x <- x %>% drop_na()
  
  
  x <- x %>% rename(date = Date, flow = flow_mean, cwb1 = WB_1week, cwb2 = WB_2week, cwb3 = WB_3week, 
                    
                    cwb6 = WB_6week, cwb12 = WB_12week, cwb24 = WB_24week, cwb52 = WB_52week, month = month, 
                    
                    year = year, week_year = week_year, week_tot = week_tot, Tmin = Tmin, Tmax = Tmax, snow = snowcover,
                    
                    prec = precipitation, sun = sunshine, rET = rET,  sin_month = sin_month, cos_month = cos_month, 
                    
                    sin_week = sin_week, cos_week = cos_week , ao_last = AO_last, ao_mean = AO_mean, pna_last = PNA_last,
                    
                    pna_mean = PNA_mean, oni_last = ONI_last,  oni_mean = ONI_mean, soi_last = SOI_last, soi_mean = SOI_mean,     
                    nao_mean = NAO_mean, flow_ma2 = flow_ma2, diff_ma2 = diff_ma2 ,    nao_last =  NAO_last,   
                    flow_ma4 = flow_ma4,  diff_ma4 = diff_ma4,     flow_ma8 =  flow_ma8,   diff_ma8 =   diff_ma8,      
                    deriv_1 = deriv_1,      deriv_2 = deriv_2,      flow_spline = flow_spline)
  
  
  # if(station == "kienstock"){
  #   x <- left_join(x, df_cat, by = "date")
  # }
  x$station <- s
  
  data <- rbind(x,data)
}
  
data %>% 
  ggplot() +
  geom_point(aes(x = flow, y = c(NA,lag(low_spline),1)))+
  facet_wrap(~station, scales = "free")


