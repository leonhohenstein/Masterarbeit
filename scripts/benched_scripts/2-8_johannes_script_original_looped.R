#### Test models on forecasting on Leons data

library(tidyverse)

library(glmnet)

library(Metrics)

library(caret)



rm(list = ls())
dataset <- "lagged_TB" #TB = Tauchenbach
# load(file = paste0("data/tauchenbach/models/",dataset,"_weekly_data.RData"))
load(file = paste0("data/tauchenbach/Final_df_Tauchenbach_weekly.RData"))


x <- as_tibble(df)

x <- x %>% mutate(year = as.integer(year), month = as.integer(year), 
                  
                  week_year = as.integer(week_year), week_tot = as.integer(week_tot),
                  
                  sin_month = sin(month), cos_month = cos(month),
                  
                  sin_week = sin(week_year), cos_week = cos(week_year))




# Rename variables and select only the ones used for modelling!

x <- x %>% dplyr::select(all_of(c("Date","flow_min", "WB_1week", "WB_2week", "WB_3week", 
                                  
                                  "WB_6week", "WB_12week","WB_24week","WB_52week", "month", "year", "week_year",
                                  
                                  "week_tot","Tmin", "Tmax", "snowcover", "precipitation", "sunshine", "rET", 
                                  
                                  "sin_month", "cos_month", "sin_week", "cos_week" )))

x <- x %>% drop_na()


colnames(x) <- c("date", "flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12","cwb24","cwb52", "month", 
                 
                 "year", "week_year","week_tot", "Tmin", "Tmax", "snow", "prec", "sun", "rET", "sin_month", 
                 
                 "cos_month","sin_week", "cos_week")

# x <- x %>% mutate(flow2 = sqrt(flow))

#in order to actaully use the transformed var (flow2) make it the response variable
#when defining the arguments in the "get_forecasts" function

vn <- c("flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12", "cwb24","cwb52", "month", 
        
        "year", "week_year","week_tot", "Tmin", "Tmax", "snow", "prec", "sun", "rET", "sin_month", "cos_month",
        
        "sin_week", "cos_week")

vn_lagged <- c("flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12", "cwb24","cwb52",
               
               "Tmin", "Tmax", "snow", "prec", "sun", "rET")


calculate_lags2 <- function(df, var, lags) {
  
  map_lag <- lags %>% map(~partial(lag, n = .x))
  
  names(map_lag) <- lags
  
  return(df %>% mutate(across(.cols = all_of(c(var)), .fns = map_lag, .names="{.col}_lag_{.fn}")))
  
}



get_forecast <- function(x, resp, horizon, var_names, filter_year = 2014, var_names_lagged = NULL, lags = NULL, 
                         
                         diff_lag = FALSE, temp_res = c("week"))
  
{
  
  if(diff_lag)
    
  {
    
    x <- x %>% mutate(y = !!as.name(resp) - lead(!!as.name(resp), horizon))
    
  }
  
  else
    
  {
    
    x <- x %>% mutate(y = lead(!!as.name(resp), horizon)) 
    
  }
  
  # Add lagged versions
  
  if(!is.null(lags))
    
  {
    
    x <- calculate_lags2(df = x, var = var_names_lagged, lags = 1:lags)
    
    ### Get additional variable names
    
    var_add <- paste0(paste0(var_names_lagged, "_lag_"), seq(lags))
    
    var_names <- c(var_names, var_add)
    
  }
  
  x <- x %>% drop_na()
  
  xtrain <- x %>% filter(year <= filter_year)
  
  xtest <- x %>% filter(year > filter_year)
  
  nr <- nrow(xtrain)
  
  iw <- floor(nr*0.6)
  
  y <- xtrain$y
  
  X <- as.matrix(xtrain[var_names])
  
  tc <- trainControl(method = "timeslice", initialWindow = iw, fixedWindow = FALSE,
                     
                     horizon = 12, skip = 20)
  
  m <- caret::train(x = X, y = y, method = "glmnet", family = "gaussian", 
                    
                    trControl = tc, tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(from = -1, to = -7, length.out = 150)), 
                    
                    preProc = c("center", "scale"), metric = "RMSE")
  
  pred <- predict(m, xtest)
  
  if(diff_lag)
    
  {
    
    out <- xtest[c("date","y",resp)] %>% mutate(pred = pred)
    
    out <- out %>% mutate(obs = !!as.name(resp) - y, pred = !!as.name(resp) - pred)
    
    out <- out %>% dplyr::select(all_of(c("date", "obs", "pred")))
    
  }
  
  else
    
  {
    
    out <- xtest[c("date", "y")] %>% mutate(pred = pred) %>% rename(obs = y)
    
  }
  
  res <- match.arg(temp_res, c("month", "week", "day"))
  
  # This is necessary, so the forecast has the correct timestamp
  
  out <- switch(res,

                month = out %>% mutate(date = date + months(horizon)),

                week = out %>% mutate(date = date + weeks(horizon)),

                day = out %>% mutate(date = date + days(horizon)))
  
  out$horizon <- horizon
  
  return(list(forecast = out))
  
  out
  
}

p <-
  get_forecast(
    x = x,
    resp = "flow",
    horizon = 2,
    var_names = vn,
    var_names_lagged = vn_lagged,
    lags = 1,
    diff_lag = FALSE
  )


#### forecasting ----
results <- list()

fc_horizons <- c(1:5)
forecasts_list <- list()
final_model_list <- list()
n_coefs_list <- list()
coefs_list <- list()
GOF_list <- list()

for (h in fc_horizons) {
  results[[paste0("fc_horizon_", h)]] <-
    get_forecast(
      x = x,
      resp = "flow",
      horizon = h,
      var_names = vn,
      var_names_lagged = vn_lagged,
      lags = 1,
      diff_lag = FALSE
      
      
    )
  
  forecasts_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$forecast
  
  final_model_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$model
  
  n_coefs_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$n_coefs
  
  coefs_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$coefs
  
  GOF_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$GOF
  
  print(paste0("finished ",h," / ",max(fc_horizons)))
  
}

# name model parameters to make storing the results distinguishable

model_name <- c("EN_weekly_original")

if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}



save(final_model_list, file = paste0("results/",model_name,"/",model_name,"final_model_list.RData"))
save(n_coefs_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"n_coefs_list.RData"))
save(coefs_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
save(forecasts_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_forecasts_list.RData"))
save(GOF_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))

