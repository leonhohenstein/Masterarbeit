#### Test models on forecasting on Leons data

library(tidyverse)

library(glmnet)

library(Metrics)

library(caret)

rm(list = ls())
model_name <- c("EN-TSRW")
dataset <- "lagged_TB" #TB = Tauchenbach
load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))

if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}

#### Test models on forecasting on Leons data

# laoding data ----

x <- as_tibble(df)

x <- x %>% mutate(year = as.integer(year), month = as.integer(year), 
                  
                  sin_month = sin(month), cos_month = cos(month))

x <- x %>% dplyr::select(!all_of(c("BF_min_monthly","BF_max_monthly", "BF_mean_monthly", "Flow_min_monthly_lag1", "Flow_min_monthly_lag2")))

x <- x %>% drop_na()



# Rename variables and select only the ones used for modelling!

x <- x %>% dplyr::select(all_of(c("Date","Flow_min_monthly", "WB_1month.WB_abs", "WB_2month.WB_abs", "WB_3month.WB_abs", 
                                  
                                  "WB_6month.WB_abs", "WB_12month.WB_abs", "month", "year", 
                                  
                                  "T_min_mean_monthly", "T_max_mean_monthly", "snowcover_sum_monthly", 
                                  
                                  "precipitation_sum_monthly", "sunshine_mean_monthly", "ETP_sum_monthly", 
                                  
                                  "sin_month", "cos_month")))



colnames(x) <- c("date", "flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12", "month", 
                 
                 "year", "Tmin", "Tmax", "snow", "rr", "sun", "etp", "sin_month", "cos_month")

x <- x %>% mutate(flow2 = sqrt(flow))
#in order to actaully use the transformed var (flow2) make it the response variable
#when defining the arguments in the "get_forecasts" function

vn <- c("flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12", "month", 
        
        "year", "Tmin", "Tmax", "snow", "rr", "sun", "etp", "sin_month", "cos_month")

vn_lagged <- c("flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12", 
               
               "Tmin", "Tmax", "snow", "rr", "sun", "etp")


 ### calculate lags ----
calculate_lags2 <- function(df, var, lags) {
  
  map_lag <- lags %>% map(~partial(lag, n = .x))
  
  names(map_lag) <- lags
  
  return(df %>% mutate(across(.cols = all_of(c(var)), .fns = map_lag, .names="{.col}_lag_{.fn}")))
  
}


### calculate lags ----

get_forecast <- function(x, resp, horizon, var_names, filter_year = 2014, var_names_lagged = NULL, lags = NULL, 
                         diff_lag = FALSE, temp_res = c("month"),
                         rolling_window = TRUE)
  
{
  
  if(diff_lag)
    
  {
    
    x <- x %>% mutate(y = !!as.name(resp) - lead(!!as.name(resp), horizon))
    #as.name turns the string defined in resp into a symbol
    #!! makes the function evaluate the symbol (insert values stored in it) now 
    #and insert the result into the expression
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
  
  iw <- floor(nr*0.6) #defines initial window for the time slices (at 60 % of the training data)
  
  y <- xtrain$y
  
  X <- as.matrix(xtrain[var_names])
  
  n_exclude <- 12 #number of timesteps to leave out from the training data after the validation in the training process
  
  if(rolling_window){
    #lets call it "rolling Window" where also 
    
    slices <- createTimeSlices(1:(nrow(xtrain)-horizon),initialWindow = iw,
                               horizon = 1, skip = 1, fixedWindow = FALSE)
    
    for (i in seq_along(slices$train)) {
      
      test_set <- slices$test[[i]]    # Get test window
      
      #  range of indices to exclude from training data
      max_test_index <- max(test_set)
      
      exclude_range <- seq(max_test_index,
                           min(max_test_index + n_exclude, nrow(xtrain)),
                           by = 1)
      # include all future data except the excluded range
      
      slices$train[[i]] <- setdiff(1:nrow(xtrain), c(test_set, exclude_range))
      
      tc <- trainControl(method = "cv", index = slices$train)
      
    }
    
  }
  else{
    
    tc <- trainControl(method = "timeslice", initialWindow = iw, fixedWindow = FALSE,
                       
                       horizon = 12, skip = 5) 
    #lets call it "growing window where the train data does not include timesteps 
    #after the validation time and ends one step before the test step of the train data set
  }
  
  
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
  
  out
  
}



p <- get_forecast(x = x, resp = "flow", horizon = 1, var_names = vn, var_names_lagged = vn_lagged, lags = 1, 
                  
                  diff_lag = FALSE)

p %>% summarize(1 - rse(obs, pred), rmse(obs, pred))



ggplot(data = p) + 
  
  geom_line(aes(x = date, y = obs, col = "observation")) + 
  
  geom_point(aes(x = date, y = obs, col = "observation"), shape = 2) +
  
  geom_line(aes(x = date, y = pred, col = "prediction")) + 
  
  geom_point(aes(x = date, y = pred, col = "prediction"), shape = 2) +
  
  scale_colour_manual(values = c("observation" = 2, "prediction" = 3)) + 
  
  theme_bw()





ggplot(data = p) + 
  
  geom_line(aes(x = date, y = obs - pred)) + 
  
  theme_bw()



