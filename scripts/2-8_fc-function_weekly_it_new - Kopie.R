#### Test models on forecasting on Leons data

library(tidyverse)

library(glmnet)

library(Metrics)

library(caret)

rm(list = ls())
dataset <- "lagged_TB" #TB = Tauchenbach
# load(file = paste0("data/tauchenbach/models/",dataset,"_weekly_data.RData"))
load(file = paste0("data/tauchenbach/Final_df_Tauchenbach_weekly.RData"))


#### Test models on forecasting on Leons data

# laoding data ----

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



obs_orig <- x %>%  select(c("date","flow", "week_tot"))



### calculate lags ----
calculate_lags2 <- function(df, var, lags) {
  
  map_lag <- lags %>% map(~partial(lag, n = .x))
  
  names(map_lag) <- lags
  
  return(df %>% mutate(across(.cols = all_of(c(var)), .fns = map_lag, .names="{.col}_lag_{.fn}")))
  
}


### create forecasting function ----

get_forecast <- function(x,
                         resp,
                         horizon,
                         var_names,
                         filter_year = 2014,
                         var_names_lagged = NULL,
                         lags = NULL,
                         diff_lag = FALSE,
                         temp_res = c("week"),
                         stepsize,
                         obj_fnct,
                         transform = NULL,
                         rolling_window = TRUE,
                         iterative = FALSE,fc_prev_its = NULL
)
  
  
{
  
  ### forecasting ----
  
  
  x <- x %>% mutate(y = !!as.name(resp)) 
  
  # if(iterative == T & horizon > 1)
  # {
  #   x <- fc_prev_its %>% filter(horizon == h-1) %>% select(c("date","pred")) %>% 
  #     merge(x,.,by = "date")
  #   
  #   x$flow <- NULL
  #   
  #   x <- x %>% rename(y = pred)
  #   
  #   var_names_lagged <- setdiff(var_names_lagged, resp)
  # }
  
  
  ##### Add Transformation ----
  
  if(transform == "sqrt")
  {
    
    x$y <- sqrt(x$y)
    
  }
  
  if(transform == "log_n")
  {
    
    x$y <- log(x$y)
    
  }
  
  if(transform == "log_10")
  {
    
    x$y <- log10(x$y)
    
  }
  
  
  ##### Add Diff Calculation and shift target variable ----
  
  if(diff_lag)
    
  {
    
    x <- x %>% mutate(y = y - lead(y, horizon))
  
  }
  
  else
    
  {
    
    x <- x %>% mutate(y = lead(y, horizon)) 
    
  }
  
  
  ##### Add Lagged Variables ----
  
  if(!is.null(lags) && !is.null(var_names_lagged))
    
  {
    
    x <- calculate_lags2(df = x, var = var_names_lagged, lags = 1:lags)
    
    ### Get additional variable names
    
    var_add <- paste0(paste0(var_names_lagged, "_lag_"), seq(lags))
    
    var_names <- c(var_names, var_add)
    
  }
  
  ##### Add results from previous iteration as lagged flows ----
  
  iterative_lags <- NULL
  
  if(horizon > 1 &&  !is.null(lags) && iterative %in% c("replace","add"))

  {
    
    for (n in 1 : min(c(( horizon - 1), lags))) {
        # n <- 1
        flow_lag_n <- fc_prev_its %>%
          filter(horizon == (h - n)) %>%
          mutate(!!sym(ifelse(n == 1,
                              "flow_it",
                              paste0("flow_it_lag_", n - 1))) := pred) %>% 
          select(c(ifelse(n == 1,
                        "flow_it",
                        paste0("flow_it_lag_", n - 1)),"date"))
        
        if(n == 1)
        {
          iterative_lags <- flow_lag_n
        }
      
        else 
        {
          iterative_lags <- merge(flow_lag_n,iterative_lags, by = "date")
        }
        
    }
        
        dupl_cols <- setdiff(intersect(names(x),names(iterative_lags)),"date") 

        x <- x %>% 
          select(-all_of(dupl_cols))
        
        x <- merge(x,iterative_lags, by = "date")

    }

  
  if(iterative == "replace")
  {
    x <- x %>% select(-contains(resp))
    
    names <- names(x)
    
    names(x) <- gsub("_it", "", names)
  }
  
  
  #### separate in test and train set ----
  x <- x %>% drop_na()
  
  dates_weeks <- x %>% select(c("date","week_tot"))
  
  var_names <- unique(var_names)
  
  x_all <- as.matrix(x[var_names])
  
  x_all <- x %>%  select(all_of(var_names))
  
  y_all <- x$y
  
  xtrain <- x %>% filter(year <= filter_year)
  
  # xtest <- x %>% filter(year > filter_year)
  
  nr <- nrow(xtrain)
  
  iw <- floor(nr*0.6) #defines initial window for the time slices (at 60 % of the training data)
  
  y_train <- xtrain$y
  
  X_train <- as.matrix(xtrain[var_names])
  
  n_exclude <- 12 #number of timesteps to leave out from the training data after the validation in the training process
  
  if(rolling_window){
    #lets call it "rolling Window" where also 
    
    slices <- createTimeSlices(1:(nrow(xtrain)-horizon),initialWindow = iw,
                               horizon = 1, skip = stepsize, fixedWindow = FALSE)
    
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
  
  else
  
    {
    
    tc <- trainControl(method = "timeslice", initialWindow = iw, fixedWindow = FALSE,
                       
                       horizon = 12, skip = stepsize) 
    #lets call it "growing window where the train data does not include timesteps 
    #after the validation time and ends one step before the test step of the train data set
  }
  
  m <- caret::train(x = X_train, y = y_train, method = "glmnet", family = "gaussian", 
                    
                    trControl = tc, tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(from = -1, to = -7, length.out = 150)), 
                    
                    preProc = c("center", "scale"), metric = obj_fnct)
  
  pred <- predict(m, x_all)
  
  x_all <- as_tibble(x_all)
  
  x_all <- merge(x_all, dates_weeks, by = "week_tot")

  if(diff_lag)
    
  {
    # out <- x_all %>% as.data.frame() %>% .[c("date",!!as.name(resp))] %>% mutate(pred = pred)
    
    out <- x_all[c("date",resp)] %>% mutate(pred = pred)

    out <- out %>% mutate(pred = !!as.name(resp) - pred)
    
    out <- out %>% dplyr::select(all_of(c("date", "pred")))
    
  }
  
  else
    
  {
    
    out <- x_all[c("date")] %>% mutate(pred = pred) 
    
  }
  
  
  if(transform == "sqrt")
  {
    out$pred <- out$pred^2 #davor war es einfach: out <- out$pred^2 aber ich denke nicht dass das richtig ist -> ausprobeiren
  }
  
  if(transform == "log_n")
  {
    out$pred <- exp(out$pred)
  }
  
  if(transform == "log_10")
  {
    out$pred <- 10^out$pred
  }
  
  res <- match.arg(temp_res, c("month", "week", "day"))
  
  # This is necessary, so the forecast has the correct timestamp

  out <- switch(res,

                month = out %>% mutate(date = date + months(horizon)),

                week = out %>% mutate(date = date + weeks(horizon)),

                day = out %>% mutate(date = date + days(horizon)))
  
  out <- x_all %>%  select(c("date",!!as.name(resp))) %>% 
    merge(., out, by = "date")
  
  out <- out %>%  rename(obs = !!as.name(resp))

  out$horizon <- h
  
  fc_prev_its <- rbind(fc_prev_its,out)
  
  out <- out %>% filter(lubridate::year(date) > filter_year)  
  
  #### evaluation and storage of results ----
  
  R2 <- hydroGOF::R2(out$pred, out$obs)
  mNSE <- hydroGOF::mNSE(out$pred, out$obs, j=1)
  kge <- hydroGOF::KGE(out$pred, out$obs, j=1)
  RMSE <- hydroGOF::rmse(out$pred, out$obs)
  
  lambda_opt <- m$bestTune$lambda
  
  GOF <- data.frame(alpha=1, 
                    lambda=lambda_opt,   
                    RMSE=RMSE, 
                    R2=R2,
                    mNSE=mNSE,
                    KGE=kge,
                    fc_horizon = horizon)  
  final_model <- m$finalModel
  coefs <- coef(final_model, s = lambda_opt) %>% 
    as.matrix() %>% 
    as.data.frame()
  n_coefs <- sum(coefs[,1] != 0) %>% 
    as.data.frame() 
  
  assign("fc_prev_its", fc_prev_its, envir = .GlobalEnv)
  
  if(iterative == FALSE) 
    
    {
    iterative_lags <- "Not Available in non-iterative model"
  }
    
  return(list(forecast = out,
              model = final_model,
              n_coefs = n_coefs,
              coefs = coefs,
              GOF = GOF,
              params = list(lags = lags,rolling_window = rolling_window,stepsize = stepsize,
                            temp_res = temp_res,transformation = transform,objc_fnct = obj_fnct),
              fc_prev_its = fc_prev_its,
              iterative_lags = iterative_lags))
  
}


#### forecasting ----

forecasts_list <- list()
final_model_list <- list()
n_coefs_list <- list()
coefs_list <- list()
GOF_list <- list()
results_prev_its <- NULL

fc_horizons <- c(1:5)
results <- list()

for (h in fc_horizons) {
  
  results[[paste0("fc_horizon_",h)]] <-
    get_forecast(
      x = x,
      resp = "flow",
      horizon = h,
      var_names = vn,
      var_names_lagged = vn_lagged,
      lags = 2,
      rolling_window = F, #T für meine, F für Johannes (simple) version
      diff_lag = FALSE,
      temp_res = "week",
      stepsize = 20,
      obj_fnct = "RMSE",
      transform = "sqrt",
      iterative = "add",
      fc_prev_its = results_prev_its  
    )
  
  forecasts_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$forecast
  
  final_model_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$model
  
  n_coefs_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$n_coefs
  
  coefs_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$coefs
  
  GOF_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$GOF
  
  print(paste0("finished ",h," / ",max(fc_horizons)))
  
  results_prev_its <-   rbind(results[[paste0("fc_horizon_",h)]][["fc_prev_its"]],results_prev_its)
  
  results_prev_its <- results_prev_its %>%
    distinct(date, horizon, .keep_all = TRUE)

  
}

# name model parameters to make storing the results distinguishable

model_name <- c("EN_weekly_iterative")

if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}



save(final_model_list, file = paste0("results/",model_name,"/",model_name,"final_model_list.RData"))
save(n_coefs_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"n_coefs_list.RData"))
save(coefs_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
save(forecasts_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_forecasts_list.RData"))
save(GOF_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))

h <- 3
x = x
resp = "flow"
horizon = h
var_names = vn
var_names_lagged = vn_lagged
lags = 2
rolling_window = F #T für meine, F für Johannes version
diff_lag = FALSE
temp_res = "week"
stepsize = 5
obj_fnct = "RMSE"
transform = "sqrt"
iterative = "replace"
filter_year <- 2014
fc_prev_its = NULL
