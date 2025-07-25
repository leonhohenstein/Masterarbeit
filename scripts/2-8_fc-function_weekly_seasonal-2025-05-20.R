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

x <- x %>% mutate(year = as.integer(year), month = as.integer(month), 
                  
                  week_year = as.integer(week_year), week_tot = as.integer(week_tot),
                  
                  sin_month = sin(month), cos_month = cos(month),
                  
                  sin_week = sin(week_year), cos_week = cos(week_year))

x <- x %>% dplyr::select(all_of(c("Date","flow_min", "WB_1week", "WB_2week", "WB_3week", 
                                  
            "WB_6week", "WB_12week","WB_24week","WB_52week", "month", "year", "week_year",
            
            "week_tot","Tmin", "Tmax", "snowcover", "precipitation", "sunshine", "rET", 
            
            "sin_month", "cos_month", "sin_week", "cos_week" ,"ao_last",       "ao_mean",      
             "pna_last",      "pna_mean",      "ONI_last",      "ONI_mean",      "SOI_last",      "SOI_mean",      "NPGO_last",    
             "NPGO_mean",        "flow_ma2",      "diff_ma2" ,    
             "flow_ma4",      "diff_ma4",      "flow_ma8",      "diff_ma8",      "deriv_1",       "deriv_2",       "flow_spline")))

x <- x %>% drop_na()



colnames(x) <- c("date", "flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12","cwb24","cwb52", "month", 
                 
                 "year", "week_year","week_tot", "Tmin", "Tmax", "snow", "prec", "sun", "rET",
                 "sin_month",   "cos_month","sin_week", "cos_week",
                 "ao_last",
                 "ao_mean",       "pna_last",      "pna_mean",      "ONI_last",      "ONI_mean",      
                 "soi_last",      "soi_mean",      "npgo_last",   "npgo_mean",      
                 "flow_ma2",      "diff_ma2" , "flow_ma4",      
                 "diff_ma4",      "flow_ma8",      "diff_ma8",      "deriv_1",       "deriv_2", 
                 "flow_spline" )

# x <- x %>% mutate(flow2 = sqrt(flow))

#in order to actaully use the transformed var (flow2) make it the response variable

#when defining the arguments in the "get_forecasts" function

vn <- c("flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12","cwb24","cwb52", "month", 
        
        "year", "week_year","week_tot", "Tmin", "Tmax", "snow", "prec", "sun", "rET",
        "sin_month",   "cos_month","sin_week", "cos_week",
        "ao_last",
        "ao_mean",       "pna_last",      "pna_mean",      "ONI_last",      "ONI_mean",      
        "soi_last",      "soi_mean",      "npgo_last",   "npgo_mean",      
        "flow_ma2",      "diff_ma2" , "flow_ma4",      
        "diff_ma4",      "flow_ma8",      "diff_ma8",      "deriv_1",       "deriv_2", 
        "flow_spline" )

vn_lagged <- c("flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12", "cwb24","cwb52",
               
               "Tmin", "Tmax", "snow", "prec", "sun", "rET", "flow_spline", "deriv_1", "deriv_2")

vn_interaction <- c("date", "flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12","cwb24","cwb52", "month", 
                    
                    "year", "week_year","week_tot", "Tmin", "Tmax", "snow", "prec", "sun", "rET",
                    "sin_month",   "cos_month","sin_week", "cos_week",
                    "ao_last",
                    "ao_mean",       "pna_last",      "pna_mean",      "ONI_last",      "ONI_mean",      
                    "soi_last",      "soi_mean",      "npgo_last",   "npgo_mean",      "flow_min",
                    "flow_ma2",      "diff_ma2" , "flow_ma4",      
                    "diff_ma4",      "flow_ma8",      "diff_ma8",      "deriv_1",       "deriv_2", 
                    "flow_spline" )


### calculate lags ----
calculate_lags2 <- function(df, var, lags) {
  
  map_lag <- lags %>% map(~partial(lag, n = .x))
  
  names(map_lag) <- lags
  
  return(df %>% mutate(across(.cols = all_of(c(var)), .fns = map_lag, .names="{.col}_lag_{.fn}")))
  
}

#----------#----------------------------------------------------------------

### create forecasting function ----

get_forecast <- function(x, resp, horizon, var_names, filter_year = 2014, var_names_lagged = NULL, lags = NULL, 
                         diff_lag = FALSE, temp_res = c("week"), stepsize, obj_fnct, transform = NULL,
                         rolling_window = TRUE, seasonality = FALSE, vn_interaction = NULL)
  
  
{
  
  #### forecasting ----
  
  x <- x %>% mutate(y = !!as.name(resp)) 
  
  #### Add Seasonality ----
  
  if(seasonality == T)
  {
    x <- x %>%
    mutate(season = case_when(
      month %in% c(6:8) ~ "summer",
      month %in% c(1:5,9:12) ~ "winter",
      TRUE ~ "other"
    ))
    x$season <- as.factor(x$season)
  }
  
  #### Add Transformation ----
  
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
  
  
  if(diff_lag)
    
  {
    
    x <- x %>% mutate(y = y - lead(y, horizon))
    #as.name turns the string defined in resp into a symbol
    #!! makes the function evaluate the symbol (insert values stored in it) now 
    #and insert the result into the expression
  }
  
  else
    
  {
    
    x <- x %>% mutate(y = lead(y, horizon)) 
    
  }
  
  
  # Add lagged versions
  
  if(!is.null(lags) && !is.null(var_names_lagged))
    
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
  else{
    
    tc <- trainControl(method = "timeslice", initialWindow = iw, fixedWindow = FALSE,
                       
                       horizon = 12, skip = stepsize) 
    #lets call it "growing window where the train data does not include timesteps 
    #after the validation time and ends one step before the test step of the train data set
  }
  
  if(seasonality == T)
    
  {
    
    xtrain <- xtrain %>%  select(-"date")

    predictors <- setdiff(names(xtrain), c("y", "season"))  # avoid y and season * season
    
    interaction_terms <- paste(predictors, "* season", collapse = " + ")
    
    formula <- as.formula(paste("y ~", interaction_terms, "+ season"))
    
    
    m <- caret::train(
      # formula = formula,  # Use the generated formula
      form = formula,
      data = xtrain,      # Training data
      method = "glmnet",  # Elastic net
      family = "gaussian",
      trControl = tc,
      tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(from = -1, to = -7, length.out = 150)),
      preProc = c("center", "scale"),
      metric = obj_fnct
    )
  }
  
  else
    
  {
  m <- caret::train(x = X, y = y, method = "glmnet", family = "gaussian", 
                    
                    trControl = tc, tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(from = -1, to = -7, length.out = 150)), 
                    
                    preProc = c("center", "scale"), metric = obj_fnct)
  }
  
  pred <- predict(m, xtest)
  
  if(diff_lag)
  {
    y_now <- xtest[[resp]]  # current y_t
    
    if(transform == "log_n") {
      pred <- exp(log(y_now) - pred)  # y_{t+h} = exp(log(y_t) - diff)
    } else if(transform == "log_10") {
      pred <- 10^(log10(y_now) - pred)
    } else if(transform == "sqrt") {
      pred <- (sqrt(y_now) - pred)^2
    } else {
      pred <- y_now - pred
    }
    
    out <- xtest[c("date")] %>% mutate(pred = pred)
  }
  
  else
    
  {
    
    out <- xtest[c("date")] %>% mutate(pred = pred)
    
  }
  
  if(!diff_lag) {
    
    if(transform == "sqrt")
    {
      out$pred <- out$pred^2
    }
    
    if(transform == "log_n")
    {
      out$pred <- exp(out$pred)
    }
    
    if(transform == "log_10")
    {
      out$pred <- 10^out$pred
    }
  }
  res <- match.arg(temp_res, c("month", "week", "day"))
  
  # This is necessary, so the forecast has the correct timestamp
  
  
  out <- switch(res, 
                
                month = out %>% mutate(date = date + months(horizon)), 
                
                week = out %>% mutate(date = date + weeks(horizon)), 
                
                day = out %>% mutate(date = date + days(horizon)))
  
  out <- xtest %>%  select(c("date",!!as.name(resp))) %>% 
    
    merge(., out, by = "date")
  
  out <- out %>%  rename(obs = !!as.name(resp))
  
  out$horizon <- h
  
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
  
  return(list(forecast = out,
              model = final_model,
              n_coefs = n_coefs,
              coefs = coefs,
              GOF = GOF,
              params = list(lags = lags,rolling_window = rolling_window,stepsize = stepsize,
                            temp_res = temp_res,transformation = transform,objc_fnct = obj_fnct)))
  
}


#### forecasting ----
results <- list()

fc_horizons <- c(1:16)
forecasts_list <- list()
final_model_list <- list()
n_coefs_list <- list()
coefs_list <- list()
GOF_list <- list()

for (h in fc_horizons) {
  
  results[[paste0("fc_horizon_",h)]] <-
    get_forecast(
      x = x,
      resp = "flow",
      horizon = h,
      var_names = vn,
      var_names_lagged = vn_lagged,
      lags = 1,
      rolling_window = F, #T für meine, F für Johannes version
      diff_lag = F,
      temp_res = "week",
      stepsize = 50,
      obj_fnct = "RMSE",
      transform = "sqrt",
      seasonality = T
    )
  
  forecasts_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$forecast
  
  final_model_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$model
  
  n_coefs_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$n_coefs
  
  coefs_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$coefs
  
  GOF_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$GOF
  
  print(paste0("finished ",h," / ",max(fc_horizons)))
  
}

# name model parameters to make storing the results distinguishable

model_name <- c("EN_weekly_season_lag_1_sqrt")

if (file.exists(paste0("results/2025-05-20/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/2025-05-20/",model_name,"/")))
}

save(final_model_list, file = paste0("results/2025-05-20/",model_name,"/",model_name,"final_model_list.RData"))
save(n_coefs_list,file = paste0("results/2025-05-20/",model_name,"/",model_name,"_",dataset,"n_coefs_list.RData"))
save(coefs_list,file = paste0("results/2025-05-20/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
save(forecasts_list,file = paste0("results/2025-05-20/",model_name,"/",model_name,"_",dataset,"_forecasts_list.RData"))
save(GOF_list,file = paste0("results/2025-05-20/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))


# h <- 1
# x = x
# resp = "flow"
# horizon = h
# var_names = vn
# var_names_lagged = vn_lagged
# lags = 2
# rolling_window = F #T für meine, F für Johannes version
# diff_lag = FALSE
# temp_res = "week"
# stepsize = 5
# obj_fnct = "RMSE"
# transform = "log10"
# seasonality = T
# filter_year <- 2014


forecast_df <- do.call(rbind,forecasts_list)

forecast_df %>% 
  filter(lubridate::year(date) == 2015) %>% 
  
  ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast colored by horizon
  geom_line(size = 1.2, alpha = 0.9) +  # Thicker, slightly transparent forecast lines
  geom_line(aes(y = obs), color = "darkgrey", linetype = "solid", size = 1.4, alpha = 1) +  # Dark grey observed line
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Clean legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name),
    subtitle = "Grey line indicates Observed Flow"
  ) +
  theme_light(base_size = 14) +
  geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted")+
  # Clean light theme with larger fonts
  theme(
    legend.position = "right",  # Keep legend on the right
    legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.text = element_text(size = 12, color = "black"),  # Larger axis labels
    axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
    strip.text = element_text(face = "bold", size = 13),  # If faceting is added later
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered, bold title
    plot.subtitle = element_text(size = 12, hjust = 0.5),  # Centered subtitle
    panel.grid.minor = element_blank(),  # Clean minor grids
    panel.grid.major = element_line(color = "grey85")  # Subtle major grid
  )





