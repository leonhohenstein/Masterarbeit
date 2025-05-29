#### Test models on forecasting on Leons data

library(tidyverse)

library(glmnet)

library(Metrics)

library(caret)

library(data.table)


rm(list = ls())

# load("~/lagged_TB_data.RData")
load(file = paste0("data/tauchenbach/Final_df_Tauchenbach_monthly.RData"))

x <- as_tibble(df)

x <- x %>% mutate(year = as.integer(year), month = as.integer(month), 
                  
                  sin_month = sin(month), cos_month = cos(month))

x <- x %>% dplyr::select(!all_of(c("BF_min_monthly","BF_max_monthly", "BF_mean_monthly")))

x <- x %>% drop_na()



# Rename variables and select only the ones used for modelling!

x <- x %>% dplyr::select(all_of(c("Date","Flow_min_monthly", "WB_1month", "WB_2month", "WB_3month", 
                                  
                                  "WB_6month", "WB_12month", "month", "year", 
                                  
                                  "T_min_mean_monthly", "T_max_mean_monthly", "snowcover_sum_monthly", 
                                  
                                  "precipitation_sum_monthly", "sunshine_mean_monthly", "ETP_sum_monthly", 
                                  
                                  "sin_month", "cos_month")))



colnames(x) <- c("date", "flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12", "month", 
                 
                 "year", "Tmin", "Tmax", "snow", "rr", "sun", "etp", "sin_month", "cos_month")

x <- x %>% mutate(flow2 = sqrt(flow))

vn <- c("flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12", "month", 
        
        "year", "Tmin", "Tmax", "snow", "rr", "sun", "etp", "sin_month", "cos_month")

vn_lagged <- c("flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12", 
               
               "Tmin", "Tmax", "snow", "rr", "sun", "etp")



calculate_lags2 <- function(df, var, lags) {
  
  map_lag <- lags %>% map(~partial(lag, n = .x))
  
  names(map_lag) <- lags
  
  return(df %>% mutate(across(.cols = all_of(c(var)), .fns = map_lag, .names="{.col}_lag_{.fn}")))
  
}




get_forecast <- function(x, resp, horizon, var_names, filter_year = 2014, var_names_lagged = NULL, lags = NULL, 
                         
                         diff_lag = FALSE, temp_res = c("month"))
  
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
                     
                     horizon = 12, skip = 5)
  
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
  
  return(out)
  
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

fc_horizons <- c(1:3)
forecasts_list <- list()

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
  
  forecasts_list[[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]
  
  print(paste0("finished ",h," / ",max(fc_horizons)))
  
}

forecast_df <- do.call(rbind,results) 

forecast_df %>% 
  as.data.frame() %>% 
  mutate(date = as.Date(date)) %>%  
  # filter(lubridate::year(date) == 2015) %>% 
  
  ggplot(aes(x = date, y = pred, color = factor(horizon))) + 
  geom_line(size = 1.2, alpha = 0.9) +  
  geom_line(aes(y = obs), color = "darkgrey", linetype = "solid", size = 1.4, alpha = 1) +  # Dark grey observed line
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon", 
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: monthly model"),
    subtitle = "Grey line indicates Observed Flow"
  ) +
  theme_light(base_size = 14) +
  theme(
    legend.position = "right",  
    legend.title = element_text(size = 14, face = "bold"),  
    legend.text = element_text(size = 12),  
    axis.text = element_text(size = 12, color = "black"), 
    axis.title = element_text(size = 14, face = "bold"), 
    strip.text = element_text(face = "bold", size = 13),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(size = 12, hjust = 0.5), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "grey85") 
  )

forecast_df_shifted <- forecast_df %>% setDT() %>% 
  .[, .(date, pred = lead(pred,first(horizon)), obs), by = horizon]

forecast_df_shifted %>%
  as.data.frame() %>% 
  mutate(date = as.Date(date)) %>%  
  # filter(lubridate::year(date) == 2015) %>%
  ggplot(aes(x = date, y = pred, color = factor(horizon))) + 
  geom_line(size = 1.2, alpha = 0.9) +  
  geom_line(aes(y = obs), color = "darkgrey", linetype = "solid", size = 1.4, alpha = 1) +  
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Clean legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: monthly model"),
    subtitle = "Grey line indicates Observed Flow"
  ) +
  theme_light(base_size = 14) +
  # geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted")+
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




GOF<- forecast_df %>% na.omit() %>%  setDT() %>% 
  .[,.(r2 = cor(pred,obs)^2,
       rse = 1 - rse(obs, pred),
       rmse(obs, pred)), by = horizon]

GOF_shifted <- forecast_df_shifted %>% na.omit() %>% setDT() %>% 
  .[,.(r2 = cor(pred,obs)^2,
       rse = 1 - rse(obs, pred),
       rmse = rmse(obs, pred)), by = horizon]



p <-
  get_forecast(
    x = x,
    resp = "flow",
    horizon = 3,
    var_names = vn,
    var_names_lagged = vn_lagged,
    lags = 1,
    
    diff_lag = FALSE
  )

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


