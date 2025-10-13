#### Upper Benchmark model 
library(data.table)

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

#create empty lists to store the results of all catchments
results <- list()
forecasts_list <- list()
final_model_list <- list()
n_coefs_list <- list()
coefs_list <- list()
GOF_list <- list()

# laoding data ----

for(station in stations_list){
  
  
  if(station == "kienstock"){
    load(file =  paste0("data/",station,"/Final_df_","catchment_kienstock","_weekly.RData"))
    
     df_cat <- df  %>% select(c("Date",          "WB_1week",      "WB_2week",      "WB_3week",      "WB_6week",      "WB_12week",     "WB_24week",    
                             "WB_52week",     "precipitation", "Tmin",          "Tmax",          "sunshine" ,     "snowcover",    
                             "rET"))
  
  colnames(df_cat) <- c("date", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12","cwb24","cwb52",  "prec",
                        
                        "Tmin", "Tmax", "sun", "snow","rET")
  
  df_cat <- df_cat %>%  rename_with(~ paste0(.x, "_cat"), .cols = -date)
  }
 
  
  load(file =  paste0("data/",station,"/Final_df_",station,"_weekly.RData"))
  
x <- as_tibble(df)

x <- x %>% mutate(year = as.integer(year), month = as.integer(month), 
                  
                  week_year = as.integer(week_year), week_tot = as.integer(week_tot),
                  
                  sin_month = sin(month), cos_month = cos(month),
                  
                  sin_week = sin(week_year), cos_week = cos(week_year))

x <- x %>% dplyr::select(all_of(c("Date","flow_mean", "WB_1week", "WB_2week", "WB_3week", 
                                  
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

if(station == "kienstock"){
  x <- left_join(x, df_cat, by = "date")
}


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

if(station == "kienstock"){
  vn <- c(vn, setdiff(names(df_cat),"date"))
}

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

if(station == "kienstock"){
  vn_interaction <- c(vn_interaction, c("cwb1_cat",  "cwb2_cat",  "cwb3_cat",  "cwb6_cat",  "cwb12_cat", "cwb24_cat", "cwb52_cat", "prec_cat",
                                        "Tmin_cat",  "Tmax_cat",  "sun_cat",   "snow_cat" , "rET_cat"  ))
}



### calculate lags ----
calculate_lags2 <- function(df, var, lags) {
  
  map_lag <- lags %>% map(~partial(lag, n = .x))
  
  names(map_lag) <- lags
  
  return(df %>% mutate(across(.cols = all_of(c(var)), .fns = map_lag, .names="{.col}_lag_{.fn}")))
  
}


# CREATE FORECASTING FUNCTION ----

get_forecast <- function(x, resp, horizon, var_names, filter_year = 2014, var_names_lagged = NULL, lags = NULL, 
                         diff_lag = FALSE, temp_res = c("week"), stepsize, obj_fnct, transform = NULL,
                         rolling_window = TRUE, seasonality = FALSE, vn_interaction = NULL,
                         lambda_opt = "lambda.min", SE.factor = 1, interaction = "automatic")
  
  
{
  
  #### forecasting ----
  x <- x %>% mutate(y = !!as.name(resp))
  
  # Select all variables that contain "flow" OR "diff" in their name
  flow_vars <- names(x)[grepl("flow|diff|deriv", names(x))]
  
  # Apply the lag to all of them at once
  x[flow_vars] <- lapply(x[flow_vars], dplyr::lag, n = horizon)
  
  #### Add Seasonality ----
  
  if(seasonality == T)
  {
    x <- x %>%
      mutate(season = case_when(
        month %in% c(3:11) ~ "summer",
        month %in% c(12,1,2) ~ "winter",
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
  
  #### Add Diff Lags ----
  
  if(diff_lag)
    
  {
    
    x <- x %>% mutate(y = y - lead(y, 1))
    #as.name turns the string defined in resp into a symbol
    #!! makes the function evaluate the symbol (insert values stored in it) now 
    #and insert the result into the expression
  }
  
  else
    
  {
    x <- x %>% mutate(y = y) 
    
    # x <- x %>% mutate(y = lead(y, horizon)) #in other version, the target variable is in the future
    
  }
  
  
  #### Add Lagged Vars  ----
  
  if(!is.null(lags) && !is.null(var_names_lagged))
    
  {
    
    x <- calculate_lags2(df = x, var = var_names_lagged, lags = 1:lags)
    
    ### Get additional variable names
    
    var_add <- paste0(paste0(var_names_lagged, "_lag_"), seq(lags))
    
    var_names <- c(var_names, var_add)
    
  }
  
 
  
  #### Separate Train and Test Data ----
  
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
    
    slices <- createTimeSlices(
      1:nrow(xtrain),
      initialWindow = iw,
      horizon = 12,         
      skip = stepsize,
      fixedWindow = rolling_window  # TRUE for rolling, FALSE for growing
    )
    
    tc <- trainControl(method = "cv", index = slices$train)
    
    # tc <- trainControl(method = "timeslice", initialWindow = iw, fixedWindow = FALSE,
    #                    
    #                    horizon = 12, skip = stepsize) 
    
    #lets call it "growing window where the train data does not include timesteps 
    #after the validation time and ends one step before the test step of the train data set
  }
  
  k <- length(tc[["index"]])
  #### GLOBAL OPTIMIZATION OF LAMBDA ----
  
  if(seasonality == T)
    
  {
    
    # xtrain <- xtrain %>%  select(-"date")
    
    predictors <- setdiff(names(xtrain), c("y", "season"))  # avoid y and season * season
    
    interaction_terms <- paste(predictors, "* season", collapse = " + ")
    
    formula <- as.formula(paste("y ~", interaction_terms, "+ season"))
    if(interaction == "manual"){
      formula <- interactions_manual
    }
    
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
  
  global_optimization_results <- m$results
  
  #### extraction of lambdas ----
  
  lambda.min <- m$results$lambda[which.min(m$results$RMSE)]
  
  # threshold_1 <- min(m$results$RMSE) + (m$results$RMSESD[1]* SE.factor) #labelled as SD but actaully represents the Standard Error
  
  threshold <- min(m$results$RMSE) + ((m$results$RMSESD[which.min(m$results$RMSE)] /sqrt(k)) * SE.factor) #labelled as SD but actaully represents the Standard Error
  
  temp <- m$results[m$results$RMSE <= threshold,]
  
  lambda.1se <- temp$lambda[which.max(temp$lambda)]
  
  lambda <- ifelse(lambda_opt == "lambda.1se",lambda.1se,lambda.min)  
  
  ##### LOCAL OPTIMIZATION OF VARIABLE / PREDICTOR COEFFICIENTS for fixed lambda
  
  
  fold_coefs <- list()  # to store coefficient vectors
  
  for (i in seq_along(slices$train)) {
    
    train_idx <- slices$train[[i]]
    xtrain_fold <- xtrain[train_idx, ]
    
    if (seasonality) {
      xtrain_fold <- xtrain_fold 
      # %>% select(-"date")
      predictors <- setdiff(names(xtrain_fold), c("y", "season"))
      interaction_terms <- paste(predictors, "* season", collapse = " + ")
      formula <- as.formula(paste("y ~", interaction_terms, "+ season"))
      
      if(interaction == "manual"){
        formula <- interactions_manual
      }
      
      
      m_fold <- caret::train(
        form = formula,
        data = xtrain_fold,
        method = "glmnet",
        family = "gaussian",
        trControl = trainControl(method = "none"),
        tuneGrid = expand.grid(alpha = 1, lambda = lambda),
        preProc = c("center", "scale"),
        metric = obj_fnct
      )
      
    } else {
      X_fold <- as.matrix(xtrain_fold[, var_names])
      y_fold <- xtrain_fold$y
      
      m_fold <- caret::train(
        x = X_fold,
        y = y_fold,
        method = "glmnet",
        family = "gaussian",
        trControl = trainControl(method = "none"),
        tuneGrid = expand.grid(alpha = 1, lambda = lambda),
        preProc = c("center", "scale"),
        metric = obj_fnct
      )
    }
    
    # Extract coefficients from the glmnet model for this fold
    coef_vector <- as.vector(coef(m_fold$finalModel, s = lambda))
    names(coef_vector) <- rownames(coef(m_fold$finalModel, s = lambda))
    fold_coefs[[paste0("Fold_", i)]] <- coef_vector
  }
  
  coefs <- do.call(cbind, fold_coefs) %>% as.data.frame()
  
  coef_means <- rowMeans(coefs)
  coef_sds <- apply(coefs, 1, sd)
  
  #### Retrain the Final Model for lambda.1se -----
  
  if(lambda_opt == "lambda.1se")
    
  {
    
    if(seasonality == T)
      
    {
      
      # xtrain <- xtrain %>%  select(-"date")
      
      predictors <- setdiff(names(xtrain), c("y", "season"))  # avoid y and season * season
      interaction_terms <- paste(predictors, "* season", collapse = " + ")
      formula <- as.formula(paste("y ~", interaction_terms, "+ season"))
      
      if(interaction == "manual"){
        formula <- interactions_manual
      }
      
      
      m <- caret::train(
        # formula = formula,  # Use the generated formula
        form = formula,
        data = xtrain,      # Training data
        method = "glmnet",  # Elastic net
        family = "gaussian",
        trControl = tc,
        tuneGrid = expand.grid(alpha = 1, lambda = lambda),
        preProc = c("center", "scale"),
        metric = obj_fnct
      )
    }
    
    else
      
    {
      m <- caret::train(x = X, y = y, method = "glmnet", family = "gaussian", 
                        
                        trControl = tc, tuneGrid = expand.grid(alpha = 1, lambda = lambda), 
                        
                        preProc = c("center", "scale"), metric = obj_fnct)
    }
  }
  
  
  
  
  ##### PREDICTION WITH SELECTED LAMBDA FOR ALL CV - FOLDS----
  
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
  
  
  # out <- switch(res, 
  #               
  #               month = out %>% mutate(date = date + months(horizon)), 
  #               
  #               week = out %>% mutate(date = date + weeks(horizon)), 
  #               
  #               day = out %>% mutate(date = date + days(horizon)))
  
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
                    lambda=lambda,   
                    RMSE=RMSE, 
                    R2=R2,
                    mNSE=mNSE,
                    KGE=kge,
                    fc_horizon = horizon)  
  
  final_model <- m$finalModel
  
  coefs <- coef(final_model, s = lambda) %>% 
    as.matrix() %>% 
    as.data.frame()
  n_coefs <- sum(coefs[,1] != 0) %>% 
    as.data.frame() 
  
  # lambdas <- data.frame(lambda.1se = lambda.1se, lambda.min = lambda.min, lambda_best_tune = lambda_opt, lambda_used = lambda)
  
  return(list(forecast = out,
              model = final_model,
              n_coefs = n_coefs,
              coefs = coefs,
              GOF = GOF,
              coef_means = coef_means,
              coef_sds = coef_sds,
              params = list(lags = lags,rolling_window = rolling_window,stepsize = stepsize,
                            temp_res = temp_res,transformation = transform,objc_fnct = obj_fnct, 
                            lambda_opt = lambda_opt, SE.factor = SE.factor, num_CV_folds = k),
              global_optimization_results = global_optimization_results))
  
}


#### forecasting ----

fc_horizons <- c(1:12)

for (h in fc_horizons) {
  
  results[[paste0("fc_horizon_",h)]] <-
    get_forecast(
      x = x,
      resp = "flow",
      horizon = h,
      var_names = vn,
      var_names_lagged = NULL, # sonst vn_lagged,
      lags = 2,
      rolling_window = F, #T für meine, F für Johannes version
      diff_lag = F,
      temp_res = "week",
      stepsize = 50,
      obj_fnct = "RMSE",
      transform = "sqrt",
      seasonality = T,
      lambda_opt = "lambda.min", #"lambda.1se" oder "lambda.min",
      SE.factor = 1,
      interaction = "automatic"
    )
  
  forecasts_list[[paste0(station)]][[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$forecast
  
  final_model_list[[paste0(station)]][[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$model
  
  n_coefs_list[[paste0(station)]][[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$n_coefs
  
  coefs_list[[paste0(station)]][[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$coefs
  
  GOF_list[[paste0(station)]][[paste0("fc_horizon_",h)]] <- results[[paste0("fc_horizon_",h)]]$GOF
  
  print(paste0("finished ",h," / ",max(fc_horizons)," station ",station))
  
  
}

# name model parameters to make storing the results distinguishable

model_name <- c("Climate_Upper_Benchmark_lambdamin")
today <- Sys.Date()

# if (file.exists(paste0("results/",today,"/",model_name,"/coefficients/"))){
# } else {
#   dir.create(file.path(paste0("results/",today,"/",model_name,"/coefficients/")))
# }

if (file.exists(paste0("results/",today,"/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/")))
}

if (file.exists(paste0("results/",today,"/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/")))
}

save(final_model_list, file = paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_final_model_list.RData"))
save(n_coefs_list,file = paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"n_coefs_list.RData"))
save(coefs_list,file = paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
save(forecasts_list,file = paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_forecasts_list.RData"))
save(GOF_list,file = paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))
save(results,file = paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"results.RData"))
}


h <- 1
resp = "flow"
horizon = h
var_names = vn
var_names_lagged = vn_lagged
lags = 1
rolling_window = F #T für meine, F für Johannes version
diff_lag = FALSE
temp_res = "week"
stepsize = 50
obj_fnct = "RMSE"
transform = "sqrt"
seasonality = T
filter_year <- 2014
lambda_opt = "lambda.1se" #"lambda.1se" oder "lambda.min"
SE.factor = 1
interaction = "automatic"




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





