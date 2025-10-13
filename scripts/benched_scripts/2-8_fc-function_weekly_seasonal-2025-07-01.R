#### Test models on forecasting on Leons data

library(tidyverse)

library(glmnet)

library(Metrics)

library(caret)

rm(list = ls())

stations_list <- c(
  "Tauchenbach",
  "Kienstock",
  "Flattach",
  "Uttendorf")

# load(file = paste0("data/tauchenbach/models/",dataset,"_weekly_data.RData"))

#create empty lists to store the results of all catchments
results <- list()
forecasts_list <- list()
final_model_list <- list()
n_coefs_list <- list()
coefs_list <- list()
GOF_list <- list()


for(station in stations_list){
  
  # station <- "Kienstock"
  
  if(station == "Kienstock"){
    load(file =  paste0("data/",station,"/Final_df_","Kienstock_catchment","_weekly.RData"))
    
    
  
  df <- df %>% rename(date = Date, cwb1 = WB_1week, cwb2 = WB_2week, cwb3 = WB_3week, 
                    
                    cwb6 = WB_6week, cwb12 = WB_12week, cwb24 = WB_24week, cwb52 = WB_52week, 
                    
                    Tmin = Tmin, Tmax = Tmax, prec = precipitation, sun = sunshine, rET = rET, snow_melt = snow_melted, snow_add = snow_added,
                    
                    swe_add4 = swe_added_agg_4, swe_melt4 = swe_melted_agg_4, swe_add8 = swe_added_agg_8, swe_melt8 = swe_melted_agg_8,
                    
                    snow_add4 = snow_added_agg_4, snow_melt4 = snow_melted_agg_4, snow_add8 = snow_added_agg_8, snow_melt8 = snow_melted_agg_8,
  
                    )
  
  cat_vars <- c("date",          "cwb1",      "cwb2",      "cwb3",      "cwb6",      "cwb12",     "cwb24",    
                    "cwb52",     "prec", "Tmin",          "Tmax",          "sun" ,     "snow_depth",    
                    "rET",  "swe_tot","snow_add","snow_melt","swe_added","swe_melted",
                    "swe_add4","swe_melt4","snow_add4","snow_melt4",
                    "swe_add8","swe_melt8","snow_add8","snow_melt8")
  df_cat <- df  %>% select(cat_vars)
  df_cat <- df_cat %>%  rename_with(~ paste0(.x, "_cat"), .cols = -date) 
  
  }
  
  
  load(file =  paste0("data/",station,"/Final_df_",station,"_weekly.RData"))
  
  
  #### Test models on forecasting on Leons data
  
  # laoding data ----
  
  x <- as_tibble(df)
  
  
  x <- x %>% mutate(year = as.integer(year), month = as.integer(month), 
                    
                    week_year = as.integer(week_year), week_tot = as.integer(week_tot),
                    
                    sin_month = sin(month), cos_month = cos(month),
                    
                    sin_week = sin(week_year), cos_week = cos(week_year))
  
  x <- x %>% dplyr::select(all_of(c("Date","flow_mean", "WB_1week", "WB_2week", "WB_3week", 
                                    
                                    "WB_6week", "WB_12week","WB_24week","WB_52week", "month", "year", "week_year",
                                    
                                    "week_tot","Tmin", "Tmax", "snow_depth", "precipitation", "sunshine", "rET", 
                                    
                                    "sin_month", "cos_month", "sin_week", "cos_week" ,"AO_last",       "AO_mean",      
                                    "PNA_last",      "PNA_mean",      "ONI_last",      "ONI_mean",      "SOI_last",      "SOI_mean",      "NAO_last",    
                                    "NAO_mean",        "flow_ma2",      "diff_ma2" ,    
                                    "flow_ma4",      "diff_ma4",      "flow_ma8",      "diff_ma8",      "deriv_1",       "deriv_2",       "flow_spline",
                                    "swe_tot","snow_added","snow_melted","swe_added","swe_melted",
                                    "swe_added_agg_4","swe_melted_agg_4","snow_added_agg_4","snow_melted_agg_4",
                                    "swe_added_agg_8","swe_melted_agg_8","snow_added_agg_8","snow_melted_agg_8"  )))
  
  x <- x %>% drop_na()
  
  
  x <- x %>% rename(date = Date, flow = flow_mean, cwb1 = WB_1week, cwb2 = WB_2week, cwb3 = WB_3week, 
                    
                    cwb6 = WB_6week, cwb12 = WB_12week, cwb24 = WB_24week, cwb52 = WB_52week, month = month, 
                   
                    year = year, week_year = week_year, week_tot = week_tot, Tmin = Tmin, Tmax = Tmax,
                    
                    prec = precipitation, sun = sunshine, rET = rET,  sin_month = sin_month, cos_month = cos_month, 
                    
                    sin_week = sin_week, cos_week = cos_week , ao_last = AO_last, ao_mean = AO_mean, pna_last = PNA_last,
                    
                    pna_mean = PNA_mean, oni_last = ONI_last,  oni_mean = ONI_mean, soi_last = SOI_last, soi_mean = SOI_mean,     
                    nao_mean = NAO_mean, flow_ma2 = flow_ma2, diff_ma2 = diff_ma2 ,    nao_last =  NAO_last,   
                    flow_ma4 = flow_ma4,  diff_ma4 = diff_ma4,     flow_ma8 =  flow_ma8,   diff_ma8 =   diff_ma8,      
                    deriv_1 = deriv_1,      deriv_2 = deriv_2,      flow_spline = flow_spline)

  
  vn <- setdiff(names(x),c("date","nr.rm"))
  
  if(station == "Kienstock"){
    x <- left_join(x, df_cat, by = "date")
  }
  
 
  #when defining the arguments in the "get_forecasts" function
  # 
  # vn <- c("flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12","cwb24","cwb52", "month", 
  #         
  #         "year", "week_year","week_tot", "Tmin", "Tmax", "snow_depth", "prec", "sun", "rET",
  #         "sin_month",   "cos_month","sin_week", "cos_week",
  #         "ao_last",
  #         "ao_mean",       "pna_last",      "pna_mean",      "oni_last",      "oni_mean",      
  #         "soi_last",      "soi_mean",      "nao_last",   "nao_mean",      
  #         "flow_ma2",      "diff_ma2" , "flow_ma4",      
  #         "diff_ma4",      "flow_ma8",      "diff_ma8",      "deriv_1",       "deriv_2", 
  #         "flow_spline" )
  
  
  # setdiff(vn,vn_new)
  
  if(station == "Kienstock"){
    vn <- c(vn, setdiff(names(df_cat),"date"))
  }
  
  vn_lagged <- c("flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12", "cwb24","cwb52",
                 
                 "Tmin", "Tmax", "snow_depth", "prec", "sun", "rET", "flow_spline", "deriv_1", "deriv_2",
                 "swe_tot","snow_added","snow_melted","swe_added","swe_melted",
                 "swe_added_agg_4","swe_melted_agg_4","snow_added_agg_4","snow_melted_agg_4",
                 "swe_added_agg_8","swe_melted_agg_8","snow_added_agg_8","snow_melted_agg_8")
  
  vn_interaction <- c("date", "flow", "cwb1", "cwb2", "cwb3", "cwb6", "cwb12","cwb24","cwb52", "month",
                      
                      "year", "week_year","week_tot", "Tmin", "Tmax", "snow_depth", "prec", "sun", "rET",
                      "sin_month",   "cos_month","sin_week", "cos_week",
                      "ao_last",
                      "ao_mean",       "pna_last",      "pna_mean",      "ONI_last",      "ONI_mean",
                      "soi_last",      "soi_mean",      "npgo_last",   "npgo_mean",      "flow_min",
                      "flow_ma2",      "diff_ma2" , "flow_ma4",
                      "diff_ma4",      "flow_ma8",      "diff_ma8",      "deriv_1",       "deriv_2",
                      "flow_spline" )
  
  
  if(station == "Kienstock"){
    vn_interaction <- intersect(vn_interaction, cat_vars) %>% 
      paste0(.,"_cat") %>% 
      c(vn_interaction,.)
    
    vn_lagged <- intersect(vn_lagged, cat_vars) %>% 
      paste0(.,"_cat") %>% 
      c(vn_lagged,.)
    
  }
  
  ### create customized summary function for CV-Evaluation ----
  
  customSummary <- function(data, lev = NULL, model = NULL) {
    mae_val <- MAE(data$obs, data$pred)
    rmse_val <- RMSE(data$obs, data$pred)
    mse_val <- mean((data$obs - data$pred)^2)
    
    out <- c(MAE = mae_val, RMSE = rmse_val, MSE = mse_val)
    return(out)
  }
  
    ### calculate lags ----
  calculate_lags2 <- function(df, var, lags) {
    
    map_lag <- lags %>% map(~partial(lag, n = .x))
    
    names(map_lag) <- lags
    
    return(df %>% mutate(across(.cols = all_of(c(var)), .fns = map_lag, .names="{.col}_lag_{.fn}")))
    
  }
  
  
  # CREATE FORECASTING FUNCTION ----
  
  get_forecast <- function(x, resp, horizon, var_names, test_years, var_names_lagged = NULL, lags = NULL, 
                           diff_lag = FALSE, temp_res = c("week"), stepsize, obj_fnct, transform = NULL,
                           rolling_window = TRUE, seasonality = FALSE, vn_interaction = NULL,
                           lambda_opt = "lambda.min", SE.factor = 1, interaction = "automatic")
    
    
  {
    
    #### forecasting ----
    
    x <- x %>% mutate(y = !!as.name(resp)) 
    
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
      
      x <- x %>% mutate(y = y - lead(y, horizon))
      #as.name turns the string defined in resp into a symbol
      #!! makes the function evaluate the symbol (insert values stored in it) now 
      #and insert the result into the expression
    }
    
    else
      
    {
      
      x <- x %>% mutate(y = lead(y, horizon)) 
      
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
    
    xtrain <- x %>% filter(!year %in% test_years)
    
    xtest <- x %>% filter(year %in% test_years)
    
    nr <- nrow(xtrain)
    
    iw <- floor(nr*0.6) #defines initial window for the time slices (at 60 % of the training data)
    
    y <- xtrain$y
    
    X <- as.matrix(xtrain[var_names])
    
    n_exclude <- 52 #number of timesteps to leave out from the training data after the validation in the training process
    
    
    
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
        
        tc <- trainControl(method = "cv", index = slices$train, summaryFunction = customSummary,
                           savePredictions = "final")
        
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
      
      tc <- trainControl(method = "cv", index = slices$train, summaryFunction = customSummary,
                         savePredictions = "final")
      
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
        tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(from = 0, to = -5, length.out = 150)),
        preProc = c("center", "scale"),
        metric = obj_fnct
      )
    }
    
    else
      
    {
      m <- caret::train(x = X, y = y, method = "glmnet", family = "gaussian", 
                        
                        trControl = tc, tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(from = 0, to = -5, length.out = 150)), 
                        
                        preProc = c("center", "scale"), metric = obj_fnct)
    }
    
    global_optimization_results <- m$results
# 
#     global_optimization_results %>% mutate(MAE_UB = MAE + MAESD) %>%
#       ggplot()+
#       geom_line(aes(x=log10(lambda), y = MAE))+
#       geom_col(aes(x=log10(lambda), y = MAE_UB), alpha = 0.5)
# 
#     global_optimization_results %>% mutate(MAE_UB = MAE + MAESD) %>%
#       ggplot()+
#       geom_line(aes(x=log10(lambda), y = MAE))+
#       geom_col(aes(x=log10(lambda), y = MAE_UB), alpha = 0.5)
# 
#     global_optimization_results %>% mutate(MSE_UB = MSE + MSESD) %>%
#       ggplot()+
#       geom_line(aes(x=log10(lambda), y = MSE))+
#       geom_col(aes(x=log10(lambda), y = MSE_UB), alpha = 0.5)
# 
#     test <- global_optimization_results %>% mutate(RMSE_div = RMSESD/RMSE, MAE_div = MAESD/MAE,MSE_div = MSESD/MSE)
#     test %>%
#       ggplot+
#       geom_line(aes(x = log10(lambda), y = RMSE_div, color = "steelblue"))+
#       geom_line(aes(x = log10(lambda), y = MAE_div, color = "red"))+
#       geom_line(aes(x = log10(lambda), y = MSE_div, color = "green"))
# 
# 
#     
    
    
    
    #### extraction of lambdas ----
    
    lambda.min <- m$results$lambda[which.min(m$results$MSE)]
    
    # threshold_1 <- min(m$results$MAE) + (m$results$MAESD[1]* SE.factor) #labelled as SD but actaully represents the Standard Error
    
    threshold <- min(m$results$MSE) + ((m$results$MSESD[which.min(m$results$MSE)] /sqrt(k)) * SE.factor) #labelled as SD but actaully represents the Standard Error
    
    temp <- m$results[m$results$MSE <= threshold,]
    
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
    MAE <- hydroGOF::mae(out$pred, out$obs)
    
    lambda_opt <- m$bestTune$lambda
    
    GOF <- data.frame(alpha=1, 
                      lambda=lambda,   
                      MAE=MAE, 
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
                              lambda_opt = lambda_opt, lambda_sel = lambda, SE.factor = SE.factor, num_CV_folds = k),
                global_optimization_results = global_optimization_results))
    
  }
  
  # name model parameters to make storing the results distinguishable
  
  model_name <- c("test_MSE_SE_1")
  
  #### forecasting ----

  fc_horizons <- c(1:12)
  
  test_years_loop <- 2015:2021
  
  for (h in fc_horizons) {
    
    results[[paste0(station)]][[paste0("fc_horizon_",h)]] <-
      get_forecast(
        x = x,
        resp = "flow",
        horizon = h,
        var_names = vn,
        var_names_lagged = vn_lagged,
        lags = 1,
        rolling_window = T, #T für meine, F für Johannes version
        diff_lag = T,
        temp_res = "week",
        stepsize = 50,
        obj_fnct = "MSE",
        transform = "sqrt",
        seasonality = F,
        test_years = test_years_loop,
        lambda_opt = "lambda.min", #"lambda.1se" oder "lambda.min",
        SE.factor = 1,
        interaction = "automatic"
      )
    
    forecasts_list[[paste0(station)]][[paste0("fc_horizon_",h)]] <- results[[paste0(station)]][[paste0("fc_horizon_",h)]]$forecast
    
    final_model_list[[paste0(station)]][[paste0("fc_horizon_",h)]] <- results[[paste0(station)]][[paste0("fc_horizon_",h)]]$model
    
    n_coefs_list[[paste0(station)]][[paste0("fc_horizon_",h)]] <- results[[paste0(station)]][[paste0("fc_horizon_",h)]]$n_coefs
    
    coefs_list[[paste0(station)]][[paste0("fc_horizon_",h)]] <- results[[paste0(station)]][[paste0("fc_horizon_",h)]]$coefs
    
    GOF_list[[paste0(station)]][[paste0("fc_horizon_",h)]] <- results[[paste0(station)]][[paste0("fc_horizon_",h)]]$GOF
    
    print(paste0("finished ",h," / ",max(fc_horizons)," station ",station))
    
  }
  
 
}  

today <- Sys.Date()


if (file.exists(paste0("results/",today,"/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/")))
}

if (file.exists(paste0("results/",today,"/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/")))
}
test_years_text <- paste0(min(test_years_loop),"_",max(test_years_loop))

if (file.exists(paste0("results/",today,"/",model_name,"/",test_years_text,"/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/",test_years_text,"/")))
}


  save(final_model_list, file = paste0("results/",today,"/",model_name,"/",test_years_text,"/",model_name,"_final_model_list_",test_years_text,".RData"))
  save(n_coefs_list,file = paste0("results/",today,"/",model_name,"/",test_years_text,"/",model_name,"_n_coefs_list_",test_years_text,".RData"))
  save(coefs_list,file = paste0("results/",today,"/",model_name,"/",test_years_text,"/",model_name,"_coefs_list_",test_years_text,".RData"))
  save(forecasts_list,file = paste0("results/",today,"/",model_name,"/",test_years_text,"/",model_name,"_forecasts_list_",test_years_text,".RData"))
  save(GOF_list,file = paste0("results/",today,"/",model_name,"/",test_years_text,"/",model_name,"_GOF_list_",test_years_text,".RData"))
  save(results,file = paste0("results/",today,"/",model_name,"/",test_years_text,"/",model_name,"_results_",test_years_text,".RData"))



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
obj_fnct = "MAE"
transform = "sqrt"
seasonality = T
filter_year <- 2014
lambda_opt = "lambda.1se" #"lambda.1se" oder "lambda.min"
SE.factor = 1
seasonality = F