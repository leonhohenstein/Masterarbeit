library(tidyverse)
library(ggplot2)
library(glmnet)
# library(fable)
library(tsibble)
library(data.table)
library(caret)

rm(list=ls())

############################################################################
#################### ELASTIC NET OPTIMIZATION  SCRIPT ######################
############################################################################


### model parameters ----

model_name <- c("EN-TSRW-diff-dir")
dataset <- "lagged_TB" #TB = Tauchenbach
lagged <- TRUE #define if lagged variables of the detrended variable should be calculated too
n_step <- 1 #defines the number of rows which are added to the training set in each iteration
n_lambdas <- 30
set.seed(42) # set a seed to ensure that random numbers generation and test/training procedures are reproducible
origin_ratio <- 0.6 #defines where the Time-Series Cross validation starts to run (e.g. for 0.6 the first TS-CV fold starts with the first 60% of the training data and each now fold builds upon that)
n_test_years <- 6
lambda_grid <- 10^seq(-3, 2, length = 50)
alpha_grid <- 1
fc_horizon <- c(1,2,3)
n_exclude <- 12

if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}

### loading data ----

pred_list <- list()
GOF_fc_list <- list()
coefs_list <- list()
slices_list <- list()
models_list <- list()
coefs_list_fc <- list()
best_tunes_list <- list()

for (h in fc_horizon) {
  # h <- 3
  
  #### preparing data ----
  
  load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))
  
  pred_eval  <- df %>% #removing surplus date columns
    select(c( "Flow_min_monthly","Date","year"))
  
  origin <- (nrow(df)*origin_ratio) %>% 
    ceiling()#defines the first row of the dataset to be used as testing data
  
  
  df <- df %>% #removing surplus date columns
    select(-ends_with(c( "BF_min_monthly","BF_max_monthly","BF_mean_monthly","rel")))
  
  df$Flow_min_monthly_lag2 <- df$Flow_min_monthly_lag1
  df$Flow_min_monthly_lag1 <- df$Flow_min_monthly

  df$flow_obs_diffs_h <- c(diff(df$Flow_min_monthly,lag = h),rep(NA, h)) #creates the difference between the current timestemp and the timnestamp to be forecasted (h)
  df$flow_obs_diff_h1_lag1 <- c(diff(df$Flow_min_monthly,lag = 1),rep(NA, 1))
  df$flow_obs_diff_h1_lag1 <- dplyr::lag(df$flow_obs_diff_h1_lag1, 1)
  df$flow_obs_diff_h1_lag2 <- dplyr::lag(df$flow_obs_diff_h1_lag1, 1)

  df$flow_obs_diff_h2_lag2 <- c(diff(df$Flow_min_monthly,lag = 2),rep(NA, 2))
  df$flow_obs_diff_h2_lag2 <- dplyr::lag(df$flow_obs_diff_h2_lag2, 2)
  df$flow_obs_diff_h2_lag3 <- dplyr::lag(df$flow_obs_diff_h2_lag2, 1)

  df$flow_obs_diff_h3_lag3 <- c(diff(df$Flow_min_monthly,lag = 3),rep(NA, 3))
  df$flow_obs_diff_h3_lag3 <- dplyr::lag(df$flow_obs_diff_h3_lag3, 3)
  df$flow_obs_diff_h3_lag4 <- dplyr::lag(df$flow_obs_diff_h3_lag3, 1)
  
  df <- na.omit(df)
  df$month <- as.numeric(df$month)
  df$year <- as.numeric(df$year)
  df$Date <- as.Date(df$Date)
  df$month_hy <- (df$month - 10) %% 12 + 1
  df$month_sin <- sin(2*pi*df$month_hy /12)
  df$month_cos <- cos(2*pi*df$month_hy /12)
  
  all_years <- unique(df$year)
  
  pred_eval$Date <- as.Date(pred_eval$Date) 
  pred_eval <-  pred_eval %>% setDT %>%
    .[year >= all_years[length(all_years)-n_test_years],]
 
  dates_eval <- df %>% 
    setDT %>% 
    .[year >= all_years[length(all_years)-n_test_years],]
  
  dates_eval_vars <- dates_eval$Date
  
  dates_eval_pred <- dplyr::lead(dates_eval$Date, h)
  
  rm(dates_eval)
  
  df$Date <- as.numeric(1:nrow(df))
  
  df_eval <- df %>% 
    setDT %>% 
    .[year >= all_years[length(all_years)-n_test_years],]
  
  df_train <- df %>% 
    setDT %>% 
    .[year < all_years[length(all_years)-n_test_years],] %>% 
    select(-"Flow_min_monthly")
  
  x_train <- df_train %>% 
    as.data.frame() %>% 
    select(-c("flow_obs_diffs_h")) 
  
  x_eval <- df_eval %>% 
    as.data.frame() %>% 
    select(-c("Flow_min_monthly","flow_obs_diffs_h")) 
  
  backtotrans_eval <- df %>% 
    setDT() %>% 
    .[month == x_eval$month[1] & year == x_eval$year[1]]
  
  x_eval <- x_eval %>% 
    data.matrix() #saving the input data as matrix in order to make it work for cv.glmnet()
  
  y_train <- df_train %>% 
    pull(flow_obs_diffs_h) 
  
  y_obs <- df_eval %>% 
    pull(Flow_min_monthly) %>% 
    as.data.frame()
  
  ################# fitting the model -----
  
  #manually creating time slices
  
  
  
  slices <- createTimeSlices(1:(nrow(x_train)-h),initialWindow = origin,
                             horizon = 1, skip = 1, fixedWindow = FALSE)
  for (i in seq_along(slices$train)) {
    set.seed(13)
    
    test_set <- slices$test[[i]]    # Get test window
    
    #  range of indices to exclude from training data
    max_test_index <- max(test_set)
    exclude_range <- seq(max_test_index,
                         min(max_test_index + n_exclude, nrow(x_train)),
                         by = 1)
    # include all future data except the excluded range
    slices$train[[i]] <- setdiff(1:nrow(x_train), c(test_set, exclude_range))
  }
  
  slices_list[[paste0("horizon-",h)]] <- slices #save the slices of each horizon in the list
  
  
  
  #training the model:
  
  TC <- trainControl(method = "cv", index = slices$train)
  
  model <- caret::train(flow_obs_diffs_h ~ .,
                        data = df_train,
                        method = "glmnet",
                        family = "gaussian",
                        trControl = TC,
                        tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda_grid),
                        preProc = c("center", "scale"),
                        metric="RMSE")
  # metric = caret::RMSE # Ensure caret's RMSE is used
  
  models_list[[paste0("horizon_",h)]] <- model
  
  
  
  #### predict on unseen out-of sample test data -------
  
  n_fill_na <- nrow(pred_eval) - nrow(x_eval)
  pred_eval$pred <- c(predict(model, newdata = x_eval),rep(NA,n_fill_na)) %>%
    as.vector()
  # pred_eval$pred <- dplyr::lag(pred_eval$pred, h)        #verwenden, ja nein?
  pred_eval$year <- NULL
  colnames(pred_eval) <- c("y_obs","Date","pred_diff")
  
  
  pred_eval$pred_BT <- NA
  for (i in (h+1):nrow(pred_eval)) {

        pred_eval$pred_BT[i]  <- pred_eval$pred_diff[i-h]  + pred_eval$y_obs[i-h]

  }
  
  pred_eval$horizon <- h
  # pred_eval$Date <- pred_eval$Date_pred   #this is just to make the naming of columns consistent for the analysis script and plotting
  pred_eval$pred <- pred_eval$pred_BT #this is just to make the naming of columns consistent for the analysis script and plotting
  pred_list[[paste0("horizon_",h)]] <- pred_eval
    pred_eval$res <- pred_eval$y_obs-pred_eval$pred_BT
  pred_eval$year <- lubridate::year(pred_eval$Date)
  

  pred_eval %>% 
    setDT() %>% 
    .[year == 2017] 
  
  ggplot(pred_eval %>% filter(year(Date) == 2017))
    
  ggplot(pred_eval)+
    geom_line(aes(x=Date, y = pred, color = "predicted"))+
    geom_line(aes(x=Date, y = y_obs, color = "observed"))
    
    geom_line(aes(x=Date, y = pred_diff, color = "pred_diff"))+
    geom_line(aes(x=Date, y = c(diff(y_obs, n = 3), NA), color = "y_obs_diff"))
    
  
  hydroGOF::R2(pred_eval$pred, pred_eval$y_obs)
  
  #now shift the results one more time (which i think is not necesssary and not right) 
  pred_eval$pred <- dplyr::lead(pred_eval$pred, n = h)
  ggplot(data = pred_eval)+
    geom_line(aes(x=Date, y = pred, color = "darkred"))+
    geom_line(aes(x=Date, y = y_obs, color = "steelblue"))
  hydroGOF::R2(pred_eval$pred, pred_eval$y_obs)
  
  ###extract coefficients ------------------------------------------------------
  
  model <- models_list[[paste0("horizon_",h)]]
  final_model <- model$finalModel
  
  model[["results"]]$alpha <- as.factor(model[["results"]]$alpha)
  
  GOF_df <- model[["results"]]
  
  n_coefs <- vector()
  final_model <- model$finalModel
  lambda_values <- model$bestTune[["lambda"]]
  lambda_names <- paste0("lambda_",1:length(lambda_values))
  lambda_opt_train <- GOF_df$lambda[which.min(GOF_df$RMSE)]
  
  for (i in 1:length(lambda_values)) {
    
    coefs_temp <- coef(final_model, s = lambda_values[i]) %>% 
      as.matrix() %>% 
      as.data.frame()
    n_coefs[i] <- sum(coefs_temp[,1] != 0)
    
    coefs_list[[paste0(lambda_names[i])]] <- coefs_temp
  }
  coefs_df <- do.call(cbind,coefs_list)
  n_coefs <- n_coefs %>% 
    as.data.frame() 
  
  #### predict also for other lambdas to check for overfitting ----
  
  
  # y_obs <- pred_list[[paste0("horizon_",h)]][[paste0("y_obs")]]
  # dates <- pred_list[[paste0("horizon_",h)]][[paste0("Dates")]]
  # 
  # pred_eval_lambdas <- matrix(data = NA, nrow= length(y_obs), ncol = length(lambda_values))
  # 
  # preproc <- model$preProcess
  # 
  # x_eval_standard <- predict(preproc, x_eval) %>% # Apply the same scaling & centering as in the model training
  #   as.matrix()
  # 
  # for (i in 1:length(lambda_values)) {
  #   # i <- 3
  #   pred_eval_lambdas[,i] <- predict(final_model, newx = x_eval_standard, s = lambda_values[i], type = "response")
  #   
  #   for (n in (h+1):nrow(pred_eval)) {
  #     
  #     pred_eval_lambdas[n,i]  <- pred_eval_lambdas[n,i]  + pred_eval$y_obs[n-h]
  #     
  #   }
  #   pred_eval_lambdas[,i] <- dplyr::lead(pred_eval_lambdas[,i], h)
  #   
  # }
  # 
  # pred_eval_lambdas <- cbind(y_obs ,pred_eval_lambdas) %>% 
  #   as.data.frame()
  #  
  #  
  # # pred_eval_lambdas$y_obs <- dplyr::lag(pred_eval_lambdas$y_obs, n = h)
  # pred_eval_lambdas$Date <- dates_eval_vars
  # 
  # pred_eval_lambdas <- pred_eval_lambdas[-(1:h), ]
  # 
  # colnames(pred_eval_lambdas) <- c("y_obs",paste0("lambda_",1:length(lambda_values)),"Date")
  # 
  # GOF_list <- list()
  
  #### calculating GOF values for each fc-period ----

#### Calculate GOF measures ----------------------------------------------------
  GOF_list <- list()
  y_pred <- pred_eval$pred

  y_obs <- pred_eval$y_obs

  R2 <- hydroGOF::R2(y_pred, y_obs)
  mNSE <- hydroGOF::mNSE(y_pred, y_obs, j=1)
  kge <- hydroGOF::KGE(y_pred, y_obs, j=1)
  RMSE <- hydroGOF::rmse(y_pred, y_obs)

  # Store the GOF results
  temp.GOF <- data.frame(alpha=1,
                         # lambda=lambda,
                         RMSE=RMSE,
                         R2=R2,
                         mNSE=mNSE,
                         KGE=kge
  )

  GOF_list[["lambda_n"]] <- temp.GOF
  alpha_opt_train <- GOF_df$alpha[which.min(GOF_df$RMSE)]
  lambda_values <- lambda_opt_train
  GOF_df_eval <- do.call(rbind,GOF_list)
  GOF_df_eval$lambda.value <- lambda_values
  rm(GOF_list)
  GOF_df_eval$n_coefs <- n_coefs %>%
    unlist()
  GOF_df_eval$fc_horizon <- h
  GOF_fc_list[[paste0("horizon_",h)]] <- GOF_df_eval
  coefs_list_fc[[paste0("fc_horizon_",h)]] <- coefs_df
  print(paste0("iteration ",h,"/",max(fc_horizon)," finished"))
  
  
}


  
#### save results --------------------------------------------------------------
save(fc_horizon, file = paste0("results/",model_name,"/",model_name,"_fc_horizon.RData"))

save(models_list, file = paste0("results/",model_name,"/",model_name,"_model_fit.RData"))
save(coefs_list_fc,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
save(pred_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_list.RData"))
save(best_tunes_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_best_tune.RData"))
save(GOF_fc_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))

