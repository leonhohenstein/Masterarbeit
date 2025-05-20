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

#note that the iterative model takes into account the most current predicted values form the previous iteration
# but it still uses the original climate data of timepoint t0 despite that (e.g. t2 -> climate (t0), lagged flows t1 and t0)


### model parameters ----

model_name <- c("EN-TSRW-sqrt-it")
dataset <- "lagged_TB" #TB = Tauchenbach
lagged <- TRUE #define if lagged variables of the detrended variable should be calculated too
n_step <- 1 #defines the number of rows which are added to the training set in each iteration
n_lambdas <- 30
set.seed(42) # set a seed to ensure that random numbers generation and test/training procedures are reproducible
origin_ratio <- 0.6 #defines where the Time-Series Cross validation starts to run (e.g. for 0.6 the first TS-CV fold starts with the first 60% of the training data and each now fold builds upon that)
n_test_years <- 6
lambda_grid <- 10^seq(-3, 2, length = 50)
alpha_grid <- 1
fc_horizon <- c(0,1,2,3)
n_exclude <- 12

if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}

### loading data and square root transformation (sqrt) ----

pred_list <- list()
GOF_fc_list <- list()
coefs_list <- list()
slices_list <- list()
models_list <- list()
coefs_list_fc <- list()
best_tunes_list <- list()

for (h in fc_horizon) {
  
  #loading data ----
    # h <- 1

  load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))
  
  
  origin <- (nrow(df)*origin_ratio) %>% 
    ceiling()#defines the first row of the dataset to be used as testing data
  
  
  df <- df %>% #removing surplus date columns
    select(-ends_with(c( "BF_min_monthly","BF_max_monthly","BF_mean_monthly","rel")))
  df <- df %>% 
    select(-contains(".WB_rel")) %>%  # Remove columns containing ".rel"
    rename_with(~ gsub("\\.WB_abs$", "", .x), ends_with(".WB_abs"))
  df$Flow_min_monthly <-   dplyr::lead(df$Flow_min_monthly, h)   #move the target variable upwards for h rows to allow a prediction of a previous month with variables of the current time point the forecasting horizont matches the forecasted month
  
  df <- na.omit(df)
  df$month <- as.numeric(df$month)
  df$year <- as.numeric(df$year)
  df$Date <- as.Date(df$Date)
  
  df$month_hy <- (df$month - 10) %% 12 + 1
  df$month_sin <- sin(2*pi*df$month_hy /12)
  df$month_cos <- cos(2*pi*df$month_hy /12)
  
  all_years <- unique(df$year)
  
  if(h > 0){
    
      df$Flow_min_monthly_lag1 <- pred_all[1:(length(pred_all)-1)]

      df$Flow_min_monthly_lag2 <- lag_flow_1_previous[1:(length(pred_all)-1)]
    
        }
  df <- na.omit(df)
  dates_eval <- df %>% 
    setDT %>% 
    .[year >= all_years[length(all_years)-n_test_years],]
  
  dates_eval_vars <- dates_eval$Date
  
  dates_eval_pred <- dplyr::lead(dates_eval$Date, h)
  
  rm(dates_eval)
  
  df$Date <- as.numeric(1:nrow(df))
  
  x <- df %>% 
    as.data.frame() %>% 
    select(-c("Flow_min_monthly")) 
  
  df_eval <- df %>% 
    setDT %>% 
    .[year >= all_years[length(all_years)-n_test_years],]
  
  df_train <- df %>% 
    setDT %>% 
    .[year < all_years[length(all_years)-n_test_years],]
  
  x_train <- df_train %>% 
    as.data.frame() %>% 
    select(-c("Flow_min_monthly")) 
  
  x_eval <- df_eval %>% 
    as.data.frame() %>% 
    select(-c("Flow_min_monthly")) %>%
    data.matrix() #saving the input data as matrix in order to make it work for cv.glmnet()
  
  
  y_train <- df_train %>% 
    pull(Flow_min_monthly) 
  
  y_obs <- df_eval %>% 
    pull(Flow_min_monthly) %>%
    as.matrix()
  
  #### square root transformation -----
  df_train_sqrt <- df_train
  df_train_sqrt[,c("Flow_min_monthly","Flow_min_monthly_lag1","Flow_min_monthly_lag2")] <- lapply(df_train[,c("Flow_min_monthly","Flow_min_monthly_lag1","Flow_min_monthly_lag2")],sqrt)
  
  x_train_sqrt <- df_train_sqrt %>% 
    as.data.frame() %>% 
    select(-c("Flow_min_monthly")) 
  
  x_eval_sqrt <- df_eval
  x_eval_sqrt[,c("Flow_min_monthly","Flow_min_monthly_lag1","Flow_min_monthly_lag2")] <- lapply(x_eval_sqrt[,c("Flow_min_monthly","Flow_min_monthly_lag1","Flow_min_monthly_lag2")],sqrt)
  
  x_eval_sqrt <- x_eval_sqrt %>% 
    as.data.frame() %>% 
    select(-c("Flow_min_monthly")) 
  
  x_eval_sqrt <- x_eval_sqrt %>% 
    data.matrix() #saving the input data as matrix in order to make it work for cv.glmnet()
  
  
  y_train_sqrt <- df_train %>% 
    pull(Flow_min_monthly) %>% 
    sqrt()
  
  y_train_original <- df_train %>% 
    pull(Flow_min_monthly)
  
  y_obs <- df_eval %>% 
    pull(Flow_min_monthly) %>%
    as.matrix()
  
  x$Flow_min_monthly_lag1 <- sqrt(x$Flow_min_monthly_lag1)
  x$Flow_min_monthly_lag2 <- sqrt(x$Flow_min_monthly_lag2)
  
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
    slices$train[[i]] <- setdiff(1:nrow(x_train_sqrt), c(test_set, exclude_range))
  }
  
  slices_list[[paste0("horizon-",h)]] <- slices #save the slices of each horizon in the list
  
  
  
  #training the model:
  
  TC <- trainControl(method = "cv", index = slices$train)
  
  model <- caret::train(Flow_min_monthly ~ .,
                        data = df_train_sqrt,
                        method = "glmnet",
                        family = "gaussian",
                        trControl = TC,
                        tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda_grid),
                        preProc = c("center", "scale"),
                        metric="RMSE")
  # metric = caret::RMSE # Ensure caret's RMSE is used
  
  models_list[[paste0("horizon_",h)]] <- model
  
  
  
  #### Analyse GOF measures for the validation data -------
  
  model <- models_list[[paste0("horizon_",h)]]
  final_model <- model$finalModel
  
  model[["results"]]$alpha <- as.factor(model[["results"]]$alpha)
  
  GOF_df_train <- model[["results"]]
  
  # plotting a generic gof plot for different lambdas ----
  
  # png(filename = paste0("results/",model_name,"/",model_name,"_RMSE_vs_Lambda_h_",h,".png"),
  #     width = 1000, height = 600, units = "px")
  # 
  # ggplot(GOF_df, aes(x = log10(lambda), y = RMSE, color = alpha, group = alpha)) +
  #   geom_line() +                           # Line for each alpha
  #   geom_point() +                          # Points on each line for clarity
  #   scale_color_brewer(palette = "Set1") +   # Distinct colors for each alpha
  #   labs(
  #     x = "Log of Lambda",
  #     y = "RMSE",
  #     color = "Alpha",
  #     title = paste0("Model Performance: RMSE vs Log(Lambda) Model: ",model_name, "FC-Horizon = ",h),
  #     subtitle = "Different Alpha Values Represented as Distinct Colors"
  #   ) +
  #   theme_minimal() +
  #   theme(legend.position = "bottom")
  # dev.off()
  
  #### predict on unseen out-of sample test data -------
  pred_eval <- data.frame()
  pred_eval <- cbind(y_obs,dates_eval_pred) %>% 
    as.data.frame()
  # 
  # test <- predict(model, newdata = x_eval) %>% 
  #   as.data.frame() 
  
  
  pred_eval$pred <- predict(model, newdata = x_eval_sqrt) %>% 
    as.vector() %>% 
    .^2
  
  pred_eval$pred <- dplyr::lead(pred_eval$pred, h)
  
  # pred_eval <- cbind(pred_eval, y_obs,dates_eval)
  
  colnames(pred_eval) <- c("y_obs","Date","pred")
  
  
  # colnames(pred_eval) <- c(paste0("pred_",h),"y_obs","Date")
  
  pred_eval$horizon <- h
  pred_eval$Date <- dates_eval_pred
  pred_list[[paste0("horizon_",h)]] <- pred_eval
  
  
  ###extract coefficients ----
  
  n_coefs <- vector()
  final_model <- model$finalModel
  lambda_values <- final_model$lambda[seq(1,length(final_model$lambda),5)]
  lambda_names <- paste0("lambda_",1:length(lambda_values))
  
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
  
  
  y_obs <- pred_list[[paste0("horizon_",h)]][[paste0("y_obs")]]
  dates <- pred_list[[paste0("horizon_",h)]][[paste0("Dates")]]
  
  pred_eval_lambdas <- matrix(data = NA, nrow= length(y_obs), ncol = length(lambda_values))
  
  preproc <- model$preProcess
  
  x_eval_standard <- predict(preproc, x_eval_sqrt) %>% # Apply the same scaling & centering as in the model training
    as.matrix()
  
  for (i in 1:length(lambda_values)) {
    
    pred_eval_lambdas[,i] <- predict(final_model, newx = x_eval_standard, s = lambda_values[i], type = "response")
    pred_eval_lambdas[,i] <- dplyr::lead(pred_eval_lambdas[,i], h)
    pred_eval_lambdas[,i] <- pred_eval_lambdas[,i]^2
    
  }
  
  
  pred_eval_lambdas <- cbind(y_obs ,pred_eval_lambdas) %>% 
    as.data.frame()
  colnames(pred_eval_lambdas) <- c("y_obs",paste0("lambda_",1:length(lambda_values)))
  pred_eval_lambdas$Date <- dates
  
  GOF_list <- list()
  
  #### calculating GOF values for each fc-period ----
  
  for (lambda in lambda_names) {
    
    temp_predictions_df <- pred_eval_lambdas %>% 
      select(contains(c("y_obs", paste0(lambda)))) 
    
    y_pred <- temp_predictions_df %>% 
      pull(paste0(lambda))
    
    y_obs <- temp_predictions_df %>% pull("y_obs")
    
    R2 <- hydroGOF::R2(y_pred, y_obs)
    mNSE <- hydroGOF::mNSE(y_pred, y_obs, j=1)
    kge <- hydroGOF::KGE(y_pred, y_obs, j=1)
    RMSE <- hydroGOF::rmse(y_pred, y_obs)
    
    # Store the GOF results
    temp.GOF <- data.frame(alpha=1, 
                           lambda=lambda,   
                           RMSE=RMSE, 
                           R2=R2,
                           mNSE=mNSE,
                           KGE=kge
    )
    
    GOF_list[[paste0(lambda)]] <- temp.GOF
  }
  
  
  GOF_df_eval <- do.call(rbind,GOF_list)
  GOF_df_eval$lambda.value <- lambda_values
  GOF_df_eval$lambda_name <- lambda_names
  rm(GOF_list)
  GOF_df_eval$n_coefs <- n_coefs %>% 
    unlist()
  GOF_df_eval$fc_horizon <- h
  GOF_fc_list[[paste0("horizon_",h)]] <- GOF_df_eval
  #### selecting and plotting for optimal GOF values ----
  
  GOF_df_eval <- GOF_df_eval %>%
    setDT() %>% 
    # .[R2>0] %>% 
    mutate(scaled_RMSE = (RMSE - min(RMSE)) / (max(RMSE) - min(RMSE)) * (max(R2) - min(R2)) + min(R2))
  
  GOF_df_eval$fc_horizon <- h
  
  lambda_opt_train <- GOF_df_train$lambda[which.min(GOF_df_train$RMSE)]
  alpha_opt_train <- GOF_df_train$alpha[which.min(GOF_df_train$RMSE)]
  
  #### plot without bars ----
  
  png(filename = paste0("results/",model_name,"/",model_name,"_Optimal_Lambda_R2_RMSE-fc-horizon_",h,".png"),
      width = 1000, height = 600, units = "px")
  plot <- GOF_df_eval %>%
    setDT() %>%
    .[R2>0] %>%
    
    ggplot(aes(x=log10(lambda.value))) +
    geom_point(aes(y=RMSE, color = "Scaled RMSE")) +
    geom_line(aes(y=RMSE, color = "Scaled RMSE"), alpha=0.2, linewidth=1.2) +
    geom_vline(aes(xintercept = log10(lambda_opt_train), color = "Validation Set"), linetype = "solid") +
    geom_vline(aes(xintercept = log10(GOF_df_eval$lambda.value[which.max(GOF_df_eval$R2)]), color = "Test Set"), linetype = "solid") +
    geom_vline(aes(xintercept = log10(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)]), color = "Test Set"), linetype = "solid") +
    
    scale_x_continuous(
      name = expression(log[10](lambda)),
      breaks = pretty(log10(GOF_df_eval$lambda.value))
    ) +
    scale_color_manual(name = "Legend",
                       values = c("R2" = "black",
                                  "Scaled RMSE" = "steelblue",
                                  "Validation Set" = "darkred",
                                  "Test Set" = "forestgreen")) +
    ggtitle(label = paste0("Optimal Lambda for model ", model_name,"FC-horizon ",h," months"),
            subtitle = paste0("Optimal lambda validation set = ", round(lambda_opt_train, 4),
                              " Optimal lambda test set = ", round(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)], 4))) +
    theme_minimal() +
    theme(legend.position = "top")
  print(plot)
  dev.off()
  
  ### plot with bars and without R2 value -----
  
  png(filename = paste0("results/",model_name,"/",model_name,"__RMSE_n_coefs-fc-horizon_",h,".png"),
      width = 1000, height = 600, units = "px")
  GOF_df_eval <- GOF_df_eval %>%
    mutate(scaled_RMSE_2 = (RMSE - min(RMSE)) /
             (max(RMSE) - min(RMSE)) * (max(n_coefs) - min(n_coefs)) + min(n_coefs))
  plot <- GOF_df_eval %>%
    ggplot(aes(x = log10(lambda.value))) +
    geom_col(aes(y = n_coefs), color = "lightgrey", alpha = 0.7) +
    geom_point(aes(y = scaled_RMSE_2, color = "Scaled RMSE"), size = 3) +  # Bigger points
    geom_line(aes(y = scaled_RMSE_2, color = "Scaled RMSE"), alpha = 0.9, linewidth = 1.5) +  # Thicker line
    geom_vline(aes(xintercept = log10(lambda_opt_train), color = "Lambda Validation Set"),
               linetype = "solid", linewidth = 1.4) +  # Thicker vline
    geom_vline(aes(xintercept = log10(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)]),
                   color = "Lambda Test Set"),
               linetype = "solid", linewidth = 1.4) +  # Thicker vline
    
    scale_x_continuous(
      name = expression(log[10](lambda)),
      breaks = pretty(log10(GOF_df_eval$lambda.value))
    ) +
    scale_y_continuous(
      name = "Number of Coefficients Used",
      sec.axis = sec_axis(~ (. - min(n_coefs)) / (max(n_coefs) - min(n_coefs)) *
                            (max(GOF_df_eval$RMSE) - min(GOF_df_eval$RMSE)) + min(GOF_df_eval$RMSE),
                          name = "RMSE")
    ) +
    scale_color_manual(name = "Legend",
                       values = c("Scaled RMSE" = "steelblue",
                                  "Lambda Validation Set" = "darkred",
                                  "Lambda Test Set" = "forestgreen")) +
    ggtitle(
      label = paste0("Optimal Lambda for Model ", model_name, " | FC-Horizon: ", h, " Months"),
      subtitle = paste0("Validation Set Lambda: ", round(lambda_opt_train, 4),
                        " | Test Set Lambda: ",
                        round(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)], 4))
    ) +
    theme_light(base_size = 15) +  # Larger base font size and lighter background
    theme(
      legend.position = "top",
      legend.title = element_text(size = 14, face = "bold"),  # Bigger and bold legend title
      legend.text = element_text(size = 12),  # Bigger legend text
      axis.title = element_text(size = 14, face = "bold"),  # Bigger and bold axis titles
      axis.text = element_text(size = 12, color = "black"),  # Bigger axis text
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered, bold title
      plot.subtitle = element_text(size = 13, hjust = 0.5),  # Centered subtitle
      panel.grid.minor = element_blank(),  # Clean minor grids
      panel.grid.major = element_line(color = "grey85")  # Subtle major grid
    )
  
  print(plot)
  
  
  print(plot)
  dev.off()
  
  coefs_list_fc[[paste0("fc_horizon_",h)]] <- coefs_df
  
  print(paste0("iteration ",h,"/",max(fc_horizon)," finished"))
  best_tunes_list[[paste0("fc_horizon_",h)]] <- model$bestTune
  
  #### save the predictions to be the input data (lagged_flow variable) for the next iteration ----
  #since it turned out that the optimal lambda / model for the validation data is different than for the evaluation, those predictions are seperately saved
  #actually in a first step, test if it is better to just predict everything with the same set of model parameters -> keep it simple
  
  lambda_opt_eval <- GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)]
  
  preproc_x <- caret::preProcess(x, method = c("center", "scale"))
  x <- predict(preproc_x, x)  # Apply the same scaling & centering as in the model training
  
  pred_all <- predict(final_model, newx = as.matrix(x), s = lambda_opt_eval, type = "response")
  # pred_all <- dplyr::lead(pred_all, h)
  pred_all <- pred_all^2
  lag_flow_1_previous <- df$Flow_min_monthly_lag1
  
}

save(fc_horizon, file = paste0("results/",model_name,"/",model_name,"_fc_horizon.RData"))

save(models_list, file = paste0("results/",model_name,"/",model_name,"_model_fit.RData"))
save(coefs_list_fc,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
save(pred_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_list.RData"))
save(best_tunes_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_best_tune.RData"))
save(GOF_fc_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))

