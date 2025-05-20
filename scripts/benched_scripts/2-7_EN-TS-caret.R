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

model_name <- c("EN-TS-caret")
dataset <- "lagged_TB" #TB = Tauchenbach
lagged <- TRUE #define if lagged variables of the detrended variable should be calculated too
n_step <- 1 #defines the number of rows which are added to the training set in each iteration
n_lambdas <- 30
set.seed(42) # set a seed to ensure that random numbers generation and test/training procedures are reproducible
origin_ratio <- 0.6 #defines where the Time-Series Cross validation starts to run (e.g. for 0.6 the first TS-CV fold starts with the first 60% of the training data and each now fold builds upon that)
n_test_years <- 6
lambda_grid <- 10^seq(-3, 2, length = 50)
alpha_grid <- c(0.1,0.5,1)
fc_horizon <- c(1,2,3)
### loading data ----
load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))
origin <- (nrow(df)*origin_ratio) %>% 
  ceiling()#defines the first row of the dataset to be used as testing data


df <- df %>% #removing surplus date columns
  select(-ends_with(c( "BF_min_monthly","BF_max_monthly","BF_mean_monthly","rel")))


df$month <- as.numeric(df$month)
df$year <- as.numeric(df$year)
df$Date <- as.Date(df$Date)
SD_vars <- df %>% 
  select(-c()) %>% 
  setDT() %>% 
  .[,lapply(.SD,sd,na.rm = T)]
all_years <- unique(df$year)

dates_eval <- df %>% 
  setDT %>% 
  .[year >= all_years[length(all_years)-n_test_years],]
dates_eval <- dates_eval$Date

df$Date <- as.numeric(1:nrow(df))

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


################# fitting the model -----

TC <- caret::trainControl(method = "timeslice",
                              initialWindow = origin,
                              horizon = 1,
                              fixedWindow = FALSE)

df_train <- na.omit(df_train)
# 
# model <- caret::train(Flow_min_monthly ~ .,
#                     data = df_train,
#                     method = "glmnet",
#                     family = "gaussian",
#                     trControl = TC,
#                     tuneGrid = expand.grid(alpha = alpha_grid, lambda = lambda_grid),
#                     preProc = c("center", "scale"),
#                     metric="RMSE")
#                     # metric = caret::RMSE # Ensure caret's RMSE is used

# save(model, file = paste0("results/",model_name,"/model_fit.RData"))

load(paste0("results/",model_name,"/model_fit.RData"))
#### Analyse GOF measures for the validation data -------
# Convert alpha to a factor

model[["results"]]$alpha <- as.factor(model[["results"]]$alpha)

GOF_df <- model[["results"]]

if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}

png(filename = paste0("results/",model_name,"/",model_name,"_RMSE_vs_Lambda.png"),
    width = 1000, height = 600, units = "px")

ggplot(GOF_df, aes(x = log10(lambda), y = RMSE, color = alpha, group = alpha)) +
  geom_line() +                           # Line for each alpha
  geom_point() +                          # Points on each line for clarity
  scale_color_brewer(palette = "Set1") +   # Distinct colors for each alpha
  labs(
    x = "Log of Lambda",
    y = "RMSE",
    color = "Alpha",
    title = "Model Performance: RMSE vs Log(Lambda)",
    subtitle = "Different Alpha Values Represented as Distinct Colors"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#### predict on unseen out-of sample test data -------

lambda_opt_train <- GOF_df$lambda[which.min(GOF_df$RMSE)]
alpha_opt_train <- GOF_df$alpha[which.min(GOF_df$RMSE)]

data_fc_list <- list()
x_fc <- x_eval
x_fc <- x_fc %>% 
  as.data.frame() 
y_lag1 <- x_fc$Flow_min_monthly_lag1
y_lag2 <- x_fc$Flow_min_monthly_lag2
pred_eval_df <- matrix(data=NA, ncol = length(fc_horizon),nrow = length(y_obs))

for (t in fc_horizon) {
  
  x_fc <- cbind(x_fc, y_obs)
  x_fc <- na.omit(x_fc)
  y_obs <- x_fc$y_obs
  x_fc <- x_fc %>% 
    select(-"y_obs")
  
   
  x_fc <- as.matrix(x_fc)

  pred_eval <- predict(model, newdata = x_fc) %>% 
    as.data.frame()
  # pred_eval_df[t:nrow(pred_eval_df),t] <- pred_eval[,1]
  
  pred_eval_df[t:nrow(pred_eval_df),t] <- pred_eval[t:nrow(pred_eval),1] #
  
  data_fc <- x_fc[t:nrow(x_eval),] %>% 
    as.data.frame
  
  data_fc <- as.list(data_fc)
  data_fc_list[[paste0("fc-horizon_",t)]][[paste0("x_fc")]] <- data_fc
  data_fc_list[[paste0("fc-horizon_",t)]][[paste0("y_obs")]] <- y_obs[t:nrow(pred_eval)]
  data_fc_list[[paste0("fc-horizon_",t)]][[paste0("dates_eval")]] <- dates_eval[t:nrow(pred_eval)]
   
  # pred_eval_list[[paste0("fc-horizon_",t)]] <- pred_eval
  
  #prepare the train and test dataset for the next iteration
  x_fc <- as.data.frame(x_fc)
  x_fc$Flow_min_monthly_lag1 <- pred_eval
  x_fc$Flow_min_monthly_lag2 <- x_fc$Flow_min_monthly_lag1
  
  
  
  
}
pred_eval_df <- as.data.frame(pred_eval_df)
pred_eval_df$Date <- dates_eval
pred_eval_df$y_obs <- y_obs

colnames(pred_eval_df) <-  c("pred_t1","pred_t2","pred_t3","Date","y_obs")
save(pred_eval_df,file = paste0("results/",model_name,"/predictions_t1_t2_t3.RData"))
# pred_eval <- predict(model, newdata = x_eval)
# 
GOF_df_eval <- postResample(pred_eval, y_obs)

hydroGOF::R2(pred_eval_df$pred_t3,pred_eval_df$y_obs)

###extract coefficients ----
coefs_list <- list()
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

# colnames(n_coefs) <- lambda_names


#### predict also for other lambdas to check for overfitting ----
GOF_fc_list <- list()

for (t in 1:length(fc_horizon)) {
  x_fc <- data_fc_list[[paste0("fc-horizon_",t)]][[paste0("x_fc")]] %>% 
    as.data.frame() %>% 
    as.matrix()
  colnames(x_fc) <- colnames(x_train)
  y_obs <- data_fc_list[[paste0("fc-horizon_",t)]][[paste0("y_obs")]]
  dates <- data_fc_list[[paste0("fc-horizon_",t)]][[paste0("dates_eval")]]

  
  
pred_eval_lambdas <- matrix(data = NA, nrow= length(y_obs), ncol = length(lambda_values))

preproc <- model$preProcess

x_fc_standard <- predict(preproc, x_fc) %>% # Apply the same scaling & centering as in the model training
  as.matrix()
# pred_eval <- predict(model, newdata = x_fc)#using predict() already uses the optimal parameters of lambda and alpha 

for (i in 1:length(lambda_values)) {
  pred_eval_lambdas[,i] <- predict(final_model, newx = x_fc_standard, s = lambda_values[i], type = "response")
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
  
  # mse <- hydroGOF::mse(y_pred, y_obs)
  # mae <- hydroGOF::mae(y_pred, y_obs) 
  R2 <- hydroGOF::R2(y_pred, y_obs)
  mNSE <- hydroGOF::mNSE(y_pred, y_obs, j=1)
  kge <- hydroGOF::KGE(y_pred, y_obs, j=1)
  RMSE <- hydroGOF::rmse(y_pred, y_obs)
  
  # Store the GOF results
  temp.GOF <- data.frame(alpha=1, 
                         lambda=lambda,   
                         # MAE=mae, 
                         # MSE=mse, 
                         RMSE=RMSE, 
                         R2=R2,
                         # , 
                         mNSE=mNSE,
                         KGE=kge
  )
  
  GOF_list[[paste0(lambda)]] <- temp.GOF
}


GOF_df_eval <- do.call(rbind,GOF_list)
GOF_df_eval$lambda.value <- lambda_values
rm(GOF_list)
GOF_df_eval$n_coefs <- n_coefs %>% 
  unlist()


# GOF_df_eval <- GOF_df_eval %>% 
#   setDT() %>% 
#   .[R2 > 0 ]

#### selecting and plotting for optimal GOF values ----


if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}

GOF_df_eval <- GOF_df_eval %>%
  setDT() %>% 
  .[R2>0] %>% 
  mutate(scaled_RMSE = (RMSE - min(RMSE)) / (max(RMSE) - min(RMSE)) * (max(R2) - min(R2)) + min(R2))
GOF_df_eval$fc_horizon <- t
#### plot without bars ----

png(filename = paste0("results/",model_name,"/",model_name,"_Optimal_Lambda_R2_RMSE-fc-horizon_",t,".png"),
    width = 1000, height = 600, units = "px")
plot <- GOF_df_eval %>%
  setDT() %>% 
  .[R2>0] %>% 
  ggplot(aes(x=log10(lambda.value))) +
  # geom_col(aes(x=log10(lambda.value), y = n_coefs)) +
  # geom_point(aes(y=R2, color = "R2")) +
  # geom_line(aes(y=R2, color = "R2"), alpha=0.2, linewidth=1.2) +
  geom_point(aes(y=RMSE, color = "Scaled RMSE")) +
  geom_line(aes(y=RMSE, color = "Scaled RMSE"), alpha=0.2, linewidth=1.2) +
  
  # Validation Set line
  geom_vline(aes(xintercept = log10(lambda_opt_train), color = "Validation Set"), linetype = "solid") +
  
  # Test Set lines
  geom_vline(aes(xintercept = log10(GOF_df_eval$lambda.value[which.max(GOF_df_eval$R2)]), color = "Test Set"), linetype = "solid") +
  geom_vline(aes(xintercept = log10(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)]), color = "Test Set"), linetype = "solid") +
  
  scale_x_continuous(
    name = expression(log[10](lambda)),
    breaks = pretty(log10(GOF_df_eval$lambda.value))
  ) +
  # scale_y_continuous(
  #   name = "R2",
  #   sec.axis = sec_axis(~ ., name = "RMSE")
  # ) +
  scale_color_manual(name = "Legend",
                     values = c("R2" = "black", 
                                "Scaled RMSE" = "steelblue", 
                                "Validation Set" = "darkred", 
                                "Test Set" = "forestgreen")) +
  ggtitle(label = paste0("Optimal Lambda for model ", model_name,"FC-horizon ",t," months"), 
          subtitle = paste0("Optimal lambda validation set = ", round(lambda_opt_train, 4),
                            " Optimal lambda test set = ", round(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)], 4))) +
  theme_minimal() +
  theme(legend.position = "top")
print(plot)
dev.off()
#### plot with bars and without R2 value -----

png(filename = paste0("results/",model_name,"/",model_name,"__RMSE_n_coefs-fc-horizon_",t,".png"),
    width = 1000, height = 600, units = "px")
GOF_df_eval <- GOF_df_eval %>%
  mutate(scaled_RMSE_2 = (RMSE - min(RMSE)) / 
           (max(RMSE) - min(RMSE)) * (max(n_coefs) - min(n_coefs)) + min(n_coefs))  
plot <- GOF_df_eval %>% 
  ggplot(aes(x = log10(lambda.value))) +
  
  # Bar plot for the number of coefficients
  geom_col(aes(y = n_coefs), color = "lightgrey", alpha = 0.5) +
  
  # Points and line for Scaled RMSE
  geom_point(aes(y = scaled_RMSE_2, color = "Scaled RMSE")) +
  geom_line(aes(y = scaled_RMSE_2, color = "Scaled RMSE"), alpha = 0.2, linewidth = 1.2) +
  
  # Vertical lines for optimal lambdas
  geom_vline(aes(xintercept = log10(lambda_opt_train), color = "Lambda Validation Set"), 
             linetype = "solid", linewidth = 1.3) +
  
  geom_vline(aes(xintercept = log10(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)]), 
                 color = "Lambda Test Set"), 
             linetype = "solid", linewidth = 1.3) +
  
  # X-axis: log-scaled lambda
  scale_x_continuous(
    name = expression(log[10](lambda)),
    breaks = pretty(log10(GOF_df_eval$lambda.value))
  ) +
  
  # Y-axis: Number of coefficients + secondary RMSE axis
  scale_y_continuous(
    name = "Number of Coefficients used",
    sec.axis = sec_axis(~ (. - min(n_coefs)) / (max(n_coefs) - min(n_coefs)) * 
                          (max(GOF_df_eval$RMSE) - min(GOF_df_eval$RMSE)) + min(GOF_df_eval$RMSE),
                        name = "RMSE")
  ) +
  
  # Custom colors for legend
  scale_color_manual(name = "Legend",
                     values = c("Scaled RMSE" = "steelblue", 
                                "Lambda Validation Set" = "darkred", 
                                "Lambda Test Set" = "forestgreen")) +
  
  # Titles
  ggtitle(label = paste0("Optimal Lambda for model ", model_name, " FC-Horizon = ", t, " months"), 
          subtitle = paste0("Optimal lambda validation set = ", round(lambda_opt_train, 4), 
                            " | Optimal lambda test set = ", 
                            round(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)], 4))) +
  
  # Theme adjustments
  theme_minimal() +
  theme(legend.position = "top")

print(plot)
dev.off()
GOF_fc_list[[paste0("fc-horizon = ",t)]] <- GOF_df_eval

}
best_tune <- model$bestTune
save(coefs_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
save(pred_eval_df,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_df.RData"))
save(best_tune,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_best_tune.RData"))
save(GOF_fc_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))

