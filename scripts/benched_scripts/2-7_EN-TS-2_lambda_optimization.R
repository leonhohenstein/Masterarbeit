library(tidyverse)
library(ggplot2)
library(glmnet)
library(fable)
library(tsibble)
library(data.table)


rm(list=ls())

############################################################################
########### ELASTIC NET OPTIMIZATION DETRENDING SCRIPT #####################
############################################################################


### model parameters ----

model_name <- c("EN-TS-2_lambda_optimization")
dataset <- "lagged_TB" #TB = Tauchenbach
lagged <- TRUE #define if lagged variables of the detrended variable should be calculated too
n_step <- 1 #defines the number of rows which are added to the training set in each iteration
n_lambdas <- 30
set.seed(42) # set a seed to ensure that random numbers generation and test/training procedures are reproducible
origin_ratio <- 0.6 #defines where the Time-Series Cross validation starts to run (e.g. for 0.6 the first TS-CV fold starts with the first 60% of the training data and each now fold builds upon that)
n_test_years <- 6
### loading data ----
load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))
origin <- (nrow(df)*origin_ratio) %>% 
  ceiling()#defines the first row of the dataset to be used as testing data


df <- df %>% #removing surplus date columns
  select(-ends_with(c( "BF_min_monthly","BF_max_monthly","BF_mean_monthly","rel")))


df$month <- as.numeric(df$month)
df$year <- as.numeric(df$year)

SD_vars <- df %>% 
  select(-c()) %>% 
  setDT() %>% 
  .[,lapply(.SD,sd,na.rm = T)]

# as.numeric(df$Date)

############### IMPLEMENT THE TS-Validation approach ####################################
df$Date <- as.Date(df$Date)
all_years <- unique(df$year)
df_eval <- df %>% 
  setDT %>% 
  .[year >= all_years[length(all_years)-n_test_years],]

df_train <- df %>% 
  setDT %>% 
  .[year < all_years[length(all_years)-n_test_years],]

df_scaled <- df_train ################ scalin of input variable not yet implemented here CODE RED

df_stretched <- df_scaled %>% 
  na.omit() %>% 
  tsibble :: as_tsibble(index = Date) %>% 
  stretch_tsibble(.init = origin, .step = n_step)
n_iterations <- max(df_stretched$.id)


# fits_list <- list()
coefs_list <- list()
GOF <- data.frame()
GOF_list <- list()  

lambda_names <- vector()

for (i in 1:n_lambdas) {
  lambda_names[i] <- paste0("lambda_",i)
}

################# fitting the model -----
  

##### Filter and select train and test data  -----------

x_stretched <- df_stretched %>% 
        mutate(.id = as.numeric(.id)) %>% 
        as.data.frame() %>% 
        select(-c("Flow_min_monthly", ".id"))%>% 
          data.matrix() #saving the input data as matrix in order to make it work for cv.glmnet()

  
y_stretched <- df_stretched %>% 
  pull(Flow_min_monthly) %>% 
  as.matrix()


fold_id <- df_stretched$.id %>% 
  as.vector()


cv.fit <- cv.glmnet(x_stretched, y_stretched, 
                     type.measure="mse",
                     alpha=1, 
                     family="gaussian", 
                     nlambda = n_lambdas,
                     # lambda.min.ratio = 0.0001,
                     standardize = T,# (standardize = TRUE assumes that input variables have to be standardized by the cv.glmnet function itself) in order to make the model give back standardized coefficients, the standardisation is done manually outside the fitting loop
                     foldid = fold_id,#an optional vector of values between 1 and nfolds identifying what fold each observation is in. If supplied, nfolds can be missing.
                     keep = T)
  
  
  
### calculating GOF values ----

preval_df <- cv.fit[["fit.preval"]] %>% 
    as.data.frame() %>% 
    setDT()
  
preval_df$fold_id <- cv.fit[["foldid"]]

preval_folds_lengths <- preval_df[,.N,by="fold_id"]


predictions_df <- preval_df[,.SD[.N],by = "fold_id"]

y_observed <- df_stretched %>% 
  select(c("Flow_min_monthly",".id")) %>% 
  as.data.frame() %>% 
  setDT() %>% 
  .[,.SD[.N],by=".id"]

predictions_df <- cbind(y_observed,predictions_df) %>% 
  select(-".id")


colnames(predictions_df) <- c("y_obs","Date","fold_id",lambda_names)

predictions_df %>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=y_obs), color = "green")+
  geom_line(aes(y=lambda_13), color = "red")


######################### Testing for lag =4 ####################################

# redictions_df_lag4 <- preval_df_lag4[,.SD[.N-4],by = "fold_id"]
# 
# 
# predictions_df_lag4 <- cbind(y_observed,predictions_df_lag4) %>% 
#   select(-".id")
# 
# 
# colnames(predictions_df_lag4) <- c("y_obs","Date","fold_id",lambda_names)
# 
# predictions_df_lag4 %>% 
#   ggplot(aes(x=Date))+
#   geom_line(aes(y=y_obs), color = "green")+
#   geom_line(aes(y=lambda_13), color = "red")




################### selecting the optimal lambda -----------------------

for (lambda in lambda_names) {
  
  # Select relevant columns based on lambda
  temp_predictions_df <- predictions_df %>% 
    select(contains(c("y_obs", paste0(lambda)))) 
  
  # Ensure correct variable selection
  y_pred <- temp_predictions_df %>% 
    pull(paste0(lambda))
  
  y_obs <- temp_predictions_df %>% pull("y_obs")
  
  # Compute performance metrics
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

GOF_df <- do.call(rbind,GOF_list)
GOF_df$lambda.value <- cv.fit[["lambda"]]
rm(GOF_list)

### GOF_plot -----

if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}

GOF_df <- GOF_df %>%
  mutate(scaled_RMSE = (RMSE - min(RMSE)) / (max(RMSE) - min(RMSE)) * (max(R2) - min(R2)) + min(R2))
# 
# png(filename = paste0("results/",model_name,"/",model_name,"_Optimal_Lambda_R2_RMSE.png"),
#     width = 1000, height = 600, units = "px")


GOF_df %>% 
  ggplot(aes(x=log10(lambda.value))) +
  geom_point(aes(y=R2, color = "R2")) +
  geom_line(aes(y=R2, color = "R2"), alpha=0.2, linewidth=1.2) +
  geom_point(aes(y=scaled_RMSE, color = "Scaled RMSE")) +
  geom_line(aes(y=scaled_RMSE, color = "Scaled RMSE"), alpha=0.2, linewidth=1.2) +
  geom_vline(xintercept = log10(GOF_df$lambda.value[which.max(GOF_df$R2)]), color = "red", linetype = "dashed") +
  geom_text(aes(x = log10(GOF_df$lambda.value[which.max(GOF_df$R2)]), 
                y = max(R2), 
                label = paste0("Optimal Lambda (R2)  = ", 
                               round(GOF_df$lambda.value[which.max(GOF_df$R2)], 4))), 
            vjust = -1, 
            color = "black") +
  geom_vline(xintercept = log10(GOF_df$lambda.value[which.min(GOF_df$RMSE)]), color = "steelblue", linetype = "dashed") +
  geom_text(aes(x = log10(GOF_df$lambda.value[which.min(GOF_df$RMSE)]), 
                y = min(scaled_RMSE), 
                label = paste0("Optimal Lambda (RMSE) = ", 
                               round(GOF_df$lambda.value[which.min(GOF_df$RMSE)], 4))), 
            vjust = -1, 
            color = "steelblue") +
  scale_x_continuous(
    name = expression(log[10](lambda)),
    breaks = pretty(log10(GOF_df$lambda.value))
  ) +
  scale_y_continuous(
    name = "R2",
    sec.axis = sec_axis(~ ., name = "RMSE")
  ) +
  scale_color_manual(values = c("R2" = "black", "Scaled RMSE" = "steelblue")) +
  labs(color = "Performance Metric") +
  ggtitle(label = paste0("Optimal Lambda for model ", model_name)) +
  theme_minimal() +
  theme(legend.position = "top")
# dev.off()

#### extracting the optimal model (chosen by smallest RMSE) ----

lambda_opt_train <- GOF_df$lambda[which.min(GOF_df$RMSE)]
# 
# coefs_df <- matrix(data=NA,nrow = (n_lambdas+1), ncol = (length(coefs_list[[paste0("lambda-",lambda)]])+1))
# coefs_df[,1] <- paste0("lambda-",1:n_lambdas)
# 
# 
# for (lambda in 1:n_lambdas) {
#   
#   coefs_df[lambda,2:ncol(coefs_df)] <- coefs_list[[paste0("iteration-",iteration)]][[paste0(lambda_opt)]] %>% 
#     unlist()
#   
# }
# coefs_df <- as.data.frame(coefs_df)
# colnames(coefs_df) <- c("iteration",coef_names,"lambda","alpha")
# rm(coefs_list)

#standardisation of the coefficients ----
#uses the formula bj(s) = bj / (SD(x)/SD(Y))
# 
# SD_Y <- SD_vars$Flow_min_monthly
# 
# coefs_df_bind <- coefs_df %>% 
#   # select(-c(".id","(Intercept)","iteration","lambda","alpha")) %>% 
#   setDT() %>% 
#   lapply(.,as.numeric) %>% 
#   as.data.frame()
# 
# 
# coefs_df_bind <- bind_rows(SD_vars,coefs_df_bind) %>% 
#   select(-c(".id","lambda","alpha","iteration","X.Intercept.")) %>% 
#   as.matrix()
# 
# coefs_df_sd <- matrix(data=NA, nrow = (n_iterations+1), ncol = (ncol(SD_vars))) 
# 
# for (i in 1:n_iterations) {
#   coefs_df_sd[i+1,] <- coefs_df_bind[i+1,]*(coefs_df_bind[1,]/SD_Y) 
#   
# }
# coefs_df_sd <- as.data.frame(coefs_df_sd)
# coefs_df_sd <- coefs_df_sd[-1,]
# colnames(coefs_df_sd) <- colnames(SD_vars)
# coefs_df_sd$iteration <- seq(1,n_iterations,1)
# 
# coefs_df_sd %>% 
#   pivot_longer(cols = !iteration,names_to = "predictor", values_to = "value_predictor") %>% 
#   ggplot(aes(x=iteration,y=value_predictor))+
#   geom_line()+
#   facet_wrap(~predictor, scales = "free")



#################### Model Evaluation ----------


x_eval <- df_eval %>% 
  select(-c("Flow_min_monthly"))%>% 
  data.matrix() #saving the input data as matrix in order to make it work for cv.glmnet()

y_eval <- df_eval %>% 
  pull(Flow_min_monthly) %>% 
  as.matrix()

predictions_df_eval <- predict(cv.fit, newx = x_eval, s= cv.fit[["lambda"]]) %>%
  as.data.frame() %>% 
  setDT


predictions_df_eval <- cbind(y_eval,predictions_df_eval)
predictions_df_eval$Date <- df_eval$Date
colnames(predictions_df_eval) <- c("y_obs",lambda_names,"Date")


predictions_df_eval %>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=y_obs), color = "green")+
  geom_line(aes(y=lambda_10), color = "red")


GOF_df_eval <- data.frame()
GOF_list_eval <- list()
for (lambda in lambda_names) {
  
  # Select relevant columns based on lambda
  temp_predictions_df <- predictions_df_eval %>% 
    select(contains(c("y_obs", paste0(lambda)))) 
  
  # Ensure correct variable selection
  y_pred <- temp_predictions_df %>% 
    pull(paste0(lambda))
  
  y_obs <- temp_predictions_df %>% pull("y_obs")
  
  # Compute performance metrics
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
                         mNSE=mNSE
                         # ,KGE=kge
  )
  
  GOF_list_eval[[paste0(lambda)]] <- temp.GOF
}

GOF_df_eval <- do.call(rbind,GOF_list_eval)
GOF_df_eval$lambda.value <- cv.fit[["lambda"]]
rm(GOF_list_eval)
lambda_opt_eval <- GOF_df_eval$lambda[which.min(GOF_df_eval$RMSE)]

### GOF_plot -----

if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}

GOF_df_eval <- GOF_df_eval %>%
  mutate(scaled_RMSE = (RMSE - min(RMSE)) / (max(RMSE) - min(RMSE)) * (max(R2) - min(R2)) + min(R2))

# png(filename = paste0("results/",model_name,"/",model_name,"_Optimal_Lambda_R2_RMSE.png"),
    # width = 1000, height = 600, units = "px")
GOF_df_eval %>% 
  ggplot(aes(x=log10(lambda.value))) +
  geom_point(aes(y=R2, color = "R2")) +
  geom_line(aes(y=R2, color = "R2"), alpha=0.2, linewidth=1.2) +
  geom_point(aes(y=scaled_RMSE, color = "Scaled RMSE")) +
  geom_line(aes(y=scaled_RMSE, color = "Scaled RMSE"), alpha=0.2, linewidth=1.2) +
  
  # Validation Set lines
  geom_vline(aes(xintercept = log10(GOF_df$lambda.value[which.max(GOF_df$R2)]), 
                 color = "Validation Set", linetype = "Validation Set")) +
  geom_vline(aes(xintercept = log10(GOF_df$lambda.value[which.min(GOF_df$RMSE)]), 
                 color = "Validation Set", linetype = "Validation Set")) +
  
  geom_text(aes(x = (log10(GOF_df$lambda.value[which.max(GOF_df$R2)])-0.2), 
                y = (max(R2)-0.4), 
                label = paste0("Validation Set  = ",  round(GOF_df$lambda.value[which.max(GOF_df$R2)], 4))), 
            vjust = -1, 
            color = "darkred") +
  geom_text(aes(x = (log10(GOF_df$lambda.value[which.min(GOF_df$RMSE)])-0.2), 
                y = (min(scaled_RMSE)+0.4), 
                label = paste0("Validation Set = ", round(GOF_df$lambda.value[which.min(GOF_df$RMSE)], 4))), 
            vjust = -1, 
            color = "darkred") +
  
  # Test Set lines
  geom_vline(aes(xintercept = log10(GOF_df_eval$lambda.value[which.max(GOF_df_eval$R2)]), 
                 color = "Test Set", linetype = "Test Set")) +
  geom_vline(aes(xintercept = log10(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)]), 
                 color = "Test Set", linetype = "Test Set")) +
  
  geom_text(aes(x = (log10(GOF_df_eval$lambda.value[which.max(GOF_df_eval$R2)])+0.2), 
                y = (max(R2)-0.4), 
                label = paste0("Test Set  = ", round(GOF_df_eval$lambda.value[which.max(GOF_df_eval$R2)], 4))), 
            vjust = -1, 
            color = "forestgreen") +
  geom_text(aes(x = (log10(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)])+0.2), 
                y = (min(scaled_RMSE)+0.4), 
                label = paste0("Test Set = ",  round(GOF_df_eval$lambda.value[which.min(GOF_df_eval$RMSE)], 4))), 
            vjust = -1, 
            color = "forestgreen") +
  
  scale_x_continuous(
    name = expression(log[10](lambda)),
    breaks = pretty(log10(GOF_df$lambda.value))
  ) +
  scale_y_continuous(
    name = "R2",
    sec.axis = sec_axis(~ ., name = "RMSE")
  ) +
  scale_color_manual(name = "Legend",
                     values = c("R2" = "black", 
                                "Scaled RMSE" = "steelblue", 
                                "Validation Set" = "darkred", 
                                "Test Set" = "forestgreen")) +
  scale_linetype_manual(name = "Legend",
                        values = c("Validation Set" = "dashed",
                                   "Test Set" = "dashed")) +
  ggtitle(label = paste0("Optimal Lambda for model ", model_name)) +
  theme_minimal() +
  theme(legend.position = "top")

# dev.off()



#saving the final model
save(lambda_names,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_lambda_definition.RData"))

save(predictions_df,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_df.RData"))
save(coefs_df_sd,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_df_sd.RData"))
save(GOF_df,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_df.RData"))
save(fits_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_fits_list.RData"))





