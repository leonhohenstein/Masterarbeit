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

model_name <- c("EN-TS-2")
dataset <- "lagged_TB" #TB = Tauchenbach
lagged <- TRUE #define if lagged variables of the detrended variable should be calculated too
n_step <- 1 #defines the number of rows which are added to the training set in each iteration
n_lambdas <- 30
set.seed(42) # set a seed to ensure that random numbers generation and test/training procedures are reproducible

### loading data ----
load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))
origin <- (nrow(df)*0.4) %>% 
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

# df_scaled <- df %>% 
#   mutate(across(.cols = where(is.numeric) & !all_of(c("Flow_min_monthly","month","year")), 
#                 .fns = ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))

df_scaled <- df 

df_streched <- df_scaled %>% 
  na.omit() %>% 
  tsibble :: as_tsibble(index = Date) %>% 
  stretch_tsibble(.init = origin, .step = n_step)

n_iterations <- max(df_streched$.id)

predictions_df <- matrix(data = NA, nrow = n_iterations, ncol = (n_lambdas +2))
# predictions_df <- data.frame()
# predictions_list <- list()
fits_list <- list()
coefs_list <- list()
GOF <- data.frame()
GOF_list <- list()  

lambda_names <- vector()

for (i in 1:n_lambdas) {
  lambda_names[i] <- paste0("lambda_",i)
}

for (iteration in 1:n_iterations) {
  
  df <- df_streched %>% 
    
    ##### Filter and select train and test data  -----------
  
  
  filter(.id == iteration)
  
  x <- df %>% 
    select(-c("Flow_min_monthly", "Date"))%>% 
    data.matrix() #saving the input data as matrix in order to make it work for cv.glmnet()
  
  y_observed <- df$Flow_min_monthly
  
  
  x_train <- x[1:(nrow(x)-1),]
  x_test <- x[nrow(x),,drop=F]
  y_train <- y_observed[1:(nrow(x)-1)]
  y_test <- y_observed[nrow(x)]
  
  predictions_df[iteration,1] <- df$Date[nrow(df)]
  predictions_df[iteration,2] <- y_test
  
  # predictions_df <- df$Date %>% 
  #   as.data.frame()
  # 
  # predictions_df <- cbind(predictions_df,df$Date)
  # 
  # predictions_df$y_obs <- df$Flow_min_monthly
  # 
  ##### ACTUAL MODEL  -----------
  for (y in 1:n_lambdas) {
    
    # lambda <- lambda_names[y]
    temp.fit <- glmnet(x_train, y_train, 
                       type.measure="mse",
                       alpha=1, 
                       family="gaussian", 
                       nlambda = n_lambdas,
                       lambda.min.ratio = 0.0001,
                       standardize = T, #in order to make the model give back standardized coefficients, the standardisation is done manually outside the fitting loop
                       )
    n_lambdas <- length(temp.fit[["lambda"]])
    
    if (y > n_lambdas) {
      y <- length(n_lambdas)
    }
    
    lambdas <- temp.fit[["lambda"]][1:n_lambdas]
    
    fit.name <- lambda_names[y]
    
   
    predictions_df[iteration,(y+2)] <-
      predict(temp.fit, #calls the model, which was fitted to according to the iteration step 
              s=lambdas[y], #uses lambda value that was defined in the temporary_testfit
              newx=x_test) %>% #newx defines the testing data set
      as.vector()
    
    
    
    #### Extracting the coefficients used in the models and store as list -------
    # lambda.value <- temp.fit[["a0"]][[paste("s",y)]]
    
    # temp.fit[["a0"]][["s0"]]
    betas <- glmnet::coef.glmnet(temp.fit,
                                 s=lambdas[y])
    
    coef_names <- betas@Dimnames[[1]]
    
    coefs <- betas[,1] %>% 
      as.list() %>% 
      data.frame
    
    colnames(coefs) <- coef_names
    coefs$lambda <- lambdas[y]
    coefs$alpha <- 1
    
    coefs_list[[paste0("iteration-",iteration)]][[lambda_names[y]]] <- coefs
    
  }
  
  
  
  fits_list[[paste0("iteration-",iteration)]] <- temp.fit #save the fits in a list for future explorations
  print(paste0("iteration ",iteration,"/",n_iterations," finished"))
  
}  

### calculating GOF values ----

predictions_df <- as.data.frame(predictions_df)
colnames(predictions_df) <- c("Date","y_obs",lambda_names)
GOF_list <- list()

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
GOF_df$lambda.value <- lambdas
rm(GOF_list)

### GOF_plot -----

if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}

GOF_df <- GOF_df %>%
  mutate(scaled_RMSE = (RMSE - min(RMSE)) / (max(RMSE) - min(RMSE)) * (max(R2) - min(R2)) + min(R2))

png(filename = paste0("results/",model_name,"/",model_name,"_Optimal_Lambda_R2_RMSE.png"),
                      width = 1000, height = 600, units = "px")


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
dev.off()

#### extracting the optimal model (chosen by smallest RMSE) ----

lambda_opt <- GOF_df$lambda[which.min(GOF_df$RMSE)]

coefs_df <- matrix(data=NA,nrow = (n_iterations), ncol = (length(coefs_list[[paste0("iteration-",iteration)]][[paste0(lambda_opt)]])+1))
coefs_df[,1] <- paste0("iteration-",1:n_iterations)

# opt_fit <- fits_list[[]]
# ceofs_df[1,] <- 
for (iteration in 1:n_iterations) {
  
  coefs_df[iteration,2:ncol(coefs_df)] <- coefs_list[[paste0("iteration-",iteration)]][[paste0(lambda_opt)]] %>% 
    unlist()
  
}
coefs_df <- as.data.frame(coefs_df)
colnames(coefs_df) <- c("iteration",coef_names,"lambda","alpha")
rm(coefs_list)
#standardisation of the coefficients
#uses the formula bj(s) = bj / (SD(x)/SD(Y))
SD_Y <- SD_vars$Flow_min_monthly

coefs_df_bind <- coefs_df %>% 
  # select(-c(".id","(Intercept)","iteration","lambda","alpha")) %>% 
  setDT() %>% 
  lapply(.,as.numeric) %>% 
  as.data.frame()


coefs_df_bind <- bind_rows(SD_vars,coefs_df_bind) %>% 
    select(-c(".id","lambda","alpha","iteration","X.Intercept.")) %>% 
  as.matrix()

coefs_df_sd <- matrix(data=NA, nrow = (n_iterations+1), ncol = (ncol(SD_vars))) 
                  
for (i in 1:n_iterations) {
  coefs_df_sd[i+1,] <- coefs_df_bind[i+1,]*(coefs_df_bind[1,]/SD_Y) 
    
}
coefs_df_sd <- as.data.frame(coefs_df_sd)
coefs_df_sd <- coefs_df_sd[-1,]
colnames(coefs_df_sd) <- colnames(SD_vars)
coefs_df_sd$iteration <- seq(1,n_iterations,1)
  
coefs_df_sd %>% 
  pivot_longer(cols = !iteration,names_to = "predictor", values_to = "value_predictor") %>% 
  ggplot(aes(x=iteration,y=value_predictor))+
    geom_line()+
    facet_wrap(~predictor, scales = "free")
  




#saving the final model
save(lambda_names,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_lambda_definition.RData"))

save(predictions_df,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_df.RData"))
save(coefs_df_sd,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_df_sd.RData"))
save(GOF_df,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_df.RData"))
save(fits_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_fits_list.RData"))





