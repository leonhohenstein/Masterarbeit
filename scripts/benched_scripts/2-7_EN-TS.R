library(tidyverse)
library(ggplot2)
library(glmnet)
library(fable)
library(tsibble)


rm(list=ls())

############################################################################
########### ELASTIC NET OPTIMIZATION DETRENDING SCRIPT #####################
############################################################################


### model parameters ----

model_name <- c("EN-TS")
dataset <- "lagged_TB" #TB = Tauchenbach
lagged <- TRUE #define if lagged variables of the detrended variable should be calculated too
n_step <- 3 #defines the number of rows which are added to the training set in each iteration
set.seed(42) # set a seed to ensure that random numbers generation and test/training procedures are reproducible

### loading data ----
load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))
origin <- (nrow(df)*0.4) %>% 
  ceiling()#defines the first row of the dataset to be used as testing data


df <- df %>% #removing surplus date columns
  select(-ends_with(c( "BF_min_monthly","BF_max_monthly","BF_mean_monthly","rel")))


df$month <- as.numeric(df$month)
df$year <- as.numeric(df$year)

############### IMPLEMENT THE TS-Validation approach ####################################
df$Date <- as.Date(df$Date)

df_streched <- df %>% 
  na.omit() %>% 
  tsibble :: as_tsibble(index = Date) %>% 
  stretch_tsibble(.init = origin, .step = n_step)

n_iterations <- max(df_streched$.id)

predictions_df <- matrix(data = NA, nrow = n_iterations, ncol = 12)
 # predictions_list <- list()
fits_list <- list()
coefs_list <- list()
GOF <- data.frame()
GOF_list <- list()  

lambda_names <- vector()

for (i in 1:8) {
    lambda_names[i] <- paste0("lambda_",i)
  }
  lambda_names[9] <- "lambda.min"
  lambda_names[10] <- "lambda.1se"
  
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
  
  ##### ACTUAL MODEL  -----------
  for (y in 1:10) {
    
    temp.fit <- cv.glmnet(x_train, y_train, type.measure="mse",
                          alpha=1, family="gaussian", nfolds = 10)
    
    
    lambdas <- temp.fit[["lambda"]][seq(1,81,by=10)]
    lambdas[9] <- temp.fit[["lambda.min"]]
    lambdas[10] <- temp.fit[["lambda.1se"]]
    
    fit.name <- lambda_names[y]
    
    predictions_df[iteration,(y+2)] <-
      predict(temp.fit, #calls the model, which was fitted to according to the iteration step 
              s=lambdas[y], #uses lambda value that was defined in the temporary_testfit
              newx=x_test) %>% #newx defines the testing data set
      as.vector()
    

    
    #### Extracting the coefficients used in the models and store as list -------
    
    
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
  mse <- hydroGOF::mse(y_pred, y_obs)
  mae <- hydroGOF::mae(y_pred, y_obs) 
  R2 <- hydroGOF::R2(y_pred, y_obs)
  mNSE <- hydroGOF::mNSE(y_pred, y_obs, j=1)
  kge <- hydroGOF::KGE(y_pred, y_obs, j=1)
  RMSE <- hydroGOF::rmse(y_pred, y_obs)
  
  # Store the GOF results
  temp.GOF <- data.frame(alpha=1, lambda=lambda,   
                         MAE=mae, MSE=mse, RMSE=RMSE, R2=R2, mNSE=mNSE, KGE=kge)
  
  GOF_list[[paste0(lambda)]] <- temp.GOF
}

GOF_df <- do.call(rbind,GOF_list)
  
if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}

save(lambda_names,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_lambda_definition.RData"))

save(predictions_df,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_df.RData"))
save(coefs_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
save(GOF_df,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_df.RData"))
save(fits_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_fits_list.RData"))




  
  