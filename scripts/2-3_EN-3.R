rm(list=ls())

library(glmnet)
library(tidyr)
library(tidyverse)
library(hydroGOF) #in order to calculate the Nash-Sutcliffe efficiency  

model_name <- "EN-2"
dataset <- "lagged_TB" #TB = Tauchenbach
###### LOAD DATASET AND SPLIT BETWEEN TRAIN AND TEST DATA--------


set.seed(42) # set a seed to ensure that random numbers generation and test/training procedures are reproducible
load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))

df <- df %>% #removing surplus date columns
  select(-ends_with(c( "BF_min_monthly","BF_max_monthly","BF_mean_monthly","rel")))


df <- na.omit(df)
df$month <- as.numeric(df$month)
df$year <- as.numeric(df$year)

train_test_ratio <- 0.8 #define the ration between train and test years 
years_total <- unique(df$year) %>% 
  length() 
train_years_length <- ceiling(years_total * train_test_ratio)

train_years <- c(df$year[1]:(df$year[1]+train_years_length))
test_years <-  c((df$year[1]+train_years_length+1):tail(df$year,n=1))                

train_df <- df %>% 
  subset(.,df$year < test_years[1])

test_df <- df %>% 
  subset(.,df$year >= test_years[1])

#create a vector with the true values that will be predicted by the model (flow_min_monthly)
y_train <- train_df$Flow_min_monthly 
y_test <- test_df$Flow_min_monthly

x_train <- train_df %>% #only remain the values to be predicted 
  select(-ends_with(c("Flow_min_monthly", #use ends_with to only not remove the "Flow_min_monthly_lagged" variables
                      "Date"))) %>% 
  data.matrix() #saving the input data as matrix in order to make it work for cv.glmnet()

x_test <- test_df %>% #only remain the values to be predicted
  select(-ends_with(c("Flow_min_monthly",
                      "Date"))) %>% 
  data.matrix()


##### ACTUAL MODEL  -----------

predictions_list <- list()
fits_list <- list()
coefs_list <- list()
GOF <- data.frame()
GOF_list <- list()


for (a in 0:10) {
  
  for (y in 1:10) {
    
    temp.fit <- cv.glmnet(x_train, y_train, type.measure="mse",
                          alpha=a/10, family="gaussian")
    
    lambdas <- temp.fit[["lambda"]][seq(1,81,by=10)]
    lambdas[9] <- temp.fit[["lambda.1se"]]
    lambdas[10] <- temp.fit[["lambda.min"]]
    
    fit.name <- paste0("alpha_", a/10,"-lambda_",y)
    
    temp.predicted <-
      predict(temp.fit, #calls the model, which was fitted to according to the iteration step 
              s=lambdas[y], #uses lambda value that was defined in the temporary fit
              newx=x_test)#newx defines the testing data set
    temp.predicted <- data.frame(q_sim = temp.predicted, q_obs = y_test, Date=test_df$Date)
 
    predictions_list[[paste0("alpha_",a/10)]][[paste0("lambda",y)]] <- temp.predicted#adds the temporary fit to the list of fits for later explorations
    
    #adds the temporary predictions to the list of fits for later explorations
    
    
    # fits_list[[paste0("alpha_",a/10)]][[paste0("lambda",y)]] <- temp.fit #save the fits in a list for future explorations
      #this version of saving fits_list saves a limited number of combinations of alphas and lambdas
      #but what we are actually interested in are the fits for different alphas and all lambdas, so there is a better line of code at in the alpha loop below
  mse <- mse(temp.predicted[,1], y_test)
  
  MAE <- mae(temp.predicted[,1], y_test)
  
  R2 <- R2(temp.predicted[,1], y_test)
  
  mNSE <- mNSE(temp.predicted[,1], y_test, j=1)
  
  kge <- KGE(temp.predicted[,1], y_test, j=1)
  
  
  ## Store the GOF ------------
  temp.GOF <- data.frame(alpha=a/10, lambda = lambdas[y], MSE=mse, fit.name=fit.name, 
                         MAE = MAE, R2=R2, mNSE=mNSE, KGE=kge)
  # GOF <- rbind(GOF, temp.GOF) #NTM brauch ich glaube nicht
  GOF_list[[paste0("alpha_",a/10)]][[paste0("lambda",y)]] <- temp.GOF
  
  #### Extracting the coefficients used in the models and store as list -------
  
  
  betas <- glmnet::coef.glmnet(temp.fit,
                               s=lambdas[y])
  
   coef_names <- betas@Dimnames[[1]]
  
  coefs <- betas[,1] %>% 
    as.list() %>% 
    data.frame
  
  colnames(coefs) <- coef_names
  coefs$lambda <- lambdas[y]
  coefs$alpha <- a/10
  
  coefs_list[[paste0("alpha_",a/10)]][[paste0("lambda",y)]] <- coefs

  }
    
    fits_list[[paste0("alpha_",a/10)]] <- temp.fit #save the fits in a list for future explorations
  
    print(paste0(a,"/10 finished"))
    
  }
  

save(predictions_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_list.RData"))
save(coefs_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
save(GOF_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))
save(fits_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_fits_list.RData"))

















