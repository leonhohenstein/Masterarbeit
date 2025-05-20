library(tidyverse)
library(ggplot2)
library(glmnet)


rm(list=ls())

############################################################################

########### ELASTIC NET OPTIMIZATION DETRENDING SCRIPT #####################


############################################################################


### model parameters ----

model_name <- c("ENO-DT")
dataset <- "lagged_TB" #TB = Tauchenbach
lagged <- TRUE #define if lagged variables of the detrended variable should be calculated too
split_ratio <- 0.8 #gives the ratio of train / testing data

set.seed(42) # set a seed to ensure that random numbers generation and test/training procedures are reproducible

### loading data ----
load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))


df <- df %>% #removing surplus date columns
  select(-ends_with(c( "BF_min_monthly","BF_max_monthly","BF_mean_monthly","rel")))


df$month <- as.numeric(df$month)
df$year <- as.numeric(df$year)

############### IMPLEMENT THE DETRENDING ####################################
df$n_month <- seq(1,nrow(df),1) #transform date into days in order to make the regression result more easily interpretable

dt_fit = lm(Flow_min_monthly ~ n_month,data = df, na.action=NULL) # regress gtemp on time

dt_values <- (dt_fit[["coefficients"]][["n_month"]]*df$n_month)

df$Flow_min_monthly_dt <- (df$Flow_min_monthly - dt_values )  


if(lagged == TRUE){
  
  df$Flow_min_monthly_dt_lag1 <- NA
  
  for(i in 2: (nrow(df))){
    df$Flow_min_monthly_dt_lag1[i] <-  df$Flow_min_monthly_dt[i-1]
    
    
  }
    print("lag = 1 was calculated")   
}




if(lagged == TRUE){
  
      df$Flow_min_monthly_dt_lag2 <- NA
      
  for(i in 3: (nrow(df))){
    df$Flow_min_monthly_dt_lag2[i] <-  df$Flow_min_monthly_dt[i-2]
    
    
  }
  print("lag = 2 was calculated")   
  print("Model is lagged model, therefore lagged flow variables were calculated and added to the dataset")
}


# par(mfrow=c(1,1))
# plot(resid(dt_fit), type="o", main="detrended") 
# plot(diff(df$Flow_min_monthly), type="o", main="first difference")
# 
# 
# par(mfrow=c(3,1)) # plot ACFs 
# acf(df$Flow_min_monthly, 24, main="gtemp") 
# acf(resid(dt_fit), 48, main="detrended") 
# acf(diff(df$Flow_min_monthly), 48, main="first difference")


#create a vector with the true values that will be predicted by the model (flow_min_monthly)
df <- na.omit(df)
dt_values <- dt_values[1:nrow(df)]
x <- df %>% 
  select(-c("Flow_min_monthly",
            "Flow_min_monthly_dt",
            "Flow_min_monthly_lag1",
            "Flow_min_monthly_lag2")) %>% 
  data.matrix() #saving the input data as matrix in order to make it work for cv.glmnet()

y_original <- df$Flow_min_monthly
y_dt <- df$Flow_min_monthly_dt


nrow_train <- (nrow(df)*split_ratio)
nrow_test <- (nrow(df)-nrow_train)

x_train_df <- matrix(NA,nrow_train,10)
y_train_df_dt <- matrix(NA,nrow_train,10)
x_test_df <- matrix(NA,nrow_test,10)
y_test_df_original <- matrix(NA,nrow_test,10)

set.seed(42)
temp <- data.frame()
x_train_list <- list()
x_test_list <- list()

for (k in 1:10) {
  
  train_rows <- sample(1:nrow(x), nrow_train)
  test_rows <- setdiff(seq(1,nrow(df),1), train_rows)
  
  x_train_df <- x[train_rows, ]
  
  x_test_df <- x[test_rows, ]
  
  y_train_df_dt[,k] <- y_dt[train_rows]
  y_test_df_original[,k] <- y_original[test_rows]
  
  x_train_list[[paste0("Fold-",k)]] <- x_train_df
  x_test_list[[paste0("Fold-",k)]] <- x_test_df
  
}


#Before Modelling, visualize the detrended values:

# predictions_df[which(predictions_df$k == k &
#                        predictions_df$lambda == lambda 
#                      # & predictions_df$Date > "2013-01-01" 
#                      ),] %>%
  
df %>%  
  ggplot()+
  geom_line(aes(x=Date,y=Flow_min_monthly), color = "green")+
  geom_line(aes(x=Date,y=Flow_min_monthly_dt), color = "red")+
  theme_light()
  
df %>%  
  ggplot()+
  geom_line(aes(x=Date,y=Flow_min_monthly), color = "green")+
  geom_line(aes(x=Date,y=Flow_min_monthly_dt), color = "red")+
  theme_light()


### Training the model and predicting --------
# we set alpha = 1 since it turned out to show best results but we still need to find an optimal lamdba

predictions_list <- list()
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

for (k in 1:10) {
  x_train <- x_train_list[[paste0("Fold-",k)]]
  x_test <- x_test_list[[paste0("Fold-",k)]]
  y_train_dt <- y_train_df_dt[,k]
  y_test_original <- y_test_df_original[,k]
  
  for (i in 1:10) {
    
    temp.fit <- cv.glmnet(x_train, y_train_dt, type.measure="mse",
                          alpha=1, family="gaussian", nfolds = nrow(x_train))
    #setting nfolds = nrow(x_train) makes it a Leave-One-Out CV 
    
    lambdas <- temp.fit[["lambda"]][seq(1,81,by=10)]
    lambdas[9] <- temp.fit[["lambda.min"]]
    lambdas[10] <- temp.fit[["lambda.1se"]]
    
    fit.name <- lambda_names[i]
    
    temp.predicted <-
      predict(temp.fit, #calls the model, which was fitted to according to the iteration step 
              s=lambdas[i], #uses lambda value that was defined in the temporary_testfit
              newx=x_test) %>% #newx defines the testing data set
      as.vector()
    
    temp.predicted_trended <- (temp.predicted + dt_values[test_rows]) 

    # y_test_trended <- (y_test + dt_values) 
    # y_test_trended <- y_test_original
    
    
    temp.predicted_df <- data.frame(q_predicted = temp.predicted_trended, q_obs = y_test_original, Date=df$Date)
    
    predictions_list[[paste0("Fold-",k)]][[lambda_names[i]]] <- temp.predicted_df#adds the temporary fit to the list of fits for later explorations
    
    #adds the temporary predictions to the list of fits for later explorations
    
    
    # fits_list[[paste0("alpha_",1)]][[paste0("lambda",y)]] <- temp.fit #save the fits in a list for future explorations
    #this version of saving fits_list saves a limited number of combinations of alphas and lambdas
    #but what we are actually interested in are the fits for different alphas and all lambdas, so there is a better line of code at in the alpha loop below
    mse <- hydroGOF::mse(temp.predicted_trended, y_test_original)
    
    mae <- hydroGOF::mae(temp.predicted_trended, y_test_original)
    
    R2 <- hydroGOF::R2(temp.predicted_trended, y_test_original)
    
    mNSE <- hydroGOF::mNSE(temp.predicted_trended, y_test_original, j=1)
    
    kge <- hydroGOF::KGE(temp.predicted_trended, y_test_original, j=1)
    
    RMSE <- hydroGOF::rmse(temp.predicted_trended, y_test_original)
    
    
    ## Store the GOF ------------
    temp.GOF <- data.frame(alpha=1, lambda = lambdas[i],  fit.name=fit.name, 
                           MAE = mae,MSE=mse, RMSE = RMSE, R2=R2, mNSE=mNSE, KGE=kge)
    # GOF <- rbind(GOF, temp.GOF) #NTM brauch ich glaube nicht
    GOF_list[[paste0("Fold-",k)]][[lambda_names[i]]] <- temp.GOF
    
    #### Extracting the coefficients used in the models and store as list -------
    
    
    betas <- glmnet::coef.glmnet(temp.fit,
                                 s=lambdas[i])
    
    coef_names <- betas@Dimnames[[1]]
    
    coefs <- betas[,1] %>% 
      as.list() %>% 
      data.frame
    
    colnames(coefs) <- coef_names
    coefs$lambda <- lambdas[i]
    coefs$alpha <- 1
    
    coefs_list[[paste0("Fold-",k)]][[lambda_names[i]]] <- coefs
    
  }
  fits_list[[paste0("Fold-",k)]] <- temp.fit #save the fits in a list for future explorations
  print(paste0("Fold ",k,"/10 finished"))
}



if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}


save(lambda_names,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_lambda_definition.RData"))

save(predictions_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_list.RData"))
save(coefs_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
save(GOF_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))
save(fits_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_fits_list.RData"))



