library(tidyverse)
library(ggplot2)
library(glmnet)


rm(list=ls())

### model parameters ----

model_name <- c("ENO-1")
dataset <- "lagged_TB" #TB = Tauchenbach
split_ratio <- 0.8 #gives the ratio of train / testing data

set.seed(42) # set a seed to ensure that random numbers generation and test/training procedures are reproducible

### loading data ----
load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))


df <- df %>% #removing surplus date columns
  select(-ends_with(c( "BF_min_monthly","BF_max_monthly","BF_mean_monthly","rel")))


df <- na.omit(df)
df$month <- as.numeric(df$month)
df$year <- as.numeric(df$year)


#create a vector with the true values that will be predicted by the model (flow_min_monthly)
x <- df %>% 
  select(-"Flow_min_monthly")%>% 
  data.matrix() #saving the input data as matrix in order to make it work for cv.glmnet()

y <- df$Flow_min_monthly


nrow_train <- (nrow(df)*split_ratio)
nrow_test <- (nrow(df)-nrow_train)

x_train_df <- matrix(NA,nrow_train,10)
y_train_df <- matrix(NA,nrow_train,10)
x_test_df <- matrix(NA,nrow_test,10)
y_test_df <- matrix(NA,nrow_test,10)

set.seed(42)
temp <- data.frame()
x_train_list <- list()
x_test_list <- list()

for (k in 1:10) {
  
train_rows <- sample(1:nrow(x), nrow_train)
test_rows <- setdiff(seq(1,nrow(df),1), train_rows)

x_train_df <- x[train_rows, ]

x_test_df <- x[test_rows, ]

y_train_df[,k] <- y[train_rows]
y_test_df[,k] <- y[test_rows]

x_train_list[[paste0("Fold-",k)]] <- x_train_df
x_test_list[[paste0("Fold-",k)]] <- x_test_df

}

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
   y_train <- y_train_df[,k]
   y_test <- y_test_df[,k]
   
  for (y in 1:10) {
    
    temp.fit <- cv.glmnet(x_train, y_train, type.measure="mse",
                          alpha=1, family="gaussian", nfolds = nrow(x_train))
    #setting nfolds = nrow(x_train) makes it a Leave-One-Out CV 
    
    lambdas <- temp.fit[["lambda"]][seq(1,81,by=10)]
    lambdas[9] <- temp.fit[["lambda.min"]]
    lambdas[10] <- temp.fit[["lambda.1se"]]
    
    fit.name <- lambda_names[y]
    
    temp.predicted <-
      predict(temp.fit, #calls the model, which was fitted to according to the iteration step 
              s=lambdas[y], #uses lambda value that was defined in the temporary_testfit
              newx=x_test) %>% #newx defines the testing data set
    as.vector()
    
    temp.predicted_df <- data.frame(q_predicted = temp.predicted, q_obs = y_test, Date=df$Date)
    
    predictions_list[[paste0("Fold-",k)]][[lambda_names[y]]] <- temp.predicted_df#adds the temporary fit to the list of fits for later explorations
    
    #adds the temporary predictions to the list of fits for later explorations
    
    
    # fits_list[[paste0("alpha_",1)]][[paste0("lambda",y)]] <- temp.fit #save the fits in a list for future explorations
    #this version of saving fits_list saves a limited number of combinations of alphas and lambdas
    #but what we are actually interested in are the fits for different alphas and all lambdas, so there is a better line of code at in the alpha loop below
    mse <- hydroGOF::mse(temp.predicted, y_test)
    
    mae <- hydroGOF::mae(temp.predicted, y_test)
    
    R2 <- hydroGOF::R2(temp.predicted, y_test)
    
    mNSE <- hydroGOF::mNSE(temp.predicted, y_test, j=1)
    
    kge <- hydroGOF::KGE(temp.predicted, y_test, j=1)
    
    RMSE <- hydroGOF::rmse(temp.predicted, y_test)

    
    ## Store the GOF ------------
    temp.GOF <- data.frame(alpha=1, lambda = lambdas[y],  fit.name=fit.name, 
                           MAE = mae,MSE=mse, RMSE = RMSE, R2=R2, mNSE=mNSE, KGE=kge)
    # GOF <- rbind(GOF, temp.GOF) #NTM brauch ich glaube nicht
    GOF_list[[paste0("Fold-",k)]][[lambda_names[y]]] <- temp.GOF
    
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
    
    coefs_list[[paste0("Fold-",k)]][[lambda_names[y]]] <- coefs

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


# 
# GOF_df <- do.call(rbind, GOF_list)
# GOF_long <- pivot_longer(GOF_df, cols = c("MSE","MAE","R2","mNSE","KGE") ,
#                             names_to = "GOF_criteria", 
#                             values_to = "GOF_value")
# 
# png(file=paste0("results/comparison_of_models/bar_plot_GOF_vs_models.png"),
#     width = 1000, height = 600, units = "px")
# 
# GOF_long %>%
#   filter(GOF_criteria %in% c("MSE","MAE","R2","mNSE","KGE")) %>%  # Keep only R2 and MAE
#   ggplot(aes(x = fit.name, y = GOF_value, fill = fit.name)) +  
#   geom_bar(stat = "identity", position = position_dodge()) +  # Bars side by side
#   facet_wrap(~GOF_criteria, scales = "free") +  # Separate plots for R2 and MAE
#   theme_minimal() +  
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x labels for readability
#   labs(x = "Model name and GOF_criterium to selection of best parameters (alpha and lambda)"
#        , y = "GOF Value", fill = "Alpha Value", title = "Comparison of R2 and MAE")
# 
# dev.off()



