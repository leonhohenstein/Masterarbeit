rm(list=ls())

library(glmnet)
library(tidyr)
library(tidyverse)
library(hydroGOF) #in order to calculate the Nash-Sutcliffe efficiency  

model_name <- "RR1_lagged"
catchment <- "TB" #TB = Tauchenbach
###### LOAD DATASET AND SPLIT BETWEEN TRAIN AND TEST DATA--------


set.seed(42) # set a seed to ensure that random numbers generation and test/training procedures are reproducible
load(file = paste0("data/tauchenbach/models/",model_name,"_",catchment,"_data.RData"))

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


##### MODEL 1 -----------

#description of MODEL 1:
#1. Fixe Training und Test Zeiträume aber unterschiedliche Ridge Regression Penalties
# 1. Beantwortet:→ wie viele variablen treffen gleiche aussage und wie wichtig ist regularization?
#in the glmnet package, alpha determines the ratio between ridge regression (0) and lasso regression (1)
#here we set alpha to 0 in order to get a pure ridge regression model:

# 
# alpha0.fit <- cv.glmnet(x_train, y_train, type.measure="mse", 
#                         alpha=0, family="gaussian")
# 
# alpha0.predicted <- 
#   predict(alpha0.fit, #fitted model that we will use to predict the test values
#           s=alpha0.fit$lambda.1se, #uses the lambda value that was found optimal to achieve best results with the simplest possible model
#           newx=x_test)#newx defines the testing data set


list.of.fits <- list()
for (i in 0:10) {
  ## Here's what's going on in this loop...
  ## We are testing alpha = i/10. This means we are testing
  ## alpha = 0/10 = 0 on the first iteration, alpha = 1/10 = 0.1 on
  ## the second iteration etc.
  
  ## First, make a variable name that we can use later to refer
  ## to the model optimized for a specific alpha.
  ## For example, when alpha = 0, we will be able to refer to 
  ## that model with the variable name "alpha0".
  fit.name <- paste0("alpha", i/10)
  
  ## Now fit a model (i.e. optimize lambda) and store it in a list that 
  ## uses the variable name we just created as the reference.
  list.of.fits[[fit.name]] <-
    cv.glmnet(x_train, y_train, type.measure="mse", alpha=i/10, 
              family="gaussian")
}
# 
### LOOP FOR CALCULATING DIFFERENT ALPHAS AND LAMBDAS -----------

## Now we see which alpha (0, 0.1, ... , 0.9, 1) does the best job

### predicting the values in the Testing dataset.and cacluating GOF -------------

GOF <- data.frame()
coefficients <- data.frame()
coefs_list <- list()
predictions_list <- list()

for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  ## Use each model to predict 'y' given the Testing dataset -------
  predicted <- 
    predict(list.of.fits[[fit.name]], 
            s=list.of.fits[[fit.name]]$lambda.1se, newx=x_test)
  
  ## Calculate the Mean Squared Error... -----------
  mse <- mean((y_test - predicted)^2)
  
  ## Calculate the Nash-Sutcliffe Efficiency -----------
  #The Nash-Sutcliffe efficiency (NSE) is a normalized statistic that determines 
  #the relative magnitude of the residual variance ("noise") compared to the 
  #measured data variance ("information") (Nash and Sutcliffe, 1970).
  
  nse <- NSE(predicted[,1], y_test)
  
  ## Calculate the coefficient of determination (R2) ----------
  #(R2) is the proportion of the variation in the dependent variable that is 
  #predictable from the independent variable(s).
  #It is a statistic used in the context of statistical models whose main purpose 
  #is either the prediction of future outcomes or the testing of hypotheses, on 
  #the basis of other related information.
  
  R2 <- R2(predicted[,1], y_test)
  
  
  ## Calculate the modified NSE   ----------
  # according to Althoff & Rodrigues (2021) the modified NSE is recommended for low flows
  mNSE <- mNSE(predicted[,1], y_test, j=1)
  
  
  ## Calculate the Kling-Gupta Efficiency ----
  # according to Althoff & Rodrigues (2021) the modified KGE is more balanced 
  #for bias under both low- and peakflow conditions.
  kge <- KGE(predicted[,1], y_test, j=1)
  
  #### Extracting the coefficients used in the models and store as list -------
  
  betas <- glmnet::coef.glmnet(list.of.fits[[fit.name]], 
                      s=list.of.fits[[fit.name]][["lambda.1se"]])
  
  coef_names <- betas@Dimnames[[1]]
  
  coefs <- betas[,1] %>% 
    as.list() %>% 
    data.frame
  
  colnames(coefs) <- coef_names
  
  coefs$alpha <- i/10

  coefs_list[[fit.name]] <- coefs
  
  
  ## Store the GOF ------------
  temp <- data.frame(alpha=i/10, MSE=mse, fit.name=fit.name, NSE = nse, R2=R2, mNSE=mNSE, KGE=kge)
  GOF <- rbind(GOF, temp)
  
  ## Store the predictions ------------
  predicted <- data.frame(q_obs = y_test,q_sim = predicted[,1], alpha = i/10, Date = test_df$Date)
  predictions_list[[fit.name]] <- predicted 
  
  png(file=paste0("results/",model_name,"/MSE_error_plots/",model_name,"_",fit.name,"_MSE_error_plot.png"))
  plot(list.of.fits[[fit.name]],xvar="lambda",label=TRUE,main = paste0("alpha = ", i/10,", model = ",model_name))
  dev.off()
  
  png(file=paste0("results/",model_name,"/MSE_error_plots/",model_name,"_",fit.name,"_MSE_error_plot.png"))
  plot(list.of.fits[[fit.name]],xvar="dev",label=TRUE,main = paste0("alpha = ", i/10,", model = ",model_name))
  dev.off()
}

plot(list.of.fits[[fit.name]], xvar = "deviance", label = TRUE)

# cv.lasso <- glmnet(x_train,y_train)
# plot(cv.lasso, xvar="dev", label = T) #the deviance plot only works for glmnet objects, not for cv.glmnet


# predictions_table_long <- do.call(rbind, predictions_list)
# predictions_table_wide <- do.call(cbind, predictions_list)
# coefficients_table_long<- do.call(rbind, coefs_list)
# coefficients_table_wide<- do.call(cbind, coefs_list)

save(predictions_list,file = paste0("results/",model_name,"/",model_name,"_",catchment,"_predictions_list.RData"))
save(coefs_list,file = paste0("results/",model_name,"/",model_name,"_",catchment,"_coefs_list.RData"))
save(GOF,file = paste0("results/",model_name,"/",model_name,"_",catchment,"_GOF.RData"))

# 
# ggplot(predictions_table_long) +
#   #geom_line(aes(x = Date, y = WB_abs),colour = "red")+
#   geom_line(aes(x = Date, y = q_sim), colour = "green")+
#   geom_line(
#     data = data.frame(Date = predictions_table_long$Date, flow_monthly_min = y_test),
#     aes(x = Date, y = flow_monthly_min),
#     colour = "darkgrey") +
#   
#   facet_wrap("alpha")

















