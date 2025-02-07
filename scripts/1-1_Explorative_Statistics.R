# Explorative Statistics -----
library(tseries)
library(tidyverse)
library(rlang)
library(ggplot2)
library(modifiedmk)

rm(list=ls())
load(file = "data/tauchenbach/Final_df_Tauchenbach.RData")

# List of all the variables -------------
c(                     #just to call all the available variables in the beginnign of the script
  "WB_1month.WB_abs",
  "WB_1month.WB_rel",
  "WB_2month.WB_abs",
  "WB_2month.WB_rel",
  "WB_3month.WB_abs",
  "WB_3month.WB_rel",
  "WB_6month.WB_abs",
  "WB_6month.WB_rel",
  "WB_12month.WB_abs",
  "WB_12month.WB_rel","Date",
  "month",
  "year",
  "T_min_mean_monthly",
  "T_max_mean_monthly",
  "snowcover_sum_monthly",
  "precipitation_sum_monthly",
  "sunshine_mean_monthly",
  "ETP_sum_monthly",
  "Flow_min_monthly",
  "bf_min_monthly",
  "bf_max_monthly",
  "bf_mean_monthly")
# Auto-Correlation Variables -----------
ACF_vars <- c(                      
  "WB_1month.WB_abs",
  "WB_2month.WB_abs",
  "WB_3month.WB_abs",
  "WB_6month.WB_abs",
  "WB_12month.WB_abs",
  "T_min_mean_monthly",
  "T_max_mean_monthly",
  "snowcover_sum_monthly",
  "precipitation_sum_monthly",
  "sunshine_mean_monthly",
  "ETP_sum_monthly",
  "Flow_min_monthly",
  "bf_min_monthly",
  "bf_max_monthly",
  "bf_mean_monthly") #define the variables to calculate the autocorrelation for

# Cross-Correlation Variables -----------
CCF_vars <- c(                     
  "WB_1month.WB_abs",
  "WB_2month.WB_abs",
  "WB_3month.WB_abs",
  "WB_6month.WB_abs",
  "WB_12month.WB_abs",
  "T_min_mean_monthly",
  "T_max_mean_monthly",
  "snowcover_sum_monthly",
  "precipitation_sum_monthly",
  "sunshine_mean_monthly",
  "ETP_sum_monthly",
  "Flow_min_monthly",
  "bf_min_monthly",
  "bf_max_monthly",
  "bf_mean_monthly",
   "month")
   #define the variables to calculate the cross- correlation for
# Auto-Correlation Function (ACF) ---------

#The ACF measures the linear predictability of the series at time t, say xt, 
#using only the value xs
#if we can predict xt perfectly from xs through a linear relationship, 
#xt = β0 + β1xs, then the correlation will be +1 when β1 > 0, and −1 when 
#β1 < 0.
#It is also called “serial correlation” or “lagged correlation” since it measures 
#the relationship between a variable’s current and historical values.

ACF_function <- function(vars,df){
  ACF_results_list <- list()
  #ACF_results <- data.frame()
  for (i in seq_along(ACF_vars)) {
    # i<- 7
    df_variable <- select(df,contains(paste0(ACF_vars[i])))  #use the slect instead of filter function to work with column names
    
    ACF_results <- acf(df_variable,plot=F,lag.max = 12, na.action = na.pass) #na.pass to skip na values, otherwise error
    
    ACF_results <-  data.frame(Variable = (ACF_vars[i]),
                               Lag_Number = ACF_results[["lag"]],
                               Aut_Cor = ACF_results[["acf"]])
    # colnames(ACF_results) <- c("Variable","Lag_Number","Aut_Cor")
    ACF_results_list[[i]] <- ACF_results #implementation of that with lists
  
    }
  
ACF_results_list_long <- do.call(rbind, ACF_results_list)

ACF_results_list_wide <- do.call(cbind, ACF_results_list)
ACF_results_list_wide <- reduce(ACF_results_list, full_join, by = "Lag_Number")
names(ACF_results_list_wide) <- make.unique(names(ACF_results_list_wide))
ACF_results_list_wide <- select(ACF_results_list_wide,contains("Aut_Cor"))
colnames(ACF_results_list_wide) <- ACF_vars
ACF_results_list_wide <- ACF_results_list_wide %>% 
  mutate(Lag_Number = ACF_results$Lag_Number) %>% 
  relocate(Lag_Number, .before=1)

return(list(
  long = ACF_results_list_long,
  wide = ACF_results_list_wide))
}

results_ACF <- ACF_function(ACF_vars,df)

results_ACF_long <- results_ACF[["long"]]
results_ACF_wide <- results_ACF[["wide"]]

ggplot(data = results_ACF_long, aes(x = Lag_Number, y = Aut_Cor))+
  geom_line()+
  ggtitle("Auto-Correlation of all Variables")+
  theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5))+
  xlab("Number of Lags")+
  ylab("Auto-Corrleation")+
  facet_wrap(~Variable)

# ggsave(filename = "explorative_plots/01_expl_Auto_Cor.png",
       # width = 22, height = 12, units = "cm")

# Cross-Variance Function -----
#Often, we would like to measure the predictability of another series yt from 
#the series xs. Assuming both series have finite variances, 



CCF_function <- function(vars,df){
  CCF_results_list <- list()
  #CCF_results <- data.frame()
  for (i in seq_along(CCF_vars)) {
    # i<- 7
    df_variable <- select(df,contains(paste0(CCF_vars[i])))  #use the slect instead of filter function to work with column names
    
    CCF_results <- ccf(df_variable,df$Flow_min_monthly,
                       plot=F,
                       lag.max = 12, 
                       na.action = na.pass) #na.pass to skip na values, otherwise error
    
    CCF_results <-  data.frame(Variable = (CCF_vars[i]),
                               Lag_Number = CCF_results[["lag"]],
                               Aut_Cor = CCF_results[["acf"]])
    # colnames(CCF_results) <- c("Variable","Lag_Number","Aut_Cor")
    CCF_results_list[[i]] <- CCF_results #implementation of that with lists
    
  }
  
  CCF_results_list_long <- do.call(rbind, CCF_results_list)
  
  CCF_results_list_wide <- do.call(cbind, CCF_results_list)
  CCF_results_list_wide <- reduce(CCF_results_list, full_join, by = "Lag_Number")
  names(CCF_results_list_wide) <- make.unique(names(CCF_results_list_wide))
  CCF_results_list_wide <- select(CCF_results_list_wide,contains("Aut_Cor"))
  colnames(CCF_results_list_wide) <- CCF_vars
  CCF_results_list_wide <- CCF_results_list_wide %>% 
    mutate(Lag_Number = CCF_results$Lag_Number) %>% 
    relocate(Lag_Number, .before=1)
  
  return(list(
    long = CCF_results_list_long,
    wide = CCF_results_list_wide))
}

results_CCF <- CCF_function(CCF_vars,df)

results_CCF_long <- results_CCF[["long"]]
results_CCF_wide <- results_CCF[["wide"]]

ggplot(data = results_CCF_long, aes(x = Lag_Number, y = Aut_Cor))+
  geom_line()+
  ggtitle("Cross-Correlation with Monthly Minimum Flow")+
  theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5))+
  xlab("Number of Lags")+
  ylab("Cross-Corrleation")+
  facet_wrap(~Variable)

# ggsave(filename = "explorative_plots/01_expl_CrossCor_flow_month_min.png",
       # width = 20, height = 12, units = "cm")


# Stationarity ------

#modified Mann-Kendall test

mmk_test <- mmkh(df$Flow_min_monthly,ci=0.95) #ci = confidence interall
# The Mann-Kendall trend test is a nonparametric trend tests which assumes no distribution of the
# data. The null hypothesis of the test is that there is no trend in the data and the alternative hypothesis
# is that the data represents a monotonic trend

bbsmk <- bbsmk(df$Flow_min_monthly, ci=0.95, nsim=200, eta=1, bl.len=NULL)
#Significant serial correlation present in time series data can be accounted for using the nonpara-
#metric block bootstrap technique, which incorporates the Mann-Kendall trend tes







#Augmented Dickey-Fuller Test
adf_test <- adf.test(df$Flow_min_monthly)
plot(df$Flow_min_monthly)

sqrt(df$Flow_min_monthly) %>% 
  adf.test()

ADF_vars <- ACF_vars

ADF_function <- function(vars,df){
  ADF_results_list <- list()
  #ADF_results <- data.frame()
  for (i in seq_along(ADF_vars)) {
  # i<- 7
    df_variable <- select(df,contains(paste0(ADF_vars[i]))) %>% 
      na.omit() %>% 
      unlist()#use the select instead of filter function to work with column names
    
    ADF_results <- adf.test(df_variable) #na.pass to skip na values, otherwise error
    
    ADF_results <-  data.frame(Variable = (ADF_vars[i]),
                               Lag_Number = ADF_results[["parameter"]],
                               P_Value = ADF_results[["p.value"]])
    # colnames(ADF_results) <- c("Variable","Lag_Number","Aut_Cor")
    ADF_results_list[[i]] <- ADF_results #implementation of that with lists
    
  }
  
  ADF_results_list_long <- do.call(rbind, ADF_results_list)
  
  ADF_results_list_wide <- do.call(cbind, ADF_results_list)
  ADF_results_list_wide <- reduce(ADF_results_list, full_join, by = "Lag_Number")
  names(ADF_results_list_wide) <- make.unique(names(ADF_results_list_wide))
  ADF_results_list_wide <- select(ADF_results_list_wide,contains("P_Value"))
  colnames(ADF_results_list_wide) <- ADF_vars
  ADF_results_list_wide <- ADF_results_list_wide %>% 
    mutate(Lag_Number = ADF_results$Lag_Number) %>% 
    relocate(Lag_Number, .before=1)
  
  return(list(
    long = ADF_results_list_long,
    wide = ADF_results_list_wide))
}

results_ADF <- ADF_function(ADF_vars,df)

results_ADF_long <- results_ADF[["long"]]
results_ADF_wide <- results_ADF[["wide"]]

ggplot(data = results_ADF_long, aes(y = P_Value, x = Variable))+
  geom_point()+
  ggtitle("Auto-Correlation of all Variables")+
  theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5))+
  xlab("Number of Lags")+
  ylab("P_Value")
