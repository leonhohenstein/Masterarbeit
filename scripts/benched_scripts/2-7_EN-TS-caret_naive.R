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

model_name <- c("EN-TS-caret_naive")
dataset <- "lagged_TB" #TB = Tauchenbach


### loading data ----
load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))
n_test_years <- 6

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


y_obs <- df_eval %>% 
  pull(Flow_min_monthly) %>%
  as.matrix()

#### fitting and results? -------

if (file.exists(paste0("results/",model_name,"/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/")))
}
y_obs <- as.data.frame(y_obs)
y_obs$Date <- dates_eval

pred_naive <- y_obs %>% 
  setDT() %>% 
  .[, `:=` (
  pred_t1 = shift(V1, type = "lag", n = 1),
  pred_t2 = shift(V1, type = "lag", n = 2),
  pred_t3 = shift(V1, type = "lag", n = 3)
)]

setnames(pred_naive, "V1", "y_obs")
predictions <- c("pred_t1","pred_t2","pred_t3")
GOF_list <- list()
for (fc_horizon in predictions) {
  
  temp_predictions_df <- pred_naive %>% 
    select(contains(c("y_obs", paste0(fc_horizon)))) 
  
  y_pred <- temp_predictions_df %>% 
    pull(paste0(fc_horizon))
  
  y_obs <- temp_predictions_df %>% pull("y_obs")
  
  # mse <- hydroGOF::mse(y_pred, y_obs)
  # mae <- hydroGOF::mae(y_pred, y_obs) 
  R2 <- hydroGOF::R2(y_pred, y_obs)
  mNSE <- hydroGOF::mNSE(y_pred, y_obs, j=1)
  kge <- hydroGOF::KGE(y_pred, y_obs, j=1)
  RMSE <- hydroGOF::rmse(y_pred, y_obs)
  
  # Store the GOF results
  temp.GOF <- data.frame(alpha=1, 
                         fc_horizon=fc_horizon,   
                         # MAE=mae, 
                         # MSE=mse, 
                         RMSE=RMSE, 
                         R2=R2,
                         # , 
                         mNSE=mNSE,
                         KGE=kge
  )
  
  GOF_list[[paste0(fc_horizon)]] <- temp.GOF
}


summary_table <- do.call(rbind,GOF_list)
summary_table <- summary_table %>% 
  select(c("fc_horizon","R2","RMSE"))
summary_table$fc_horizon <- c(1,2,3)

# Create a basic gt table
gt_table <- gt(summary_table) %>%
  tab_header(
    title = "Summary Statistics of Monthly Forecasting Performance",
    subtitle = paste0("Model: ", model_name, " (naive forecasting)")
  ) %>%
  fmt_number(
    columns = c(R2, RMSE),
    decimals = 2
  ) %>%
  cols_label(
    fc_horizon = md("Forecasting Horizon<br>(months)"),
    R2 = md("R²"),
    RMSE = md("RMSE")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_footnote(
    footnote = md("This model serves as a benchmark and only applied naive forecasting<br>simply by setting all forecasts to be the value of the last available observation."),
    locations = cells_title(groups = "title")
  )



gtsave(gt_table, paste0("results/",model_name,"/summary_table_forecasts.png"))









