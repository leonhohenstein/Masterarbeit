library(ggplot2)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(tibble)
library(gt)
library(webshot2)
library(data.table)
library(RColorBrewer)
library(caret)
library(purrr)
library(stringr)
library(purrr)
library(patchwork)
library(lfstat)
rm(list=ls())

### name the models and dataset of interest ------
today <- Sys.Date()
# today <-
model_name <- c("test_MSE_SE_1")
fc_years <- c("2003_2004")
dataset <- "lagged_TB" #TB = Tauchenbach
# station <- "tauchenbach"
fc_years_list <- c("2003_2004","2015_2016","2015_2021")

ensemble_df_all <- NULL
upper_BM_df_all <- NULL
forecast_df_all <- NULL

for (fc_years in fc_years_list) {
  
  # fc_years <- "2015_2021"



load(paste0("results/",today,"/Climate_Ensemble/",fc_years,"/Climate_Ensemble_forecasts_list_",fc_years,".RData"))

ensemble_df_all <-   purrr::imap_dfr(forecasts_list, ~ { 
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    df <- .x
    
    df <- df %>% mutate(station = station, horizon = horizon, 
                        date = date, obs = obs, pred = pred, 
                        horizon = horizon,horizon_char = horizon_char,
                        ensemble = ensemble,
                        fc_method = "ensemble")
    
  })
}) %>% mutate(fc_period = fc_years) %>%  rbind(.,ensemble_df_all)

ensemble_df_all$station[ensemble_df_all$station == "tauchenbach"] <- "Tauchenbach"
ensemble_df_all$station[ensemble_df_all$station == "kienstock"] <- "Kienstock"
ensemble_df_all$station[ensemble_df_all$station == "flattach"] <- "Flattach"
ensemble_df_all$station[ensemble_df_all$station == "uttendorf"] <- "Uttendorf"

load(paste0("results/",today,"/Climate_Upper_BM/",fc_years,"/Climate_Upper_BM_forecasts_list_",fc_years,".RData"))
upper_BM_df_all <-   purrr::imap_dfr(forecasts_list, ~ { 
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    df <- .x
    
    df <- df %>% mutate(station = station, horizon = horizon, 
                        date = date, obs = obs, pred = pred, 
                        horizon = horizon,horizon_char = horizon_char,
                        fc_method = "upper_BM")
    
  })
})%>% mutate(fc_period = fc_years) %>%  rbind(.,upper_BM_df_all)

upper_BM_df_all$station[upper_BM_df_all$station == "tauchenbach"] <- "Tauchenbach"
upper_BM_df_all$station[upper_BM_df_all$station == "kienstock"] <- "Kienstock"
upper_BM_df_all$station[upper_BM_df_all$station == "flattach"] <- "Flattach"
upper_BM_df_all$station[upper_BM_df_all$station == "uttendorf"] <- "Uttendorf"


load(paste0("results/",today,"/",model_name,"/",fc_years,"/",model_name,"_coefs_list_",fc_years,".RData"))
load(paste0("results/",today,"/",model_name,"/",fc_years,"/",model_name,"_forecasts_list_",fc_years,".RData"))
load(paste0("results/",today,"/",model_name,"/",fc_years,"/",model_name,"_final_model_list_",fc_years,".RData"))
load(paste0("results/",today,"/",model_name,"/",fc_years,"/",model_name,"_GOF_list_",fc_years,".RData"))
load(paste0("results/",today,"/",model_name,"/",fc_years,"/",model_name,"_n_coefs_list_",fc_years,".RData"))
load(paste0("results/",today,"/",model_name,"/",fc_years,"/",model_name,"_results_",fc_years,".RData"))

# load("data/flow_quantiles_list.RData")


# quantiles <- flow_quantiles_list[[paste0(station)]]

GOF_df_all <-   purrr::imap_dfr(GOF_list, ~ { 
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    df <- .x
    
    df <- df %>% mutate(station = station, horizon = fc_horizon, 
                        horizon = horizon,horizon_char = horizon_char,
                        fc_method = "fc_model")
    
  })
})


forecast_df_all <-   purrr::imap_dfr(forecasts_list, ~ { 
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    df <- .x
    
    df <- df %>% mutate(station = station, horizon = horizon, 
                        horizon = horizon,horizon_char = horizon_char,
                        fc_method = "fc_model")
    
  })
}) %>% mutate(fc_period = fc_years) %>%  rbind(.,forecast_df_all)

coefs_list_final <- list()

cm_list <- list()

### Visualizing Cofficients --------------

n_horizons <- as.numeric(length(unique(names(forecasts_list))))

if (file.exists(paste0("results/",today,"/",model_name,"/coefficients/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/coefficients/")))
}


n_coefs_all <-   purrr::imap_dfr(n_coefs_list, ~ { 
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    df <- .x
    
    df <- df %>% mutate(station = station,  
                        horizon_char = horizon_char,
                        fc_method = "fc_model") %>% 
      rename(n_coefs = ".")
    
  })
})

}

hist_fc_df_all <- forecast_df_all %>% rename(flow = pred) %>% select(-"obs") %>%  filter(fc_period %in% c("2003_2004","2015_2016"))
hist_fc_df_all <- forecast_df_all %>% mutate(flow = obs, fc_method = "observed_flow") %>% select(-c("obs","pred")) %>% 
  rbind(hist_fc_df_all, .) %>% filter(fc_period %in% c("2003_2004","2015_2016"))

hist_fc_df_all <- ensemble_df_all %>% 
  filter(fc_period %in% c("2003_2004","2015_2016")) %>%  
  filter(ensemble == "ensemble_median") %>% 
  mutate(fc_method = ensemble) %>% rename(flow = pred) %>% 
  select(-c("obs","ensemble")) %>% 
  rbind(hist_fc_df_all, .) 

hist_fc_df_all <- upper_BM_df_all %>% 
  filter(fc_period %in% c("2003_2004","2015_2016")) %>%  
  rename(flow = pred) %>% 
  select(-c("obs")) %>% 
  rbind(hist_fc_df_all, .) 

hist_fc_df_all <- hist_fc_df_all %>% filter(lubridate::year(date) %in% c(2003,2015)) 
  

# coefficients plots ------
#### COEFS HORIZONTAL BARS ----



#### COEFS VERTICAL BARS ---------


coefs_df_all <- purrr::imap_dfr(coefs_list, ~ {
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    
    df <- .x
    df <- rownames_to_column(df)
    df <- df %>% mutate(horizon_char = horizon_char, 
                        station = station) %>% 
      rename(variable = rowname, coefs = s1)
    
    
  })
  
})


coefs_df_all$fc_horizon <- stringr::str_remove(coefs_df_all$horizon, "fc_horizon_") %>%
  as.numeric()

coefs_df_all$n_horizon <- coefs_df_all$fc_horizon %>% 
  factor(levels = sort(unique(.)))

coefs_df_all$variable <- as.factor(coefs_df_all$variable)


coefs_means <- purrr::imap_dfr(
  results,
  ~ {
    station <- .y
    
    purrr::imap_dfr(.x, ~ {
      tibble(
        term = names(.x$coef_means),
        estimate = .x$coef_means,
        fc_horizon = as.integer(gsub("fc_horizon_", "", .y))) %>% mutate(station = station) %>% 
        rename(mean_coef_CV = estimate, variable = term)
      
    }
    
    )
  }
)

coefs_sd <- purrr::imap_dfr(
  results,
  ~ {
    station <- .y
    
    purrr::imap_dfr(.x, ~ {
      tibble(
        term = names(.x$coef_sd),
        estimate = .x$coef_sd,
        fc_horizon = as.integer(gsub("fc_horizon_", "", .y))) %>% mutate(station = station) %>% 
        rename(sd_coef_CV = estimate, variable = term)
      
    }
    
    )
  }
)

coefs_df_all <- left_join(coefs_df_all, coefs_means, by = c("variable", "fc_horizon","station"))
coefs_df_all <- left_join(coefs_df_all, coefs_sd, by = c("variable", "fc_horizon","station"))

coefs_df_all$coefs_ub <-  coefs_df_all$mean_coef_CV + coefs_df_all$sd_coef 
coefs_df_all$coefs_lb <-  coefs_df_all$mean_coef_CV - coefs_df_all$sd_coef 


lambda_df_all <- purrr::imap_dfr(results, ~ {
  
  station <- .y
  
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    
    
    
    df <- .x$global_optimization_results
    
    df <- df %>% mutate(horizon_char = horizon_char,
                        station = station) 
    
  })
})

lambda_df_all$fc_horizon <- stringr::str_remove(lambda_df_all$horizon_char, "fc_horizon_") %>%
  as.numeric()

lambda_df_all$n_horizon <- lambda_df_all$fc_horizon %>% 
  factor(levels = sort(unique(.)))


# CREATING FORECAST_DF WITH ALL FC-METHODS for all Stations ----
temp <- NULL
forecast_df_long_all <- NULL
fc_df_quant_GOF <- NULL
quantiles_all <- NULL
cum_seg_all <- NULL


for(s in unique(forecast_df_all$station)){
  # 
  s <- "Kienstock"
  forecast_df <- forecast_df_all %>% filter(station ==s)
  start_date_GOF <- forecast_df %>% 
    filter(horizon == as.numeric(max(forecast_df$horizon)) & station == s) 

  start_date_GOF <- start_date_GOF$date[1]

  load(file =  paste0("data/",s,"/Final_df_",s,"_weekly.RData"))
  
  naive_season <- df %>%   filter(Date < start_date_GOF) %>%
    select(c("Date","flow_mean","week_year")) %>%  rename(date = Date, flow = flow_mean)
  
  naive_season <- naive_season %>%  setDT() %>% .[, naive_season := mean(flow, na.rm = T), by = week_year]

    
  quantiles <- sapply(1:100, function(x) quantile(df$flow_mean, probs = 1-(x/100))) %>% 
    data.frame(quant_flow = ., exceed_prob = 1:100)
  
  mean_obs_tot <- df %>%   filter(Date > start_date_GOF) %>% pull(flow_mean) %>% 
    mean()
  cum_seg <- NULL
  for(q in 1:100){
    
    # q <- 90
    
    cum_seg_th <- quantiles %>% filter(q == exceed_prob) 
    cum_seg <- forecast_df %>% filter(obs < cum_seg_th$quant_flow & station == s)
    cum_seg <- cum_seg %>% setDT() %>% 
      .[, {
        mean_pred <- mean(pred)
        mean_obs_tot <- mean_obs_tot
        mean_obs_seg <- mean(obs)
        ssr <- sum((pred - mean_obs_tot)^2)
        sstot <- sum((obs - mean_obs_tot)^2)
        r2 <- 1-(ssr / sstot)
        RMSE = hydroGOF::rmse(sim = pred, obs = obs)
        r2_2 = hydroGOF::R2(sim = pred, obs = obs)
        
        .(r2= r2,RMSE= RMSE, mean_pred = mean_pred, mean_obs_seg = mean_obs_seg, mean_obs_tot = mean_obs_tot,r2_2 =r2_2)
      }, by = horizon]
    
    cum_seg$exceed_prob <- q
    cum_seg$station <- s
    
    cum_seg_all <- rbind(cum_seg, cum_seg_all)
  
  }

        
    # ADDING OTHER MODEL FORECASTS ----
    
    forecast_df  <- df %>% rename(date = Date) %>%  select(c("week_year","date")) %>% 
      left_join(forecast_df,.,by = "date")
    
    forecast_df <- naive_season %>%
      distinct(week_year, .keep_all = TRUE) %>%
      select(week_year, naive_season) %>%
      left_join(forecast_df,., by = "week_year")
    
    forecast_df <- ensemble_df_all %>% filter(ensemble == "ensemble_median" & station == s) %>%  
      select(c("date","pred","horizon")) %>% 
      rename(ensemble_median = pred) %>% 
      left_join(forecast_df,.,by= (c("date","horizon")))
    
    forecast_df <- upper_BM_df_all %>%  filter(station == s) %>% 
      select(c("date","pred","horizon")) %>% 
      rename(upper_benchmark = pred) %>% 
      left_join(forecast_df,.,by= (c("date","horizon")))
    
    forecast_df <- forecast_df %>% setDT() %>% 
      .[, naive_lag := lag(obs, horizon), by = .(horizon)]
    
    forecast_df$station <- s
    temp <- rbind(forecast_df,temp)
    quantiles$station <- s
    quantiles_all <- rbind(quantiles_all, quantiles)


    
    forecast_df_long <- temp %>% rename(catchment_memory_model = pred) %>% 
      select(-"fc_method") %>% pivot_longer(cols = c("naive_season","upper_benchmark","ensemble_median",
                                                     "catchment_memory_model","naive_lag"), values_to = "pred",
                                            names_to = "fc_method")  
    
    forecast_df_long$res <- forecast_df_long$pred - forecast_df_long$obs
    
    forecast_df_long_all <- rbind(forecast_df_long_all, forecast_df_long)
    
    
    for(q in 1:100){
      
      # q <- 51
      fc_df_quant <- NULL
      cum_seg_th <- quantiles_all %>% filter(q == exceed_prob  & station ==s) 
      fc_df_quant <- forecast_df_long %>% filter(obs < cum_seg_th$quant_flow & station ==s)
      fc_df_quant <- fc_df_quant %>% setDT() %>% 
        .[, {
          mean_pred <- mean(pred)
          mean_obs_tot <- mean_obs_tot
          mean_obs_seg <- mean(obs)
          ssr <- sum((pred - mean_obs_tot)^2)
          sstot <- sum((obs - mean_obs_tot)^2)
          r2 <- 1-(ssr / sstot)
          RMSE = hydroGOF::rmse(sim = pred, obs = obs)
          RRMSE_seg = RMSE/mean_obs_seg
          r2_2 = hydroGOF::R2(sim = pred, obs = obs)
          
          .(r2= r2,RMSE= RMSE, mean_pred = mean_pred, mean_obs_seg = mean_obs_seg, 
            mean_obs_tot = mean_obs_tot,r2_2 =r2_2)
        }, by = .(horizon, fc_method, station)]
      
      fc_df_quant$exceed_prob <- q
      fc_df_quant$station <- s
      
      fc_df_quant_GOF <- rbind(fc_df_quant, fc_df_quant_GOF)
      
    }
}

forecast_df_all <- temp
cum_seg_all <- left_join(cum_seg_all , quantiles_all, by = c("exceed_prob","station"))

# test <- fc_df_quant_GOF %>% filter(station ==s &horizon ==1 & fc_method =="naive_season", exceed_prob ==50)



# test <- forecast_df_long_all %>% filter(station ==s &horizon ==1 & fc_method =="naive_season")

# START LOOP FOR PLOTTING ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
for (s  in unique(forecast_df_all$station)) {
  
  # s <- "Tauchenbach"
  coefs_df <- coefs_df_all %>% filter(station ==s)
  top_coefs <- coefs_df_all %>% filter(station == s) %>% setDT() %>% 
    .[variable != "(Intercept)", .(coefs = sum(abs(coefs))), by = variable]
  
  top_coefs <- top_coefs %>%  arrange(desc(coefs))
  
  top_coefs <- top_coefs$variable[1:15]
  
  png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_coefs_bars_all_fc-h_",s,"_",fc_years,".png"),
      width = 1000, height = 600, units = "px")
  
  plot <- coefs_df %>%
    filter(station == s) %>% 
    filter(variable %in% top_coefs) %>%
    mutate(variable = factor(variable, levels = top_coefs)) %>% 
    ggplot(aes(x = n_horizon, y = coefs, fill = n_horizon)) +
    geom_col(color = "black", width = 0.9) +
    facet_wrap(~variable, scales = "fixed", ncol = 5, nrow = 3) +
    labs(
      x = "Forecasting Horizon",
      y = "Coefficient of Predictors",
      fill = "Forecasting Horizon",
      title = paste0("Station: ",s," | Importance of 20 Most Important Predictor Variables in ",model_name,"_",fc_years),
      subtitle = paste0(Sys.time())
    ) +
    theme_minimal(base_size = 14) +  # Set base font size
    geom_hline(yintercept = 0)+
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),  # Facet titles
      legend.position = "bottom"
    )
  
  # scale_x_discrete(breaks = n_horizon)
  print(plot)
  dev.off()
  
  #### Coefficient Consistency vs. horizons ----
  
  
  #### Error LINES Version 1 ----
  
  
  png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_coefs_lines_shadings",s,"_",fc_years,".png"),
      width = 1000, height = 600, units = "px")
  
plot <-   coefs_df_all %>%
    filter(variable %in% top_coefs[1:16] & station ==s) %>%
    mutate(variable = factor(variable, levels = top_coefs)) %>%
    ggplot(aes(x = as.numeric(n_horizon), y = coefs, group = 1)) +
    geom_ribbon(aes(ymin = coefs_lb, ymax = coefs_ub), alpha = 0.8, fill = "gray") +
    geom_line() +
    geom_point(fill = "steelblue", shape = 21) +
    facet_wrap(~variable, scales = "fixed", ncol = 4, nrow = 4)+
    theme_bw()+
    labs(x= "Forecasting Horizon [weeks]",
         y = "Variable Coefficients",
         title = "Variable Coefficients vs. Forecasting Horizons",
         subtitle = paste0("Model: ", model_name, " from: ", Sys.Date()," station = ",s,"_",fc_years))+
    scale_x_continuous(breaks = seq(1,12,1))
print(plot)
  dev.off()
  #### Error BARS Version 2 ----
  
  png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_coefs_error_bars_",s,"_",fc_years,".png"),
      width = 1000, height = 600, units = "px")
  
plot <-   coefs_df_all %>%
    filter(variable %in% top_coefs[1:16] & station ==s) %>%
    mutate(variable = factor(variable, levels = top_coefs)) %>% 
    ggplot(aes(x = n_horizon, y = coefs, fill = n_horizon)) +
    
    geom_col(color = NA, width = 0.9, alpha = 1) +                    
    # geom_point(aes(y = mean_coef_CV), size = 0.8) +                          
    geom_errorbar(aes(ymin = coefs_lb, ymax = coefs_ub),                    
                  width = 0.4, size = 0.5) +                                  # Thicker lines
    
    geom_hline(yintercept = 0) +                                            
    facet_wrap(~variable, scales = "fixed", ncol = 4, nrow = 4) +                                
    scale_x_discrete(breaks = seq(1, 12, 1)) +                                #
    labs(
      x = "Forecasting Horizon [weeks]",
      y = "Variable Coefficients",
      title = paste0("Variable Coefficients vs. Forecasting Horizons"),
      subtitle = paste0("Model: ", model_name, " from: ", Sys.Date()," station = ",s,"_",fc_years)
    ) +
    theme_bw() +                                                            
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "none")
print(plot)
  dev.off()
  




#### LAMBDA vs. RMSE Plots ----


lambda_df <- lambda_df_all %>% filter(station == s)
n_CV_folds <- results[[paste0(s)]][["fc_horizon_1"]][["params"]][["num_CV_folds"]]
  
SE.factor <- results[[paste0(s)]][["fc_horizon_1"]][["params"]][["SE.factor"]]

lambda_min <- lambda_df %>%
  group_by(fc_horizon) %>%
  filter(MSE == min(MSE)) %>%
  slice(1) %>%  ungroup() %>%
  select(fc_horizon, lambda_min = lambda, MSE = MSE, MSESD = MSESD)

lambda_min$threshold <- lambda_min$MSE + ((lambda_min$MSESD/sqrt(n_CV_folds)) * SE.factor) #labelled as SD but actaully represents the Standard Error

lambda.1se_graf <- as.numeric(c(rep(NA,(max(lambda_min$fc_horizon)))))
lambda.opt <- as.numeric(c(rep(NA,(max(lambda_min$fc_horizon)))))

for(h in 1:nrow(lambda_min)){
  
  temp <- lambda_df[which(lambda_df$fc_horizon == h),]
  # if()
  #add lambda_opt
  lambda.opt[h] <- results[[paste0(s)]][["fc_horizon_1"]][["params"]][["lambda_opt"]]
  
  temp <- temp[temp$MSE <= lambda_min$threshold[h],]
  lambda.1se_graf[h] <- temp$lambda[which.max(temp$lambda)]
  
}

lambda_1se_df <- tibble(
  fc_horizon = lambda_min$fc_horizon,
  lambda_1se = lambda.1se_graf,
  lambda.opt = lambda.opt)

lambda_lines  <- lambda_min %>%
  select(fc_horizon, lambda_min) %>%
  left_join(lambda_1se_df, by = "fc_horizon")

lambda_df$MSE_lb <- lambda_df$MSE-lambda_df$MSESD
lambda_df$MSE_ub <- lambda_df$MSE+lambda_df$MSESD


h <- 8
lambda_df %>% filter(fc_horizon ==h) %>% 
  ggplot(aes(x = log10(lambda), y = MSE)) +
  geom_line() +
  geom_vline(
    data = lambda_lines,
    aes(xintercept = log10(lambda_min[h])),
    linetype = "dashed", color = "steelblue"
  ) +
  geom_vline(
    data = lambda_lines,
    aes(xintercept = log10(lambda_1se[h])),
    linetype = "dashed", color = "forestgreen"
  ) +
  geom_vline(
    data = lambda_lines,
    aes(xintercept = log10(lambda.opt[h])),
    linetype = "dashed", color = "red"
  ) +
  labs(title = paste0("MSE across log10(lambda) per forecast horizon in ", model_name," | ",s,"_",fc_years, " | ",Sys.Date()),
       subtitle = "Blue = lambda.min, Green = lambda.1se",
       x = "log10(lambda)", y = "MSE") +
  theme_minimal()

wtest <- lambda_df %>% filter(fc_horizon ==h)











lambda_lines_long <- lambda_lines %>%
  pivot_longer(cols = c(lambda_min, lambda_1se, lambda.opt), 
               names_to = "lambda_type", 
               values_to = "lambda_value") %>%
  mutate(color_label = case_when(
    lambda_type == "lambda_min" ~ "lambda.min",
    lambda_type == "lambda_1se" ~ "lambda.1se",
    lambda_type == "lambda.opt" ~ "lambda.opt"
  ))






png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_lambda_plot_",s,"_",fc_years,".png"),
    width = 1000, height = 600, units = "px")

plot <- lambda_df %>%
  ggplot(aes(x = log10(lambda), y = MSE)) +
  geom_line() +
  facet_wrap(~fc_horizon) +
  geom_line(aes(y = MSE_lb), color = "darkred", linetype = "dotted") +
  geom_line(aes(y = MSE_ub), color = "darkred", linetype = "dotted") +
  geom_vline(
    data = lambda_lines_long,
    aes(xintercept = log10(lambda_value), color = color_label),
    linetype = "dashed"
  ) +
  labs(
    title = paste0("MSE across log10(lambda) per forecast horizon in ", model_name," | ",s,"_",fc_years, " | ",Sys.Date()),
    subtitle = "Red = lambda.min, Steelblue = lambda.1se, Green = lambda.opt",
    x = "log10(lambda)", y = "MSE", color = "Lambda Type"
  ) +
  scale_color_manual(
    values = c("lambda.min" = "red", "lambda.1se" = "steelblue", "lambda.opt" = "forestgreen")
  ) +
  theme_minimal() 
# plot <- lambda_df %>%
#   ggplot(aes(x = log10(lambda), y = MSE)) +
#   geom_line() +
#   facet_wrap(~fc_horizon) +
#   geom_line(aes(y = MSE_lb), color = "darkred", linetype = "dotted") +
#   geom_line(aes(y = MSE_ub), color = "darkred", linetype = "dotted") +
#   geom_vline(
#     data = lambda_lines,
#     aes(xintercept = log10(lambda_min)),
#     linetype = "dashed", color = "lambda.min"
#   ) +
#   geom_vline(
#     data = lambda_lines,
#     aes(xintercept = log10(lambda_1se)),
#     linetype = "dashed", color = "lambda.1se"
#   ) +
#   geom_vline(
#     data = lambda_lines,
#     aes(xintercept = log10(lambda.opt[h])),
#     linetype = "dashed", color = "lambda.opt"
#   ) +
#   labs(title = paste0("MSE across log10(lambda) per forecast horizon in ", model_name," | ",s, " | ",Sys.Date()),
#        subtitle = "Blue = lambda.min, Green = lambda.1se",
#        x = "log10(lambda)", y = "MSE") +
#   scale_color_manual(labels = c("lambda.min" ,"lambda.1se","lambda_opt"),
#                      values = c("red","steelblue","forestgreen"))+
#   theme_minimal()
print(plot)
dev.off()

#### QUANTILE BASED EVALUATION ----

#### GOF vs. Horizons Plot ----------
if (file.exists(paste0("results/",today,"/",model_name,"/GOF/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/GOF/")))
}

if (file.exists(paste0("results/",today,"/",model_name,"/hydrographs/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/hydrographs/")))
}
cum_seg_all %>% 
  filter(horizon==5) %>% 
  ggplot() +
  # geom_line(aes(x = exceed_prob, y = mean_pred, color = "Predicted Mean"))+
  # geom_line(aes(x = exceed_prob, y = mean_obs, color = "Observed Mean", linetype="dotted"))+
  # geom_line(aes(x = exceed_prob, y = RMSE, color = "RMSE"))+
  geom_line(aes(x = exceed_prob, y = r2, color = "r2"))+
  geom_line(aes(x = exceed_prob, y = r2_2, color = "r2_2"))

quantile_y_intercept <- quantiles_all %>% filter(exceed_prob %in% seq(10,100,10) & station == s) %>% pull(quant_flow)
ylims <- forecast_df_all %>% filter(date > start_date_GOF & station ==s) %>% pull(obs) %>% range()

p1 <- forecast_df_all %>%
  filter(lubridate::year(date) > 2015 & station ==s) %>%
  ggplot() +
  geom_line(aes(x = date, y = obs, color = "Observed Flow [m3/s]"), linewidth = 1) +
  geom_hline(yintercept = quantile_y_intercept, linetype = "dashed", aes(color = "Flow Quantiles [m3/s]",)) +
  scale_color_manual(values = c("Observed Flow [m3/s]" = "steelblue","Flow Quantiles [m3/s]" = "lightgrey")) +
  lims(y = ylims)+
  theme_bw() +
  theme(legend.position = "top")+
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Clean legend title
    title = paste0("Hydrographs and Quantile based RMSE visualization \nModel: ", model_name," | ",s," | ",fc_years)
    # subtitle = "Grey line indicates Observed Flow"
  ) 

p2 <- cum_seg_all %>%
  filter(horizon == 9 & station == s) %>%
  filter(exceed_prob %in% seq(10, 100, 10)) %>%
  arrange(quant_flow) %>%  # sort by y-axis variable
  ggplot() +
  geom_path(aes(y = quant_flow, x = RMSE)) +  # connects points in order of data
  geom_point(aes(y = quant_flow, x = RMSE)) +  # add points on top
  lims(y = ylims) +
  theme_bw() +
  theme(
    plot.margin = margin(5, 0, 5, 0),
    axis.text.y = element_text(),
    axis.ticks.y = element_line()
  )


combined <- p1 + p2 + plot_layout(ncol = 2, widths = c(5, 1))

png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_quantile_eval_hydrograph_",s,"_",fc_years,".png"),
    width = 1000, height = 600, units = "px")


print(combined)
dev.off()




##### Hydrographs of predicted vs. observed flows ---------

png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_hydrogaphs_facet_",s,"_",fc_years,".png"),
    width = 1000, height = 600, units = "px")


plot <- forecast_df_all %>% 
  filter(lubridate::year(date) == 2021) %>% 
  filter(station == s) %>% 
  ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast lines colored by horizon
  geom_line(linewidth = 1.2) +  # Slightly thicker forecast lines
  geom_line(aes(y = obs), color = "grey", linetype = "solid", linewidth = 1.2) +  # Darker grey for observed line
  facet_wrap(~horizon_char) +  # Facet by horizon, free y-axis
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Rename legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s," | ",fc_years),
    subtitle = "Grey line indicates Observed Flow"
  ) +
  geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted")+
  theme_light(base_size = 14) +  # Nicer base theme with larger default font size
  theme(
    legend.position = "right",   # Legend on the right
    legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.text = element_text(size = 12, color = "black"),  # Larger axis text
    axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
    strip.text = element_text(face = "bold", size = 13),  # Larger, bold facet labels
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered, bold title
    plot.subtitle = element_text(size = 12, hjust = 0.5),  # Centered subtitle
    panel.grid.minor = element_blank(),  # Optional: remove minor grids for cleaner look
    panel.grid.major = element_line(color = "grey85")  # Light major gridlines
  )

print(plot)


dev.off()


png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_hydrographs_stacked_",s,"_",fc_years,".png"),
    width = 1000, height = 600, units = "px")

plot <- forecast_df_all %>% 
  filter(lubridate::year(date) == 2015) %>% 
  filter(station == s) %>% 
  ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast colored by horizon
  geom_line(linewidth = 1.2, alpha = 0.9) +  # Thicker, slightly transparent forecast lines
  geom_line(aes(y = obs), color = "darkgrey", linetype = "solid", linewidth = 1.4, alpha = 1) +  # Dark grey observed line
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Clean legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s," | ",fc_years),
    subtitle = "Grey line indicates Observed Flow"
  ) +
  theme_light(base_size = 14) +
  geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted")+
  # Clean light theme with larger fonts
  theme(
    legend.position = "right",  # Keep legend on the right
    legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.text = element_text(size = 12, color = "black"),  # Larger axis labels
    axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
    strip.text = element_text(face = "bold", size = 13),  # If faceting is added later
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered, bold title
    plot.subtitle = element_text(size = 12, hjust = 0.5),  # Centered subtitle
    panel.grid.minor = element_blank(),  # Clean minor grids
    panel.grid.major = element_line(color = "grey85")  # Subtle major grid
  )

print(plot)

dev.off()




png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_hydrographs_stacked_",s,"_",fc_years,".png"),
    width = 1000, height = 600, units = "px")
test <- fc_df_quant_GOF %>% filter(station ==s & horizon ==1 & exceed_prob == 50)
plot <-
  fc_df_quant_GOF %>% 
  filter(station == s) %>% 
  ggplot() +
  geom_line(aes(x = exceed_prob, y = RMSE, color = fc_method), linewidth = 1)+
  facet_wrap(~horizon) +
  theme_bw()+
  lims(y = fc_df_quant_GOF %>% filter(station ==s) %>% pull(RMSE) %>% range())+
  labs(
    y = "RMSE [m³/s]",
    x = "Exceedance Probability [%]",
    color = "Forecasting Model",  # Clean legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s,"| ",fc_years)
    # subtitle = "Grey line indicates Observed Flow"
  ) +
  theme_light(base_size = 14) +
  geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted")+
  # Clean light theme with larger fonts
  theme(
    legend.position = "right",  # Keep legend on the right
    legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.text = element_text(size = 12, color = "black"),  # Larger axis labels
    axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
    strip.text = element_text(face = "bold", size = 13),  # If faceting is added later
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered, bold title
    plot.subtitle = element_text(size = 12, hjust = 0.5),  # Centered subtitle
    panel.grid.minor = element_blank(),  # Clean minor grids
    panel.grid.major = element_line(color = "grey85")  # Subtle major grid
  ) # Dark grey observed line


print(plot)

dev.off()


# PLOTTING STRATIFIED RMSE along horizons ----




png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_stratified_GOF_",s,"_",fc_years,".png"),
    width = 1000, height = 600, units = "px")

plot <- fc_df_quant_GOF %>% filter(exceed_prob %in% c(1,50,80,95) & station ==s) %>% 
  ggplot()+
  geom_line(aes(x=horizon, y = RMSE, color = fc_method),linewidth = 1) +
  theme_bw()+
  facet_wrap(~exceed_prob)+
  lims(y = fc_df_quant_GOF %>% filter(station ==s) %>% pull(RMSE) %>% range())+
  labs(
    y = "RMSE [m³/s]",
    x = "Forecasting Horizon",
    color = "Forecasting Model",  # Clean legend title
    title = paste0("RMSE along Forecasting Horizons for different Flow Quantiles \nModel: ", model_name," | ",s," | ",fc_years)
    # subtitle = "Grey line indicates Observed Flow"
  ) +
  theme_light(base_size = 14) +
  geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted")+
  # Clean light theme with larger fonts
  theme(
    legend.position = "right",  # Keep legend on the right
    legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.text = element_text(size = 12, color = "black"),  # Larger axis labels
    axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
    strip.text = element_text(face = "bold", size = 13),  # If faceting is added later
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered, bold title
    plot.subtitle = element_text(size = 12, hjust = 0.5),  # Centered subtitle
    panel.grid.minor = element_blank(),  # Clean minor grids
    panel.grid.major = element_line(color = "grey85")  # Subtle major grid
  ) # Dark grey observed line


print(plot)
dev.off()


#### Stratifized Error Measures ----

fc_df_quants <- NULL

for (q in c(1,50,80,95)) {
  
  th <- quantiles %>% filter(exceed_prob == q) %>% pull(quant_flow)
  temp <- forecast_df_long %>% filter(obs < q)
  temp$exceed_prob <- q
  fc_df_quants <- rbind(temp, fc_df_quants)
  




png(file=paste0("results/",today,"/",model_name,"/GOF/",model_name,"_residuals_boxplot_model_comparison_",s,"_",fc_years,".png"),
    width = 1000, height = 600, units = "px")

plot <- forecast_df_long_all %>% 
  filter(fc_method != "naive_season" & station ==s) %>% 
  filter(horizon %in% seq(1,12,2)) %>% 
  ggplot() +
  geom_boxplot(aes(x=as.factor(horizon), y = res,fill =fc_method), color = "black" )+
  # facet_wrap(~quantile)+
  labs(title = "Residuals per Forecasting Horizons",
       subtitle = paste0("Comparison of Naive Models with Elastic Net Model | Residuals = Predicted - Observed | ",s ," | ",fc_years),
       x = "Forecasting Horizons [weeks]",
       y = "Residuals [m^3/s]")+
  theme_bw()
print(plot)
dev.off()

}
# 
# png(file=paste0("results/",today,"/",model_name,"/GOF/",model_name,"_residuals_naive_models.png"),
#     width = 1000, height = 600, units = "px")
# 
# plot <- forecast_df_quant %>% 
#   filter(quantile == "Q90") %>%
#   ggplot() +
#   geom_point(aes(x=obs, y = res,color =fc_method))+
#   labs(title = "RMSE vs. Forecasting Horizons",
#        subtitle = "Comparison of Naive Models with Elastic Net Model",
#        x = "Forecasting Horizons",
#        y = "Residuals [m^3/s]")+
#   # facet_wrap(~quantile)+
#   theme_bw()
# print(plot)
# dev.off()
# 
# 
# ####
# h <- 9
# 
# forecast_df_quant %>% 
#   filter(lubridate::year(date) %in% 2018:2019) %>% 
#   filter(fc_method %in% c(
#     "naive_lag",
#     "catchment_memory",
#     "upper_benchmark"
#     # ,"naive_season"
#     
#   )) %>% 
#   filter(horizon == h) %>% 
#   
#   ggplot(aes(x = date, y = pred, color = factor(fc_method))) +  # Forecast colored by horizon
#   geom_line(size = 1, alpha = 0.9) +  # Thicker, slightly transparent forecast lines
#   geom_line(aes(y = obs), color = "black", linetype = "solid", size = 1.4, alpha = 1) +  # Dark grey observed line
#   labs(
#     y = "Monthly Low Flow [m³/s]",
#     x = "Date",
#     color = "Forecasting Horizon",  # Clean legend title
#     title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name),
#     subtitle = paste0("Grey line indicates Observed Flow | Horizon = ",h)
#   ) +
#   theme_light(base_size = 14) +
#   geom_hline(yintercept = quantiles$Q95, color = "grey", linetype = "dotted",linewidth = 1)+
#   # Clean light theme with larger fonts
#   theme(
#     legend.position = "bottom",  # Keep legend on the right
#     legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
#     legend.text = element_text(size = 12),  # Larger legend text
#     axis.text = element_text(size = 12, color = "black"),  # Larger axis labels
#     axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
#     strip.text = element_text(face = "bold", size = 13),  # If faceting is added later
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered, bold title
#     plot.subtitle = element_text(size = 12, hjust = 0.5),  # Centered subtitle
#     panel.grid.minor = element_blank(),  # Clean minor grids
#     panel.grid.major = element_line(color = "grey85")  # Subtle major grid
#   )


#### Continuous Ranked Probability Score CRPS ----

#plotting distribution of residuals 

fc_df_quants %>%
  filter(horizon == 1) %>% 
  ggplot()+
  geom_histogram(aes(x=res),color = "black",fill = "steelblue")+
  facet_wrap(~fc_method)+
  geom_vline(xintercept = 0, color = "darkred")

fc_df_quants %>%
  filter(horizon == 1) %>% 
  ggplot()+
  stat_ecdf(aes(x=res,color = fc_method),fill = "steelblue",geom="step",size = 1.5)+
  # facet_wrap(~fc_method)+
  geom_vline(xintercept = 0, color = "darkred")

fc_df_quants %>%
  filter(horizon == 1) %>% 
  ggplot()+
  stat_ecdf(aes(x=obs),color = "steelblue",geom="step",size = 1.5)+
  # facet_wrap(~fc_method)+
  geom_vline(xintercept = 0, color = "darkred")+
  geom_vline(xintercept = c(quantiles$Q95[1],quantiles$Q90[1],quantiles$Q50[1]))+
  scale_y_continuous(breaks = seq(0,100,25))+
  labs(x= "Flow [m^3/s ]",
       y = "Non-Exceedance Probability [%]",
       title = "")

fc_df_quants %>%
  filter(horizon == 1) %>% 
  ggplot()+
  geom_point(aes(x=obs,y=res),fill = "steelblue")+
  # facet_wrap(~fc_method)+
  geom_vline(xintercept = 0, color = "darkred")



#### Summary Table ----------
# 
# summary_table <- fc_df_quant %>% 
#   # filter(quantile == "all_data") %>% 
#   mutate(RMSE_scaled_season = (RMSE))
#   select(c("horizon","R2","RMSE","RMSE_scaled_season","RMSE_scaled_lag"))
# 
# n_coefs <- rownames_to_column(n_coefs)
# 
# names(n_coefs) <- c("horizon", "n_coefs")
# 
# n_coefs$horizon <- gsub("fc_horizon_", "", n_coefs$horizon)
# 
# n_coefs$horizon <- as.numeric(n_coefs$horizon)
# 
# summary_table$horizon <- as.numeric(summary_table$horizon)
# 
# summary_table <- merge(summary_table, n_coefs, by = "horizon")
# 
# # summary_table$lambda.applied <- summary_table$lambda.value[1]
# setcolorder(summary_table, c("horizon", "R2", "RMSE", "RMSE_scaled_season","RMSE_scaled_lag", 
#                              # "lambda.applied",
#                              "n_coefs"))
# 
# # Create a basic gt table
# gt_table <- gt(summary_table) %>%
#   tab_header(
#     title = "Summary Statistics of Monthly Forecasting Performance",
#     subtitle = paste0("Model: ", model_name," ",as.Date(Sys.time()))
#   ) %>%
#   fmt_number(
#     columns = c(R2, RMSE, RMSE_scaled_season,RMSE_scaled_lag),
#     decimals = 2
#   ) %>%
#   cols_label(
#     horizon = md("Forecasting<br>Horizon<br>(weeks)"),
#     R2 = md("R²"),
#     RMSE = md("RMSE"),
#     # lambda.value = md("Optimal<br>Lambda"),
#     # lambda.applied = md("Applied<br>Lambda"),
#     n_coefs = md("Optimal Number<br>of Coefficients"),
#     RMSE_scaled_season = md("RMSE_scaled_season"),
#     RMSE_scaled_lag = md("RMSE_scaled_lag")
#   ) %>%
#   tab_style(
#     style = list(
#       cell_text(weight = "bold")
#     ),
#     locations = cells_column_labels(everything())
#   ) 
# # tab_footnote(
# #   footnote = "For forcasting of all months, the paramters for lamdba and number of coefficients of the forecasting horizon = 1 and not the optimal values were applied.",
# #   locations = cells_title(groups = "title")
# # )
# print(gt_table)
# gtsave(gt_table, paste0("results/",today,"/",model_name,"/summary_table_forecasts.png"))

}


# HISTORIC DROUGHT EVALUTATION ----

test <- hist_fc_df_all %>% filter(station == "Tauchenbach" &
                                    horizon == 1 &
                                    fc_method == "observed_flow"&
                                    fc_period == "2015_2016")
hist_fc_df_all %>% setDT() %>% 
  .[, {
    # data <- .SD
    data <- test
    
    
    
  }]


