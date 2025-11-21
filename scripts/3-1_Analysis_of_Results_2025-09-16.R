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
library(ggnewscale)
rm(list=ls())

# today <- Sys.Date()-5
today <- as.Date("2025-11-14")
date_BM_models <- "2025-10-03"
model_name <- c("sqrt_interaction_no_seas")
dataset <- "lagged_TB" #TB = Tauchenbach
fc_years_list <- list(
  2015:2016
                        ,2003:2004
                        ,2015:2021
)
# 
# ensemble_path <- "results/2025-10-03/Ensemble_Model/2015_2016/Ensemble_Model_forecasts_list_"
# Upper_BM_path <- "results/2025-10-03/Climate_Upper_BM/2015_2016/Climate_Upper_BM_forecasts_list_"

# LOADING DATAFRAMES OF ALL MODELS AND FC-PERIODS --------------


ensemble_df_all <- NULL
upper_BM_df_all <- NULL
forecast_df_all <- NULL
n_coefs_all <- NULL
coefs_means_all <- NULL
coefs_sd_all <- NULL
coefs_df_all <- NULL
lambda_df_all <- NULL
GOF_df_all <- NULL

for (fc_years in fc_years_list) {
  
   # fc_years <- 2015:2016
  
  fc_years_text <- paste0(min(fc_years),"_",max(fc_years))
  
  load(paste0("results/",date_BM_models,"/Ensemble_Model/",fc_years_text,"/Ensemble_Model_forecasts_list_",fc_years_text,".RData"))
# load(paste0("results/","2025-10-03","/sinus_1se_lags_seas/","2015_2021","/sinus_1se_lags_seas_forecasts_list_","2015_2021",".RData"))

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
}) %>% mutate(fc_period = fc_years_text) %>%  rbind(.,ensemble_df_all)


ensemble_df_all$station[ensemble_df_all$station == "tauchenbach"] <- "Tauchenbach"
ensemble_df_all$station[ensemble_df_all$station == "kienstock"] <- "Kienstock"
ensemble_df_all$station[ensemble_df_all$station == "flattach"] <- "Flattach"
ensemble_df_all$station[ensemble_df_all$station == "uttendorf"] <- "Uttendorf"

load(paste0("results/",date_BM_models,"/Climate_Upper_BM/",fc_years_text,"/Climate_Upper_BM_forecasts_list_",fc_years_text,".RData"))
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
})%>% mutate(fc_period = fc_years_text) %>%  rbind(.,upper_BM_df_all)

upper_BM_df_all$station[upper_BM_df_all$station == "tauchenbach"] <- "Tauchenbach"
upper_BM_df_all$station[upper_BM_df_all$station == "kienstock"] <- "Kienstock"
upper_BM_df_all$station[upper_BM_df_all$station == "flattach"] <- "Flattach"
upper_BM_df_all$station[upper_BM_df_all$station == "uttendorf"] <- "Uttendorf"


load(paste0("results/",today,"/",model_name,"/",fc_years_text,"/",model_name,"_coefs_global_list_",fc_years_text,".RData"))
load(paste0("results/",today,"/",model_name,"/",fc_years_text,"/",model_name,"_forecasts_list_",fc_years_text,".RData"))
load(paste0("results/",today,"/",model_name,"/",fc_years_text,"/",model_name,"_final_model_list_",fc_years_text,".RData"))
load(paste0("results/",today,"/",model_name,"/",fc_years_text,"/",model_name,"_GOF_list_",fc_years_text,".RData"))
load(paste0("results/",today,"/",model_name,"/",fc_years_text,"/",model_name,"_n_coefs_list_",fc_years_text,".RData"))
load(paste0("results/",today,"/",model_name,"/",fc_years_text,"/",model_name,"_results_",fc_years_text,".RData"))

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
}) %>% mutate(fc_period = fc_years_text) %>%  rbind(.,GOF_df_all)


forecast_df_all <-   purrr::imap_dfr(forecasts_list, ~ { 
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    df <- .x
    
    df <- df %>% mutate(station = station, horizon = horizon, 
                        horizon = horizon,horizon_char = horizon_char,
                        fc_method = "fc_model")
    
  })
}) %>% mutate(fc_period = fc_years_text) %>%  rbind(.,forecast_df_all)

coefs_list_final <- list()

cm_list <- list()

n_horizons <- as.numeric(length(unique(names(forecasts_list))))

if (file.exists(paste0("results/",today,"/",model_name,"/coefficients/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/coefficients/")))
}


if (file.exists(paste0("results/",today,"/",model_name,"/coefficients/",fc_years_text))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/coefficients/",fc_years_text)))
}

if (file.exists(paste0("results/",today,"/",model_name,"/coefficients_over_lambdas/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/coefficients_over_lambdas/")))
}

if (file.exists(paste0("results/",today,"/",model_name,"/coefficients_over_lambdas/",fc_years_text))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/coefficients_over_lambdas/",fc_years_text)))
}

n_coefs_all <-   purrr::imap_dfr(n_coefs_list, ~ { 
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    df <- .x
    
    df <- df %>% mutate(station = station,  
                        horizon_char = horizon_char,
                        fc_method = "fc_model",
                        fc_period = fc_years_text) %>% 
      rename(n_coefs = ".")
    
  })
})%>% mutate(fc_period = fc_years_text) %>%  rbind(.,n_coefs_all)



# LOAD PREDICTOR COEFFICIENTS ---------

coefs_df_all <- purrr::imap_dfr(coefs_global_list, ~ {
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    
    df <- .x
    df <- df %>% 
      rownames_to_column(var = "variable") %>%  # explizit "variable" nennen
      rename(coefs = ".") %>%                   # "." Spalte in coefs umbenennen
      mutate(
        horizon_char = horizon_char,
        station = station,
        fc_period = fc_years_text
      )
    
    df
  })
})%>% mutate(fc_period = fc_years_text) %>%  rbind(.,coefs_df_all)



coefs_means_all <- purrr::imap_dfr(
  results,
  ~ {
    station <- .y
    
    purrr::imap_dfr(.x, ~ {
      tibble(
        term = names(.x$coef_local_means),
        estimate = .x$coef_local_means,
        fc_horizon = as.integer(gsub("fc_horizon_", "", .y))) %>% mutate(station = station,
                                                                         fc_period = fc_years_text) %>% 
        rename(mean_coef_CV = estimate, variable = term)
      
    }
    
    )
  }
)%>% mutate(fc_period = fc_years_text) %>%  rbind(.,coefs_means_all)

coefs_sd_all <- purrr::imap_dfr(
  results,
  ~ {
    station <- .y
    
    purrr::imap_dfr(.x, ~ {
      tibble(
        term = names(.x$coef_local_sd),
        estimate = .x$coef_local_sd,
        fc_horizon = as.integer(gsub("fc_horizon_", "", .y))) %>% mutate(station = station,
                                                                         fc_period = fc_years_text) %>% 
        rename(sd_coef_CV = estimate, variable = term)
      
    }
    
    )
  }
)%>% mutate(fc_period = fc_years_text) %>%  rbind(.,coefs_sd_all)



lambda_df_all <- purrr::imap_dfr(results, ~ {
  
  station <- .y
  
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    
    
    
    df <- .x$global_optimization_results
    
    df <- df %>% mutate(horizon_char = horizon_char,
                        station = station,
                        fc_period = fc_years_text) 
    
  })
})%>% mutate(fc_period = fc_years_text) %>%  rbind(.,lambda_df_all)


}


coefs_df_all$fc_horizon <- stringr::str_remove(coefs_df_all$horizon, "fc_horizon_") %>%
  as.numeric()

coefs_df_all$n_horizon <- coefs_df_all$fc_horizon %>% 
  factor(levels = sort(unique(.)))

coefs_df_all$variable <- as.factor(coefs_df_all$variable)


coefs_df_all <- left_join(coefs_df_all, coefs_means_all, by = c("variable", "fc_horizon","station","fc_period"))
coefs_df_all <- left_join(coefs_df_all, coefs_sd_all, by = c("variable", "fc_horizon","station","fc_period"))

coefs_df_all$coefs_ub <-  coefs_df_all$mean_coef_CV + coefs_df_all$sd_coef 
coefs_df_all$coefs_lb <-  coefs_df_all$mean_coef_CV - coefs_df_all$sd_coef 


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
forecast_df_complete <- NULL
for (fc_years in fc_years_list) {
  
  fc_years_text <- paste0(min(fc_years),"_",max(fc_years))
  
    for(s in unique(forecast_df_all$station)){
      # 
      # s <- "Uttendorf"
      forecast_df <- forecast_df_all %>% filter(station ==s & fc_period == fc_years_text)
      start_date_GOF <- forecast_df %>% 
        filter(horizon == as.numeric(max(forecast_df$horizon)) & station == s) 
    
      start_date_GOF <- start_date_GOF$date[1]
    
      load(file =  paste0("data/",s,"/Final_df_",s,"_weekly.RData"))
      
      naive_season <- df %>%   filter(!lubridate::year(Date) %in% fc_years) %>%
        select(c("Date","flow_mean","week_year")) %>%  rename(date = Date, flow = flow_mean)
      
      naive_season <- naive_season %>%  setDT() %>% .[, naive_season := mean(flow, na.rm = T), by = week_year]
    
        
      quantiles <- df %>% filter(!lubridate::year(Date) %in% fc_years) %>% pull("flow_mean") 

      quantiles <- sapply(1:100, function(x) quantile(quantiles, probs = 1-(x/100))) %>% 
        data.frame(quant_flow = ., exceed_prob = 1:100)
      
      quantiles$fc_period <- fc_years_text
      
      # mean_obs_tot <- df %>%   filter(Date > start_date_GOF) %>% pull(flow_mean) %>% 
      #   mean()
      cum_seg <- NULL
      for(q in 1:100){
        
        # q <- 90
        
        cum_seg_th <- quantiles %>% filter(q == exceed_prob) %>% pull(quant_flow)
        cum_seg <- forecast_df %>% filter(obs < cum_seg_th & station == s)
        cum_seg <- cum_seg %>% setDT() %>% 
          .[date > start_date_GOF, {
            MARE <- mean((abs(pred-obs))/obs)
            # mean_pred <- mean(pred)
            # mean_obs_tot <- mean_obs_tot
            # mean_obs_seg <- mean(obs)
            # ssr <- sum((pred - mean_obs_tot)^2)
            # sstot <- sum((obs - mean_obs_tot)^2)
            # r2 <- 1-(ssr / sstot)
            RMSE = as.numeric(hydroGOF::rmse(sim = pred, obs = obs))
            # r2_2 = hydroGOF::R2(sim = pred, obs = obs)
            
            .(
              # r2= r2,r2_2 =r2_2 ,mean_obs_tot = mean_obs_tot, mean_pred = mean_pred, mean_obs_seg = mean_obs_seg,
              RMSE= RMSE, 
              MARE = MARE)
          }, by = horizon]
        
        cum_seg$exceed_prob <- q
        cum_seg$station <- s
        cum_seg$fc_period <- fc_years_text
        
        
        cum_seg_all <- rbind(cum_seg, cum_seg_all)
      
      }
    
        ## Adding other models forecasts (naives, ensemble , upperBM) ----
        
        forecast_df  <- df %>% rename(date = Date) %>%  select(c("week_year","date")) %>% 
          left_join(forecast_df,.,by = "date")
        
        forecast_df <- naive_season %>%
          distinct(week_year, .keep_all = TRUE) %>%
          select(week_year, naive_season) %>%
          left_join(forecast_df,., by = "week_year")
        
        forecast_df <- ensemble_df_all %>% filter(ensemble == "ensemble_median" & station == s & fc_period == fc_years_text) %>%  
          select(c("date","pred","horizon","fc_period")) %>% 
          rename(ensemble_median = pred) %>% 
          left_join(forecast_df,.,by= (c("date","horizon","fc_period")))
        
        forecast_df <- upper_BM_df_all %>%  filter(station == s) %>% 
          select(c("date","pred","horizon","fc_period")) %>% 
          rename(upper_benchmark = pred) %>% 
          left_join(forecast_df,.,by= (c("date","horizon","fc_period")))
        
        naive_lag <- df %>% select(flow_mean,Date) %>%
          rename(obs = flow_mean, date = Date) %>%
          mutate(fc_period = fc_years_text) %>% 
          tidyr::crossing(.,horizon = 1:12)
        naive_lag <- naive_lag %>% arrange(horizon, date) %>% 
          setDT() %>% 
          .[, naive_lag := lag(obs, horizon), by = .(horizon)]
        
        forecast_df <- naive_lag %>% select(-obs) %>% left_join(forecast_df, ., by = c("date","horizon","fc_period"))
        
        
        forecast_df$station <- s
        temp <- rbind(forecast_df,temp)
        forecast_df_complete <- rbind(forecast_df_complete,forecast_df)
        quantiles$station <- s
        quantiles_all <- rbind(quantiles_all, quantiles)
    
    
        
        forecast_df_long <- temp %>% rename(catchment_memory_model = pred) %>% 
          select(-"fc_method") %>% pivot_longer(cols = c("naive_season","upper_benchmark","ensemble_median",
                                                         "catchment_memory_model","naive_lag"), values_to = "pred",
                                                names_to = "fc_method")  
        
        forecast_df_long$res <- forecast_df_long$pred - forecast_df_long$obs
        
        forecast_df_long_all <- rbind(forecast_df_long_all, forecast_df_long)
        
        ## Adding quantile predictions ----
        for(q in 1:100){
          
          # q <- 51
          fc_df_quant <- NULL
          cum_seg_th <- quantiles_all %>% filter(q == exceed_prob  & station ==s & fc_period == fc_years_text) 
          fc_df_quant <- forecast_df_long %>% filter(obs < cum_seg_th$quant_flow & station ==s & fc_period == fc_years_text)
          
          fc_df_quant <- fc_df_quant %>% setDT() %>% 
            .[date > start_date_GOF, {
              MARE <- mean((abs(pred-obs))/obs)
              # mean_pred <- mean(pred, na.rm = T)
              # mean_obs_tot <- mean_obs_tot
              mean_obs_seg <- mean(obs, na.rm = T)
              # ssr <- sum((pred - mean_obs_tot)^2)
              # sstot <- sum((obs - mean_obs_tot)^2)
              # r2 <- 1-(ssr / sstot)
              RMSE = as.numeric(hydroGOF::rmse(sim = pred, obs = obs))
              RRMSE_seg = RMSE/mean_obs_seg
              # r2_2 = hydroGOF::R2(sim = pred, obs = obs)
              
              .( # r2= r2,r2_2 =r2_2 ,mean_obs_tot = mean_obs_tot, mean_pred = mean_pred, mean_obs_seg = mean_obs_seg,
                RMSE= RMSE, 
                RRMSE_seg = RRMSE_seg,
                MARE = MARE)
            }, by = .(horizon, fc_method, station,fc_period)]
          
          fc_df_quant$exceed_prob <- q
          fc_df_quant$station <- s
          fc_df_quant$fc_period <- fc_years_text
          fc_df_quant_GOF <- rbind(fc_df_quant, fc_df_quant_GOF)
          
        }
    }
}

forecast_df_all <- temp
save(forecast_df_complete, file = (paste0("results/",today,"/",model_name,"/forecast_df_complete.RData")))

cum_seg_all <- left_join(cum_seg_all , quantiles_all, by = c("exceed_prob","station","fc_period"))

# 
# test <- forecast_df_long %>% filter(station ==s & fc_period == fc_years_text & fc_method == "naive_season")
# 
# test <- test %>% setDT() %>% .[, .(n = .N), by = .(horizon)]
# 

save(quantiles_all, file = (paste0("results/",today,"/",model_name,"/quantiles_all.RData")))

### define colors for models, catchments, variables ----
colors_models <- thematic::okabe_ito(8)

# color_legend <- data.frame(y = rep(1,8), x = 1:8, c = c("a","b","c","d","e","f","g","h"))
# color_legend %>% 
#   ggplot()+
#   geom_col(aes(x = x, y = y, fill = c))+
#   scale_fill_manual(values = c("a" = colors_models[1],
#                     "b" = colors_models[2],
#                     "c" = colors_models[3],
#                     "d" = colors_models[4],
#                     "e" = colors_models[5],
#                     "f" = colors_models[6],
#                     "g" = colors_models[7],
#                     "h" = colors_models[8]))


models_color_code <- c("catchment_memory_model"  = colors_models[1],
                       "upper_benchmark" = colors_models[2],
                       "ensemble_median" = colors_models[6],
                       "naive_season" =  colors_models[4],
                       "naive_lag" = colors_models[5])
models_labels <- c("catchment_memory_model"  = "Catchment Memory",
                   "upper_benchmark" = "Upper Benchmark",
                   "ensemble_median" = "Ensemble Model",
                   "naive_season" =  "Naive Seasonal",
                   "naive_lag" = "Naive Lag")

pred_obs_color_code <- c("Forecasted"  = colors_models[1],
                         "Observed" = colors_models[3])


# START LOOP FOR PLOTTING ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
for (fc_years in fc_years_list) {
    
    fc_years_text <- paste0(min(fc_years),"_",max(fc_years))
    
    coefs_df_all <- coefs_df_all %>% setDT() %>% 
      .[variable != "(Intercept)", c(.SD,.(coefs_rel = (abs(coefs)*100)/(sum(abs(coefs))) ) ), 
        by = .(station, fc_period,fc_horizon)]

    top_coefs_all <- coefs_df_all %>% setDT() %>% 
      .[variable != "(Intercept)", .(coefs = sum(abs(coefs))), by = .(fc_period,variable,station)]
    
    coefs_df_all <- coefs_df_all[, !duplicated(names(coefs_df_all)), with = FALSE]
    
    top_coefs_real <- coefs_df_all %>% filter(variable != "(Intercept)", n_horizon == 1) %>% 
      group_by(fc_period, station) %>% 
      slice_max(order_by =  coefs, n = 10) 
      
    
    ## Plot Model Realism (only horizon == 1) ----
    
    png(file=paste0("results/",today,"/",model_name,"/coefficients/",fc_years_text,"/",model_name,"_model_realism_",fc_years_text,"_relative.png"),
        width = 1000, height = 600, units = "px")
    
    plot <- top_coefs_real %>%
      filter(fc_period == fc_years_text) %>% 
      filter(coefs > 0) %>% 
      ggplot(aes(x = coefs, y = reorder(variable, coefs_rel), fill = n_horizon)) +
      geom_col(color = "black", width = 0.9) +
      # geom_errorbarh(aes(xmin = coefs_lb, xmax = coefs_ub), height = 0.3) +
      facet_wrap(~station, scales = "free", ncol = 2, nrow = 2) +
      # ylim(0,100)+
      labs(
        x = "Relative Importance [%]",
        y = "",
        fill = "Forecasting Horizon",
        title = paste0("Station: ",s," | Relative Importance of 20 Most Important Predictor Variables in ",model_name,"_",fc_years_text),
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
    
    print(plot)
    dev.off()
    
    for (s  in unique(forecast_df_all$station)) {
      
    
    ## Plotting Coefficients ----

    coefs_df <- coefs_df_all %>% filter(station ==s & fc_period == fc_years_text)
    
   top_coefs_all <- coefs_df_all %>% setDT() %>% 
      .[variable != "(Intercept)", .(coefs = sum(abs(coefs))), by = .(fc_period,variable,station)]
    
    top_coefs <- top_coefs_all %>% filter(station ==s & fc_period == fc_years_text) %>%  arrange(desc(coefs))
    
    top_coefs <- top_coefs$variable[1:15]
    
    png(file=paste0("results/",today,"/",model_name,"/coefficients/",fc_years_text,"/",model_name,"_coefs_bars_all_fc-h_",s,"_",fc_years_text,".png"),
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
        title = paste0("Station: ",s," | Absolute Importance of 20 Most Important Predictor Variables in ",model_name,"_",fc_years_text),
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
    
    ## Plot relative predictor importance ----
    
    png(file=paste0("results/",today,"/",model_name,"/coefficients/",fc_years_text,"/",model_name,"_coefs_bars_all_fc-h_",s,"_",fc_years_text,"_relative.png"),
        width = 1000, height = 600, units = "px")
    
    plot <- coefs_df %>%
      filter(station == s) %>% 
      filter(variable %in% top_coefs) %>%
      mutate(variable = factor(variable, levels = top_coefs)) %>% 
      ggplot(aes(x = n_horizon, y = coefs_rel, fill = n_horizon)) +
      geom_col(color = "black", width = 0.9) +
      facet_wrap(~variable, scales = "fixed", ncol = 5, nrow = 3) +
      # ylim(0,100)+
      labs(
        x = "Forecasting Horizon",
        y = "Relative Importance [%]",
        fill = "Forecasting Horizon",
        title = paste0("Station: ",s," | Relative Importance of 20 Most Important Predictor Variables in ",model_name,"_",fc_years_text),
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
    
    png(file=paste0("results/",today,"/",model_name,"/coefficients/",fc_years_text,"/",model_name,"_coefs_lines_shadings",s,"_",fc_years_text,".png"),
        width = 1000, height = 600, units = "px")
    
    
    plot <-   coefs_df_all %>%
      filter(variable %in% top_coefs[1:16] & station ==s & fc_period == fc_years_text) %>%
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
           subtitle = paste0("Model: ", model_name, " from: ", Sys.Date()," station = ",s,"_",fc_years_text))+
      scale_x_continuous(breaks = seq(1,12,1))
  print(plot)
    dev.off()

    png(file=paste0("results/",today,"/",model_name,"/coefficients/",fc_years_text,"/",model_name,"_coefs_error_bars_",s,"_",fc_years_text,".png"),
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
        subtitle = paste0("Model: ", model_name, " from: ", Sys.Date()," station = ",s,"_",fc_years_text)
      ) +
      theme_bw() +                                                            
      theme(
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "none")
  print(plot)
    dev.off()
    
    ### Predictor Coefs along lambda - line plot ----
    
    for(h in unique(forecast_df$horizon)){
      
      results[[paste0(s)]][[paste0("fc_horizon_",h)]][["params"]]$lambda.1se
      model <- final_model_list[[paste0(s)]][[paste0("fc_horizon_",h)]]
      
      betas <- as.matrix(model$beta)
      lambdas = model$lambda
      names(lambdas) = colnames(betas) 
      coefs_lambdas <-  as.data.frame(betas) %>%
        tibble::rownames_to_column("variable") %>%
        pivot_longer(-variable) %>%
        mutate(lambda=lambdas[name])
      
      line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "solid", "dashed", "dotted", "dotdash",
                      "longdash", "twodash", "solid", "dashed", "dotted")
      
      lambda.1se <- log10(results[[paste0(s)]][[paste0("fc_horizon_", h)]][["params"]]$lambda.1se)
      lambda.min <- log10(results[[paste0(s)]][[paste0("fc_horizon_", h)]][["params"]]$lambda_opt)
      
      
      # PLOT ONLY THE TOP 15 PREDICTORS 
      png(file=paste0("results/",today,"/",model_name,"/coefficients_over_lambdas/",fc_years_text,"/top_coefs_lambdas_",s,"_",fc_years_text,".png"),
          width = 1000, height = 600, units = "px")
      
      plot <- coefs_lambdas %>% 
        filter(variable %in% top_coefs) %>%
        ggplot(aes(x = log10(lambda), y = value, color = variable)) +
        geom_line(size = 1.2) +
        scale_y_continuous(limits = c(min(coefs_lambdas$value), max(coefs_lambdas$value))) +
        theme_light(base_size = 18) +
        ggtitle(paste0("Coefficients of Top 15 Predictors vs. Lambda in ", 
                       model_name, " FC-Horizon = ", h)) +
        xlab("log10 (Lambda)") +
        ylab("Predictor Coefficient") +
        
        # First legend: predictor variables
        # scale_color_manual(values = setNames(rainbow(length(top_coefs)), top_coefs),
        #                    name = "Predictors") +
        
        # Reset scale so the next color mapping gets its own legend
        new_scale_color() +
        
        # Second legend: vertical lines
        geom_vline(aes(xintercept = lambda.1se, color = "Lambda 1SE"), linetype = "dashed", linewidth = 1) +
        geom_vline(aes(xintercept = lambda.min, color = "Lambda Min"), linetype = "dashed", linewidth = 1) +
        scale_color_manual(values = c("Lambda 1SE" = "steelblue", "Lambda Min" = "darkred"),
                           name = "Reference Lines") +
        
        theme(
          legend.position = "bottom",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(hjust = 1),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)
        )
      
      print(plot)
      dev.off()
      
      # PLOT ALL PREDICTORS 
      png(file=paste0("results/",today,"/",model_name,"/coefficients_over_lambdas/",fc_years_text,"/coefs_lambdas_",s,"_",fc_years_text,".png"),
          width = 1000, height = 600, units = "px")
      
      plot <- coefs_lambdas %>% 
        ggplot(aes(x = log10(lambda), y = value, color = variable)) +
        geom_line(size = 1.2, show.legend = F) +
        scale_y_continuous(limits = c(min(coefs_lambdas$value), max(coefs_lambdas$value))) +
        theme_light(base_size = 18) +
        ggtitle(paste0("Coefficients of Top 15 Predictors vs. Lambda in ", 
                       model_name, " FC-Horizon = ", h)) +
        xlab("log10 (Lambda)") +
        ylab("Predictor Coefficient") +
        
        # First legend: predictor variables
        # scale_color_manual(values = setNames(rainbow(length(top_coefs)), top_coefs),
        #                    name = "Predictors") +
        
        # Reset scale so the next color mapping gets its own legend
        new_scale_color() +
        
        # Second legend: vertical lines
        geom_vline(aes(xintercept = lambda.1se, color = "Lambda 1SE"), linetype = "dashed", linewidth = 1) +
        geom_vline(aes(xintercept = lambda.min, color = "Lambda Min"), linetype = "dashed", linewidth = 1) +
        scale_color_manual(values = c("Lambda 1SE" = "steelblue", "Lambda Min" = "darkred"),
                           name = "Reference Lines") +
        
        theme(
          legend.position = "bottom",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(hjust = 1),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)
        )
      print(plot)
      dev.off()
      
      
    }
    
  #### LAMBDA vs. RMSE Plots ----
  
  
  lambda_df <- lambda_df_all %>% filter(station == s & fc_period == fc_years_text)
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
  
  for(h in unique(as.numeric(lambda_min$fc_horizon))){
    
    temp <- lambda_df[which(lambda_df$fc_horizon == h),]
    # if()
    #add lambda_opt
    lambda.opt[h] <- results[[paste0(s)]][["fc_horizon_1"]][["params"]][["lambda_opt"]]
    
    temp <- temp[temp$MSE <= lambda_min$threshold[which(lambda_min$fc_horizon == h)],]
    lambda.1se_graf[h] <- temp$lambda[which.max(temp$lambda)]
    
  }
  lambda.1se_graf <- lambda.1se_graf %>% na.omit()
  lambda.opt <- lambda.opt %>% na.omit()
  
  lambda_1se_df <- tibble(
    fc_horizon = lambda_min$fc_horizon,
    lambda_1se = lambda.1se_graf,
    lambda.opt = lambda.opt)
  
  lambda_lines  <- lambda_min %>%
    select(fc_horizon, lambda_min) %>%
    left_join(lambda_1se_df, by = "fc_horizon")
  
  lambda_df$MSE_lb <- lambda_df$MSE-lambda_df$MSESD
  lambda_df$MSE_ub <- lambda_df$MSE+lambda_df$MSESD
  
  
  if (file.exists(paste0("results/",today,"/",model_name,"/lambda_selection/"))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/lambda_selection/")))
  }
  
  if (file.exists(paste0("results/",today,"/",model_name,"/lambda_selection/",fc_years_text))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/lambda_selection/",fc_years_text)))
  }
  
  png(file=paste0("results/",today,"/",model_name,"/lambda_selection/",fc_years_text,"/lambda_selection_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  lambda_lines_long <- lambda_lines %>%
    pivot_longer(cols = c(lambda_min, lambda_1se, lambda.opt), 
                 names_to = "lambda_type", 
                 values_to = "lambda_value") %>%
    mutate(color_label = case_when(
      lambda_type == "lambda_min" ~ "lambda.min",
      lambda_type == "lambda_1se" ~ "lambda.1se",
      lambda_type == "lambda.opt" ~ "lambda.opt"
    ))
  
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
      title = paste0("MSE across log10(lambda) per forecast horizon in ", model_name," | ",s,"_",fc_years_text, " | ",Sys.Date()),
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
  
  #### Overall GOF measures ----
  
  if (file.exists(paste0("results/",today,"/",model_name,"/GOF/"))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/GOF/")))
  }
  if (file.exists(paste0("results/",today,"/",model_name,"/GOF/",fc_years_text))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/GOF/",fc_years_text)))
  }
  
  png(file=paste0("results/",today,"/",model_name,"/GOF/",fc_years_text,"/",model_name,"overall_GOF_MARE",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")

  plot <- fc_df_quant_GOF %>% filter(station == s & fc_period == fc_years_text & exceed_prob == 99) %>% 
    ggplot()+
    geom_line(aes(x=horizon, y = MARE, color = fc_method),linewidth = 1)+
    theme_bw()+
    scale_x_continuous(breaks = seq(1,12,1))+
    scale_color_manual(values = models_color_code,
                       labels = models_labels)+
    labs(x= "Lead Time [weeks]",color = "Forecasting Method", 
         y = "MARE", 
         title = paste0("Mean Absolute Relative Error | Station ",s))
  print(plot)
  dev.off()  
 
  
  
  png(file=paste0("results/",today,"/",model_name,"/GOF/",fc_years_text,"/",model_name,"overall_GOF_RMSE",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  plot <- fc_df_quant_GOF %>% filter(station == s & fc_period == fc_years_text & exceed_prob == 99) %>% 
    ggplot()+
    geom_line(aes(x=horizon, y = RMSE, color = fc_method),linewidth = 1)+
    theme_bw()+
    labs(x= "Lead Time [weeks]",color = "Forecasting Method", y = "RMSE", title = paste0("RMSE | Station ",s))+
    scale_color_manual(values = models_color_code, labels = models_labels)+
    scale_x_continuous(breaks = seq(1,12,1))
  
  print(plot)
  dev.off() 
  
  # png(file=paste0("results/",today,"/",model_name,"/GOF/",fc_years_text,"/",model_name,"MARE_stations_comparison",s,"_",fc_years_text,".png"),
  #     width = 1000, height = 600, units = "px")
  
  # plot <- fc_df_quant_GOF %>% filter(fc_period == fc_years_text & exceed_prob == 1 & 
  #                                      fc_method == "ensemble_median") %>% 
  #   ggplot()+
  #   geom_line(aes(x=horizon, y = MARE, color = station),linewidth = 1)+
  #   theme_bw()+
  #   labs(x= "Lead Time [weeks]", y = "MARE", title = paste0("MARE vs. Lead Time | Station ",s))+
  #   scale_x_continuous(breaks = seq(1,12,1))
  # print(plot)
  # dev.off() 
  # 
  
  #### Quantile based residuals ----
  
  
  if (file.exists(paste0("results/",today,"/",model_name,"/hydrographs/"))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/hydrographs/")))
  }
  if (file.exists(paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text)))
  }
  
  quantile_y_intercept <- quantiles_all %>% 
    filter(exceed_prob %in% seq(10,100,10) & station == s & fc_period ==fc_years_text) %>% 
    pull(quant_flow)
  
  Q80_y_intercept <- quantiles_all %>% 
    filter(exceed_prob == 80 & station == s & fc_period ==fc_years_text) %>% 
    pull(quant_flow)
  ylims <- forecast_df_all %>% filter(date > start_date_GOF & station ==s) %>% pull(obs) %>% range()
  
  p1 <- forecast_df_all %>%
    filter(lubridate::year(date) %in% fc_years & station ==s & fc_period == fc_years_text) %>%
    ggplot() +
    geom_line(aes(x = date, y = obs, color = "Observed Flow [m3/s]"), linewidth = 1) +
    geom_hline(data = data.frame(y=quantile_y_intercept),aes(yintercept = y, color = "Flow Quantiles [m3/s]"),
               linetype = "dashed", linewidth = .8) +
    geom_hline(data = data.frame(y=Q80_y_intercept),aes(yintercept = y, color = "Q80 (Drought Event Threshold)"),
               linetype = "dashed", linewidth = 0.8) +
    
    scale_color_manual(values = c("Observed Flow [m3/s]" = "steelblue",
                                  "Flow Quantiles [m3/s]" = "black",
                                  "Q80 (Drought Event Threshold)" = "red3")) +
    lims(y = ylims)+
    theme_bw() +
    theme(legend.position = "top")+
    labs(
      y = "Weekly Mean Flow [m³/s]",
      x = "Date",
      color = "Legend",  
      title = paste0("Hydrographs and Quantile based RMSE visualization \nModel: ", model_name," | ",s," | ",fc_years_text)
      # subtitle = "Grey line indicates Observed Flow"
    ) 
  
  p2 <- cum_seg_all %>%
    filter(horizon == 9 & station == s & fc_period == fc_years_text) %>%
    filter(exceed_prob %in% seq(10, 100, 10)) %>%
    arrange(quant_flow) %>%  # sort by y-axis variable
    ggplot() +
    geom_path(aes(y = quant_flow, x = RMSE)) +  # connects points in order of data
    geom_point(aes(y = quant_flow, x = RMSE)) +  # add points on top
    lims(y = ylims) +
    labs(y = "Quantile Flow")+
    theme_bw() +
    theme(
      plot.margin = margin(5, 0, 5, 0),
      axis.text.y = element_text(),
      axis.ticks.y = element_line()
    )
  
  p3 <- cum_seg_all %>%
    filter(horizon == 9 & station == s & fc_period == fc_years_text) %>%
    filter(exceed_prob %in% seq(10, 100, 10)) %>%
    arrange(quant_flow) %>%  # sort by y-axis variable
    ggplot() +
    geom_path(aes(y = quant_flow, x = MARE)) +  # connects points in order of data
    geom_point(aes(y = quant_flow, x = MARE)) +  # add points on top
    lims(y = ylims) +
    labs(y = "Quantile Flow")+
    theme_bw() +
    theme(
      plot.margin = margin(5, 0, 5, 0),
      axis.text.y = element_text(),
      axis.ticks.y = element_line()
    )
  
  
  combined <- p1 + p2 +p3 + plot_layout(ncol = 3, widths = c(5, 1,1))
  
  png(file=paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text,"/",model_name,"_quantile_eval_hydrograph_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  
  print(combined)
  dev.off()
  
  
  
  
  ##### Hydrographs of predicted vs. observed flows ---------
  
  png(file=paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text,"/",model_name,"_hydrogaphs_facet_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  
  plot <- forecast_df_all %>% rename(Observed = obs, Forecasted = pred) %>% 
    pivot_longer(cols = c(Observed,Forecasted), values_to = "flow", names_to = "pred_obs") %>% 
    filter(lubridate::year(date) %in% fc_years) %>% 
    filter(station == s & fc_period == fc_years_text & horizon %in% c(1,5,7,12)) %>% 
    ggplot() + 
    geom_line(aes(y = flow, x = date, color = pred_obs), linetype = "solid", linewidth = 1, alpha = 0.7) +  
    facet_wrap(~horizon) +  
    labs(
      y = "Weekly Mean Flow [m³/s]",
      x = "Date",
      color = "Forecasting Horizon",  # Rename legend title
      title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s," | ",fc_years_text)    ) +
    scale_color_manual(values = pred_obs_color_code)+
    geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted")+
    theme_bw(base_size = 14) +  # Nicer base theme with larger default font size
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
  
  
  png(file=paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text,"/",model_name,"_hydrographs_stacked_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  plot <- forecast_df_all %>% 
    filter(lubridate::year(date) == 2016) %>% 
    filter(station == s & fc_period == fc_years_text) %>% 
    ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast colored by horizon
    geom_line(linewidth = 1.2, alpha = 0.9) +  # Thicker, slightly transparent forecast lines
    geom_line(aes(y = obs), color = "darkgrey", linetype = "solid", linewidth = 1.4, alpha = 1) +  # Dark grey observed line
    labs(
      y = "Weekly Mean Flow [m³/s]",
      x = "Date",
      color = "Forecasting Horizon",  # Clean legend title
      title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s," | ",fc_years_text)) +
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
  
  
  
  
  png(file=paste0("results/",today,"/",model_name,"/GOF/",fc_years_text,"/",model_name,"-GOF_exceed_prob-",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  plot <-
    fc_df_quant_GOF %>% 
    filter(station == s & fc_period == fc_years_text& horizon %in% c(1,3,5,7,10,12)) %>% 
    ggplot() +
    geom_line(aes(x = exceed_prob, y = RMSE, color = fc_method), linewidth = 1)+
    facet_wrap(~horizon) +
    theme_bw()+
    lims(y = fc_df_quant_GOF %>% filter(station ==s) %>% pull(RMSE) %>% range())+
    labs(
      y = "RMSE [m³/s]",
      x = "Exceedance Probability [%]",
      color = "Forecasting Model",  # Clean legend title
      title = paste0("RMSE over Flow percentiles\nModel: ", model_name," | ",s,"| ",fc_years_text)
    ) +
    theme_light(base_size = 14) +
    geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted")+
    scale_color_manual(values = models_color_code, labels = models_labels)+
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
  
  png(file=paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text,"/",model_name,"_stratified_GOF_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  plot <- fc_df_quant_GOF %>% filter(exceed_prob %in% c(1,50,80,95) & station ==s & fc_period == fc_years_text) %>% 
    ggplot()+
    geom_line(aes(x=horizon, y = RMSE, color = fc_method),linewidth = 1) +
    theme_bw()+
    facet_wrap(~exceed_prob)+
    lims(y = fc_df_quant_GOF %>% filter(station ==s) %>% pull(RMSE) %>% range())+
    labs(
      y = "RMSE [m³/s]",
      x = "Forecasting Horizon",
      color = "Forecasting Model",  # Clean legend title
      title = paste0("RMSE along Forecasting Horizons for different Flow Quantiles \nModel: ", model_name," | ",s," | ",fc_years_text)
      # subtitle = "Grey line indicates Observed Flow"
    ) +
    theme_light(base_size = 14) +
    geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted")+
    scale_color_manual(values = models_color_code, labels = models_labels)+
    scale_x_continuous(breaks = seq(1,12,2))+
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
    
  
  
  
  
  png(file=paste0("results/",today,"/",model_name,"/GOF/",fc_years_text,"/",model_name,"_residuals_boxplot_model_comparison_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  plot <- forecast_df_long_all %>% 
    filter(fc_method != "naive_season" & station ==s & fc_period == fc_years_text) %>% 
    filter(horizon %in% seq(1,12,2)) %>% 
    ggplot() +
    geom_boxplot(aes(x=as.factor(horizon), y = res,fill =fc_method), color = "black" )+
    scale_fill_manual(values = models_color_code, labels = models_labels)+
    labs(title = "Residuals per Forecasting Horizons",
         subtitle = paste0("Comparison of Naive Models with Elastic Net Model | Residuals = Predicted - Observed | ",s ," | ",fc_years_text),
         fill = "Forecasting Method",
         x = "Forecasting Horizons [weeks]",
         y = "Residuals [m^3/s]")+
    theme_bw()
  print(plot)
  dev.off()
  }
    }
  
    
    png(file=paste0("results/",today,"/",model_name,"/GOF/",fc_years_text,"/",model_name,"MARE_stations_comparison_",fc_years_text,".png"),
        width = 1000, height = 600, units = "px")
    
    plot <- fc_df_quant_GOF %>% filter(fc_period == fc_years_text & exceed_prob == 1) %>% 
      ggplot()+
      geom_line(aes(x=horizon, y = RMSE, color = fc_method),linewidth = 1)+
      theme_bw()+
      scale_color_manual(values = models_color_code, labels = models_labels)+
      facet_wrap(~station, scales = "free")+
      labs(x= "Lead Time [weeks]", y = "RMSE",color = "Forecast Method" ,title = paste0("RMSE vs. Lead Time | Station ",s))+
      scale_x_continuous(breaks = seq(1,12,1))
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





## Skill Scores ----

skill_scores <- fc_df_quant_GOF %>% select(horizon, fc_method, station, fc_period, RMSE, MARE, exceed_prob) 

skill_scores <- skill_scores %>% filter(fc_method == "naive_lag") %>% 
  rename(RMSE_lag = RMSE) %>% 
  select(-c(MARE,)) %>% 
  left_join(skill_scores, ., by = c("station", "fc_period", "exceed_prob","horizon"), relationship = "many-to-many") %>% 
  rename(fc_method = fc_method.x) %>% 
  select(-fc_method.y)

skill_scores <- skill_scores %>% filter(fc_method == "naive_lag") %>% 
  rename(MARE_lag = MARE) %>% 
  select(-c(RMSE,RMSE_lag)) %>% 
  left_join(skill_scores, ., by = c("station", "fc_period", "exceed_prob","horizon"), relationship = "many-to-many") %>% 
  rename(fc_method = fc_method.x) %>% 
  select(-fc_method.y)

skill_scores <- skill_scores %>% filter(fc_method == "naive_season") %>% 
  rename(RMSE_season = RMSE) %>% 
  select(-c(MARE,MARE_lag,RMSE_lag)) %>% 
  left_join(skill_scores, ., by = c("station", "fc_period", "exceed_prob","horizon"), relationship = "many-to-many") %>% 
  rename(fc_method = fc_method.x) %>% 
  select(-fc_method.y)

skill_scores <- skill_scores %>% filter(fc_method == "naive_season") %>% 
  rename(MARE_season = MARE) %>% 
  select(-c(RMSE,RMSE_season,MARE_lag,RMSE_lag)) %>% 
  left_join(skill_scores, ., by = c("station", "fc_period", "exceed_prob","horizon"), relationship = "many-to-many") %>% 
  rename(fc_method = fc_method.x) %>% 
  select(-fc_method.y)


skill_scores <- skill_scores %>% 
  mutate("SS-MARE-lag" = 1-(MARE/MARE_lag),
         "SS-RMSE-lag" = 1-(RMSE/RMSE_lag),
         "SS-MARE-season" = 1-(MARE/MARE_season),
         "SS-RMSE-season" = 1-(RMSE/RMSE_season),)

fc_df_quant_GOF <- skill_scores %>% select(c("SS-MARE-lag","SS-RMSE-lag","SS-MARE-season","SS-RMSE-season",
                                             horizon, fc_method,fc_period,exceed_prob,station)) %>% 
  left_join(fc_df_quant_GOF, ., by = c("station","horizon", "fc_method", "fc_period","exceed_prob"))

# Summary Table ----------

summary_table <- fc_df_quant_GOF %>% filter(exceed_prob == 1 &
                                     horizon %in% c(1,4,8,12) &
                                     fc_method %in% c("catchment_memory_model") &
                                     fc_period == "2015_2021")


# Create a basic gt table
gt_table <- summary_table %>% 
  select(horizon, station,"SS-RMSE-season","SS-RMSE-lag") %>% 
  gt() %>%
  tab_header(
    title = "Summary Statistics of Monthly Forecasting Performance",
    subtitle = paste0("Model: ", model_name," ",as.Date(Sys.time()),"FC-Period: ",fc_years_text)
  ) %>%
  fmt_number(
    columns = c(
      # R2, 
      "SS-RMSE-season","SS-RMSE-lag",
      # ,RMSE_scaled_season,RMSE_scaled_lag
      ),
    decimals = 2
  ) %>%
  cols_label(
    horizon = md("Forecasting<br>Horizon<br>(weeks)"),
    # R2 = md("R²"),
    "SS-RMSE-season" = md("SS-RMSE-season"),
    "SS-RMSE-season" = md("SS-RMSE-season"),
    
    # lambda.value = md("Optimal<br>Lambda"),
    # lambda.applied = md("Applied<br>Lambda"),
    # n_coefs = md("Optimal Number<br>of Coefficients"),
    # RMSE_scaled_season = md("RMSE_scaled_season"),
    # RMSE_scaled_lag = md("RMSE_scaled_lag")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  )
# tab_footnote(
#   footnote = "For forcasting of all months, the paramters for lamdba and number of coefficients of the forecasting horizon = 1 and not the optimal values were applied.",
#   locations = cells_title(groups = "title")
# )
print(gt_table)
gtsave(gt_table, paste0("results/",today,"/",model_name,"/summary_table_",fc_years_text,".png"))





# Visualisation of sinusoidal month transformation ----
# df <- df %>% mutate(year = as.integer(year), month = as.integer(month),
# 
#        week_year = as.integer(week_year), week_tot = as.integer(week_tot),
# 
#        week_hyd = sin(week_year / (52 / (2 * pi)) - 4 * 2 * pi / 52),
# 
#        week_hyd_sqrt = sqrt(sin(week_year / (52 / (2 * pi)) - 4 * 2 * pi / 52)),
# 
# 
#        # week_hyd_half = abs(sin(week_year / (104 / (2 * pi)) - 8 * 2 * pi / 52)),
# 
#        # week_sin = sin(week_year / (52 / (2 * pi))),
# 
#       week_sin_half = sin(week_year/(104/(2*pi)) -52 * 2 * pi /104))
# 
# 
# df %>% filter(year == 2011) %>%
#   ggplot() +
#   geom_line(aes(x = Date, y = week_hyd, color = "week_hyd"),linewidth=1) +
#   geom_line(aes(x = Date, y = week_sin_half, color = "week_sin_half"),linewidth=1) +
#   scale_x_datetime(date_breaks = "1 month", date_labels = "%b")+
#   theme_bw()




# HISTORIC DROUGHT EVALUTATION ----

rm(list = setdiff(ls(), c("forecast_df_all","forecasts_list","quantiles_all","model_name","today","date_BM_models")))

hist_fc_df_all <- data.frame()

for (fc_years_text in c("2003_2004","2015_2016","2015_2021")) {
      
  for (models in c("Ensemble_Model","Climate_Upper_BM",paste0(model_name))) {
# 
#     fc_years_text <- "2003_2004"
#     models <- "Ensemble_Model"
#     # 
    if(models == "Ensemble_Model"){      
    load(paste0("results/",date_BM_models,"/Ensemble_Model/",fc_years_text,"/Ensemble_Model_forecasts_list_",fc_years_text,".RData"))}
    
    if(models == "Climate_Upper_BM"){      
      load(paste0("results/",date_BM_models,"/Climate_Upper_BM/",fc_years_text,"/Climate_Upper_BM_forecasts_list_",fc_years_text,".RData"))}
    
    if(models == paste0(model_name)){            
      load(paste0("results/",today,"/",models,"/",fc_years_text,"/",models,"_forecasts_list_",fc_years_text,".RData"))
}
    
    hist_fc_df_all <-   purrr::imap_dfr(forecasts_list, ~ { 
        
        station <- .y
        
        purrr::imap_dfr(.x, ~ {
          
          horizon_char <- .y
          df <- .x
          if(models == "Ensemble_Model"){
            df <- df %>% filter(ensemble == "ensemble_mean")
          }
          df <- df %>% mutate(station = station, horizon = horizon, 
                              horizon = horizon,horizon_char = horizon_char,
                              fc_method = models) %>% 
            select(c(station, horizon, horizon_char,fc_method, pred, obs,date))

  })
}) %>% mutate(fc_period = fc_years_text) %>%  rbind(.,hist_fc_df_all)

  }
  
}


hist_fc_df_all %>% 
  filter( horizon == 5, fc_period == "2015_2021") %>% 
  ggplot()+
  geom_line(aes( x = date, y = pred, color = station))+
  facet_wrap(~fc_method)+
  lims(y = c(0,30))

hist_fc_df_all <- forecast_df_all %>% filter(lubridate::year(date) %in% 2003:2004) %>% 
  select("date" ,        "obs" ,       "horizon",      "station",      
         "horizon_char", "fc_method",    "fc_period","naive_season")   %>% 
  mutate(pred = naive_season, fc_period = "2015_2016",fc_method = "naive_season") %>% select(-"naive_season") %>% 
  distinct %>% 
  rbind(.,hist_fc_df_all)

hist_fc_df_all <- forecast_df_all %>% filter(lubridate::year(date) %in% 2003:2004) %>% 
  select("date" ,        "obs" ,       "horizon",      "station",      
         "horizon_char", "fc_method",    "fc_period","naive_season")   %>% 
  mutate(pred = naive_season, fc_period = "2003_2004",fc_method = "naive_season") %>% select(-"naive_season") %>% 
  distinct %>% 
  rbind(.,hist_fc_df_all)

hist_fc_df_all <- forecast_df_all %>% filter(lubridate::year(date) %in% 2015:2021) %>%
  select("date" ,        "obs" ,       "horizon",      "station",
         "horizon_char", "fc_method",    "fc_period","naive_lag")   %>%
  mutate(pred = naive_lag, fc_period = "2015_2021",fc_method = "naive_lag") %>% select(-"naive_lag") %>%
  distinct %>%
  rbind(.,hist_fc_df_all)

hist_fc_df_all <- hist_fc_df_all %>% 
  pivot_longer(cols=(c("pred","obs")),values_to = "flow",names_to = "pred_obs")
hist_fc_df_all <- hist_fc_df_all %>% na.omit()
hist_fc_df_all <- hist_fc_df_all %>% distinct()

save(hist_fc_df_all, file = (paste0("results/",today,"/",model_name,"/hist_fc_df_all.RData")))


