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

### name the models and dataset of interest ------
# today <- Sys.Date()-5
today <- as.Date("2025-10-03")
model_name <- c("sinus_1se_lags_seas")
dataset <- "lagged_TB" #TB = Tauchenbach
fc_years_list <- list(2015:2016
                        ,2003:2004
                        ,2015:2021
)
ensemble_df_all <- NULL
upper_BM_df_all <- NULL
forecast_df_all <- NULL
n_coefs_all <- NULL
coefs_means_all <- NULL
coefs_sd_all <- NULL
coefs_df_all <- NULL
lambda_df_all <- NULL

for (fc_years in fc_years_list) {
  
   # fc_years <- 2015:2016
  
  fc_years_text <- paste0(min(fc_years),"_",max(fc_years))
  


load(paste0("results/",today,"/Ensemble_Model/",fc_years_text,"/Ensemble_Model_forecasts_list_",fc_years_text,".RData"))
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

load(paste0("results/",today,"/Climate_Upper_BM/",fc_years_text,"/Climate_Upper_BM_forecasts_list_",fc_years_text,".RData"))
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
}) %>% mutate(fc_period = fc_years_text) %>%  rbind(.,forecast_df_all)

coefs_list_final <- list()

cm_list <- list()

### Visualizing Cofficients --------------

n_horizons <- as.numeric(length(unique(names(forecasts_list))))

if (file.exists(paste0("results/",today,"/",model_name,"/coefficients/",fc_years_text))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/coefficients/",fc_years_text)))
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



#### COEFS VERTICAL BARS ---------

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
      
      naive_season <- df %>%   filter(Date < start_date_GOF) %>%
        select(c("Date","flow_mean","week_year")) %>%  rename(date = Date, flow = flow_mean)
      
      naive_season <- naive_season %>%  setDT() %>% .[, naive_season := mean(flow, na.rm = T), by = week_year]
    
        
      quantiles <- df %>% filter(!lubridate::year(Date) %in% fc_years) %>% pull("flow_mean") 

      quantiles <- sapply(1:100, function(x) quantile(quantiles, probs = 1-(x/100))) %>% 
        data.frame(quant_flow = ., exceed_prob = 1:100)
      
      quantiles$fc_period <- fc_years_text
      
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
            RMSE = as.numeric(hydroGOF::rmse(sim = pred, obs = obs))
            r2_2 = hydroGOF::R2(sim = pred, obs = obs)
            
            .(r2= r2,RMSE= RMSE, mean_pred = mean_pred, mean_obs_seg = mean_obs_seg, mean_obs_tot = mean_obs_tot,r2_2 =r2_2)
          }, by = horizon]
        
        cum_seg$exceed_prob <- q
        cum_seg$station <- s
        cum_seg$fc_period <- fc_years_text
        
        
        cum_seg_all <- rbind(cum_seg, cum_seg_all)
      
      }
    
            
        # ADDING OTHER MODEL FORECASTS ----
        
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
          cum_seg_th <- quantiles_all %>% filter(q == exceed_prob  & station ==s & fc_period == fc_years_text) 
          fc_df_quant <- forecast_df_long %>% filter(obs < cum_seg_th$quant_flow & station ==s & fc_period == fc_years_text)
          
          fc_df_quant <- fc_df_quant %>% setDT() %>% 
            .[, {
              mean_pred <- mean(pred, na.rm = T)
              mean_obs_tot <- mean_obs_tot
              mean_obs_seg <- mean(obs, na.rm = T)
              ssr <- sum((pred - mean_obs_tot)^2)
              sstot <- sum((obs - mean_obs_tot)^2)
              r2 <- 1-(ssr / sstot)
              RMSE = as.numeric(hydroGOF::rmse(sim = pred, obs = obs))
              RRMSE_seg = RMSE/mean_obs_seg
              r2_2 = hydroGOF::R2(sim = pred, obs = obs)
              
              .(r2= r2,RMSE= RMSE, mean_pred = mean_pred, mean_obs_seg = mean_obs_seg, 
                mean_obs_tot = mean_obs_tot,r2_2 =r2_2)
            }, by = .(horizon, fc_method, station,fc_period)]
          
          fc_df_quant$exceed_prob <- q
          fc_df_quant$station <- s
          fc_df_quant$fc_period <- fc_years_text
          fc_df_quant_GOF <- rbind(fc_df_quant, fc_df_quant_GOF)
          
        }
    }
}

forecast_df_all <- temp
cum_seg_all <- left_join(cum_seg_all , quantiles_all, by = c("exceed_prob","station","fc_period"))

# test <- fc_df_quant_GOF %>% filter(station ==s &horizon ==1 & fc_method =="naive_season", exceed_prob ==50)



# test <- forecast_df_long_all %>% filter(station ==s &horizon ==1 & fc_method =="naive_season")

# START LOOP FOR PLOTTING ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
for (s  in unique(forecast_df_all$station)) {
  for (fc_years in fc_years_list) {
    
    fc_years_text <- paste0(min(fc_years),"_",max(fc_years))
      
    # s <- "Tauchenbach"
    
    coefs_df <- coefs_df_all %>% filter(station ==s & fc_period == fc_years_text)
    coefs_df <- coefs_df %>% setDT() %>% 
      .[variable != "(Intercept)", c(.SD,.(coefs_rel = (abs(coefs)*100)/(sum(abs(coefs))) ) ), by = fc_horizon]
    
    top_coefs <- coefs_df_all %>% filter(station == s) %>% setDT() %>% 
      .[variable != "(Intercept)", .(coefs = sum(abs(coefs))), by = variable]
    
    top_coefs <- top_coefs %>%  arrange(desc(coefs))
    
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
    
    ### PLOT RELATIVE PREDICTOR IMPORTANCE 
    
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
    
    
    #### Error LINES Version 1 ----
    
    
    png(file=paste0("results/",today,"/",model_name,"/coefficients/",fc_years_text,"/",model_name,"_coefs_lines_shadings",s,"_",fc_years_text,".png"),
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
           subtitle = paste0("Model: ", model_name, " from: ", Sys.Date()," station = ",s,"_",fc_years_text))+
      scale_x_continuous(breaks = seq(1,12,1))
  print(plot)
    dev.off()
    #### Error BARS Version 2 ----
    
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
    
    # LAMBDA VS. PREDICTOR COEFS LINE PLOT ----
    
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
  
  #### QUANTILE BASED EVALUATION ----
  
  #### GOF vs. Horizons Plot ----------
  if (file.exists(paste0("results/",today,"/",model_name,"/GOF/"))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/GOF/")))
  }
  if (file.exists(paste0("results/",today,"/",model_name,"/GOF/",fc_years_text))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/GOF/",fc_years_text)))
  }
  
  
  if (file.exists(paste0("results/",today,"/",model_name,"/hydrographs/"))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/hydrographs/")))
  }
  if (file.exists(paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text)))
  }
  
  cum_seg_all %>% 
    filter(horizon==5 & fc_period == fc_years_text) %>% 
    ggplot() +
    # geom_line(aes(x = exceed_prob, y = mean_pred, color = "Predicted Mean"))+
    # geom_line(aes(x = exceed_prob, y = mean_obs, color = "Observed Mean", linetype="dotted"))+
    # geom_line(aes(x = exceed_prob, y = RMSE, color = "RMSE"))+
    geom_line(aes(x = exceed_prob, y = r2, color = "r2"))+
    geom_line(aes(x = exceed_prob, y = r2_2, color = "r2_2"))
  
  quantile_y_intercept <- quantiles_all %>% 
    filter(exceed_prob %in% seq(10,100,10) & station == s & fc_period ==fc_years) %>% 
    pull(quant_flow)
  ylims <- forecast_df_all %>% filter(date > start_date_GOF & station ==s) %>% pull(obs) %>% range()
  
  p1 <- forecast_df_all %>%
    filter(lubridate::year(date) %in% fc_years & station ==s & fc_period == fc_years_text) %>%
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
    theme_bw() +
    theme(
      plot.margin = margin(5, 0, 5, 0),
      axis.text.y = element_text(),
      axis.ticks.y = element_line()
    )
  
  
  combined <- p1 + p2 + plot_layout(ncol = 2, widths = c(5, 1))
  
  png(file=paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text,"/",model_name,"_quantile_eval_hydrograph_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  
  print(combined)
  dev.off()
  
  
  
  
  ##### Hydrographs of predicted vs. observed flows ---------
  
  png(file=paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text,"/",model_name,"_hydrogaphs_facet_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  
  plot <- forecast_df_all %>% 
    filter(lubridate::year(date) == 2021) %>% 
    filter(station == s & fc_period == fc_years_text) %>% 
    ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast lines colored by horizon
    geom_line(linewidth = 1.2) +  # Slightly thicker forecast lines
    geom_line(aes(y = obs), color = "grey", linetype = "solid", linewidth = 1.2) +  # Darker grey for observed line
    facet_wrap(~horizon_char) +  # Facet by horizon, free y-axis
    labs(
      y = "Monthly Low Flow [m³/s]",
      x = "Date",
      color = "Forecasting Horizon",  # Rename legend title
      title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s," | ",fc_years_text),
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
  
  
  png(file=paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text,"/",model_name,"_hydrographs_stacked_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  plot <- forecast_df_all %>% 
    filter(lubridate::year(date) == 2015) %>% 
    filter(station == s & fc_period == fc_years_text) %>% 
    ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast colored by horizon
    geom_line(linewidth = 1.2, alpha = 0.9) +  # Thicker, slightly transparent forecast lines
    geom_line(aes(y = obs), color = "darkgrey", linetype = "solid", linewidth = 1.4, alpha = 1) +  # Dark grey observed line
    labs(
      y = "Monthly Low Flow [m³/s]",
      x = "Date",
      color = "Forecasting Horizon",  # Clean legend title
      title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s," | ",fc_years_text),
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
  
  
  
  
  png(file=paste0("results/",today,"/",model_name,"/hydrographs/",fc_years_text,"/",model_name,"_hydrographs_facet_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  plot <-
    fc_df_quant_GOF %>% 
    filter(station == s & fc_period == fc_years_text) %>% 
    ggplot() +
    geom_line(aes(x = exceed_prob, y = RMSE, color = fc_method), linewidth = 1)+
    facet_wrap(~horizon) +
    theme_bw()+
    lims(y = fc_df_quant_GOF %>% filter(station ==s) %>% pull(RMSE) %>% range())+
    labs(
      y = "RMSE [m³/s]",
      x = "Exceedance Probability [%]",
      color = "Forecasting Model",  # Clean legend title
      title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s,"| ",fc_years_text)
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
    
  
  
  
  
  png(file=paste0("results/",today,"/",model_name,"/GOF/",fc_years_text,"/",model_name,"_residuals_boxplot_model_comparison_",s,"_",fc_years_text,".png"),
      width = 1000, height = 600, units = "px")
  
  plot <- forecast_df_long_all %>% 
    filter(fc_method != "naive_season" & station ==s & fc_period == fc_years_text) %>% 
    filter(horizon %in% seq(1,12,2)) %>% 
    ggplot() +
    geom_boxplot(aes(x=as.factor(horizon), y = res,fill =fc_method), color = "black" )+
    # facet_wrap(~quantile)+
    labs(title = "Residuals per Forecasting Horizons",
         subtitle = paste0("Comparison of Naive Models with Elastic Net Model | Residuals = Predicted - Observed | ",s ," | ",fc_years_text),
         x = "Forecasting Horizons [weeks]",
         y = "Residuals [m^3/s]")+
    theme_bw()
  print(plot)
  dev.off()
  }
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

rm(list = setdiff(ls(), c("forecast_df_all","forecasts_list","quantiles_all","model_name","today")))

hist_fc_df_all <- data.frame()

for (fc_years_text in c("2003_2004","2015_2016","2015_2021")) {
      
  for (models in c("Ensemble_Model","Climate_Upper_BM",paste0(model_name))) {
    
    # fc_years_text <- "2003_2004"
    # models <- "Ensemble_Model"
    # 
      load(paste0("results/",today,"/",models,"/",fc_years_text,"/",models,"_forecasts_list_",fc_years_text,".RData"))
      
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


# naive_lags <- data.frame()
# 
# test_years_list <- list(2015:2016
#   ,2003:2004
#   ,2015:2021
#   )
# 
# for(s in unique(forecast_df_all$station)){
#   
#   for(i in 1:length(test_years_list)){
#     
#    test_years_loop <- test_years_list[[i]]
#    
#    test_years_text <- paste0(min(test_years_loop),"_",max(test_years_loop))
#   
#    # s <- "Uttendorf"
#   
#   load(file =  paste0("data/",s,"/Final_df_",s,"_weekly.RData"))
#   
#   temp <- df %>% filter(year %in% test_years_loop) %>% select(c("Date","flow_mean")) %>% crossing(horizon = 1:12)
#   temp$station <- s
#   naive_lags <- temp %>% setDT() %>% 
#     .[, .(pred = lag(flow_mean, horizon),obs = flow_mean, date = Date, 
#           fc_period = test_years_text, station = s), by = .(horizon)] %>%
#     rbind(.,naive_lags)
#   
#     }
#   }
#   
# 
# 
# naive_lags <- naive_lags %>% 
#   mutate(fc_method = "naive_lag",  horizon_char = paste0("fc_horizon_",horizon)) 

# hist_fc_df_all <- hist_fc_df_all %>% rbind(.,naive_lags)



hist_fc_df_all <- hist_fc_df_all %>% 
  pivot_longer(cols=(c("pred","obs")),values_to = "flow",names_to = "pred_obs")
hist_fc_df_all <- hist_fc_df_all %>% na.omit()
hist_fc_df_all <- hist_fc_df_all %>% distinct()

hist_fc_df_all %>% 
  filter(pred_obs == "pred", horizon == 5, fc_period == "2015_2021") %>% 
  ggplot()+
  geom_line(aes( x = date, y = flow, color = station))+
  facet_wrap(~fc_method)+
  lims(y = c(0,30))


# completeness <- data.frame(attributes = c("horizon","station","fc_method","fc_period"),
#                    ist = c(length(unique(hist_fc_df_all$horizon)),
#                      length(unique(hist_fc_df_all$station)),
#                      length(unique(hist_fc_df_all$fc_method)),
#                      length(unique(hist_fc_df_all$fc_period))),
#                    soll = c(12,4,5,3))
                   
data <- hist_fc_df_all %>% filter(station == "Uttendorf" &
                                    horizon == 1 &
                                    fc_method == "naive_lag"&
                                    fc_period == "2015_2016"&
                                    pred_obs == "pred"
                                    # lubridate::year(date) %in% c(2003,2015)
                                  )
data <- data %>% distinct()


# Calculate Drought Objects ----

calculate_drought_objects <-
  function(data,
           quantiles_all,
           tmin_pooling,
           ratio_pooling) {
    data <- data %>% as.data.frame() %>% distinct()
    
    data <-
      data %>% select(c(
        "date",
        "flow",
        "station",
        "fc_method",
        "fc_period",
        "horizon","pred_obs"
      )) %>% setDT()
    
    station <- unique(data$station)
    fc_period <- unique(data$fc_period)
    
    lf_obj <-
      data.frame(date = seq(min(data$date), max(data$date), by = "day"))
    lf_obj <- merge(lf_obj, data, by = "date", all.x = TRUE)
    lf_obj$date <- as.Date(lf_obj$date)
    lf_obj <- lf_obj[order(lf_obj$date),]
    lf_obj <- tidyr::fill(lf_obj, everything(), .direction = "down")
    
    results <-
      lf_obj %>% select(c("date", "station", "fc_method", "fc_period","horizon","pred_obs"))
    
    # results<- results %>% mutate(discharge = NA,threshold = NA, def.increase = NA, event.no = 0, event.orig = 0)
    
    
    lf_obj <- lf_obj %>%
      mutate(day   = day(date),
             month = month(date),
             year  = year(date))
    
    
    lf_obj <- createlfobj(lf_obj, hyearstart = 11, baseflow = FALSE)
    flowunit(lf_obj) <- "m^3/s"
    
    quantiles_all <- quantiles_all %>% setDT()
    
    Q80 <- quantiles_all$quant_flow[quantiles_all$exceed_prob == 80 &
                                      quantiles_all$station == station &
                                      quantiles_all$fc_period == fc_period]
    
    
    deficit_obj <- lfstat::find_droughts(lf_obj, threshold = Q80)
    
    
    if (max(deficit_obj$event.no) > 1) {
      deficit_obj <-
        pool_ic(deficit_obj, tmin = tmin_pooling, ratio = ratio_pooling)
      
    }
    
    
    dates <- as.Date(index(deficit_obj))
    
    # lf_obj <- data.frame(date = as.Date(index(lf_obj)),
    #                       discharge = as.numeric(coredata(lf_obj$discharge)),
    #                       threshold = Q80)
    #
    deficit_obj <- as.data.frame(deficit_obj)
    deficit_obj$date <- dates
    
    
    if (!"event.orig" %in% names(deficit_obj)) {
      deficit_obj <- dplyr::mutate(deficit_obj, event.orig = 0)
    }
    
    results <- merge(results, deficit_obj,
                     by = "date", all.x = T)
    
    return(results)
  }

setDT(hist_fc_df_all)

drought_objects <-
  hist_fc_df_all[, calculate_drought_objects(.SD,
                                             quantiles_all,
                                             ratio_pooling = 0.1,
                                             tmin_pooling = 14),
                 by = .(station, fc_method, horizon, fc_period,pred_obs),
                 .SDcols = c("date", "flow", "station", "fc_method", "horizon", "fc_period","pred_obs")]

# drought_objects$fc_method <- drought_objects %>% as_tibble() %>% 
#   select(!duplicated(names(.))) %>%
#   select(c( "station" ,     "fc_method" ,   "horizon",     
#                                                                  "fc_period",    "date" ,         "discharge" ,   
#                                                                  "threshold",    "def.increase", "event.no",     
#                                                                  "event.orig"))
# drought_objects <- drought_objects %>% distinct()

data <- drought_objects %>% select(c("date",  "discharge" ,       
                                     "threshold",    "def.increase", 
                                     "event.no", "event.orig")) 

data$fc_method <- drought_objects$fc_method
data$fc_period <- drought_objects$fc_period
data$station <- drought_objects$station
data$horizon <- drought_objects$horizon
data$pred_obs <- drought_objects$pred_obs


data <- data %>%
  setDT() %>%
  .[, `:=`(
    start    = fifelse(event.no > 0, first(date), as.Date(NA)),
    end      = fifelse(event.no > 0, last(date),  as.Date(NA)),
    duration = fifelse(event.no > 0, as.integer(last(date) - first(date) + 1), 0),
    date     = date,
    def.vol      = fifelse(event.no > 0, sum(def.increase),  NA),
    qmin      = fifelse(event.no > 0, min(discharge),  NA)),
  by = .(station, fc_method, horizon, fc_period,event.no,pred_obs), ]

wide <- data %>%
  pivot_wider(
    id_cols     = all_of(c("date","station","fc_method","fc_period","horizon","threshold")),    
    names_from  = pred_obs,
    values_from =  c(event.no, discharge, def.increase, event.orig,start,end,duration,qmin,def.increase,def.vol),
    names_prefix = ""
    # values_fn   = list(event.no = max),  # bei Duplikaten z.B. max nehmen
    # values_fill = 0                      # fehlende als 0 statt NA
  ) 


wide <- wide %>% filter(station == "Flattach",
                        fc_period == "2015_2021",
                        horizon == 12,
                        fc_method == "Ensemble_Model")
#
wide <- wide %>% setDT() %>% 
  .[, `:=` (start_pred = na.locf(start_pred, fromLast = TRUE, na.rm = F),
            end_pred = na.locf(end_pred, fromLast = TRUE, na.rm = F))
  ,by = .(station, fc_method, horizon, fc_period), ]

wide <- wide %>% mutate(duration_pred_test = end_pred - start_pred + 1)

wide <- wide %>% filter(event.no_obs != 0)

wide <- wide %>% setDT() %>% 
  .[, `:=` (qmin_pred = min(discharge_pred),
            def.vol_pred = ((threshold-discharge_pred)*60*60*24))
    ,by = .(station, fc_method, horizon, fc_period,event.no_obs), ]

wide %>% 
  ggplot()+
  geom_line(aes(x = date, y = discharge_obs, color = "steelblue"))+
  geom_line(aes(x = date, y = discharge_pred, color = "darkred"))+
  geom_hline(yintercept = unique(wide$threshold))
  

pred_obs_diff <- wide %>% setDT() %>% 
  .[event.no_obs != 0 &
      start_obs < fcase(
        fc_period == "2003_2004", as.Date("2003-12-31"),
        fc_period == "2015_2016", as.Date("2015-12-31"),
        fc_period == "2015_2021", as.Date("2018-06-30")
      ), .(start_diff = start_pred - start_obs,
                         duration_diff = duration_pred - duration_obs,
                         qmin_diff = qmin_pred - qmin_obs,
                         def.vol_diff = sum(def.increase_pred) - sum(def.increase_obs),
                        event_name = paste0(unique(station)," | ",
                                            unique(fc_method)," | ",
                                            unique(fc_period)," | ",
                                            unique(horizon)," | ",
                                            unique(.GRP)),
                        start = first(start_obs),
                        end = first(end_obs),
           duration_obs = first(duration_obs)),
    by = .(station, fc_method, horizon, fc_period,sapply(event.no_obs, toString))]

pred_obs_diff <- pred_obs_diff %>% distinct()
 
pred_obs_diff <- quantiles_all %>% filter(exceed_prob == 80) %>% select(Q80 = quant_flow,station,fc_period) %>% 
  left_join(pred_obs_diff, . , by = c("station","fc_period"))
pred_obs_diff <- quantiles_all %>% filter(exceed_prob == 50) %>% select(Q50 = quant_flow,station,fc_period) %>% 
  left_join(pred_obs_diff, . , by = c("station","fc_period"))

pred_obs_diff$def.vol_diff_mean_perc = (pred_obs_diff$def.vol_diff/(pred_obs_diff$Q80*60*60*24*pred_obs_diff$duration_obs))*100

#def.vol_diff_perc = durchschnittliche abweichung zwischen vorhergesagtem und beobachtetem defizitvolumen
#wie viel prozent des Q80-Flows wurde jeden Tag für das Event Falsch vorhergesagt

pred_obs_diff$def.vol_diff_rel = (pred_obs_diff$def.vol_diff/(pred_obs_diff$Q80))

pred_obs_diff <- pred_obs_diff %>% mutate(start_diff = as.numeric(start_diff))

### Summarizing Historic Event Analysis Table station-wise ----

min_max_norm <- function(x){
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }

vars <- c("start_diff", "duration_diff","qmin_diff","def.vol_diff_rel")

pred_obs_diff_scaled <- copy(pred_obs_diff)

pred_obs_diff_scaled[, (vars) := lapply(.SD, min_max_norm), .SDcols = vars, 
                     by = .(station, horizon)]

summary_hist <- pred_obs_diff_scaled %>% setDT() %>% 
  .[, .(n_events = .N,
      start_diff_mean = as.numeric(mean(start_diff)),
      start_diff_median = as.numeric(median(start_diff)),
      duration_diff_mean = mean(duration_diff),
      duration_diff_median = median(duration_diff),
      qmin_diff_mean = mean(qmin_diff),
      qmin_diff_median = median(qmin_diff),
      duration_obs_mean = mean(duration_obs),
      duration_obs_median = median(duration_obs),
      def.vol_diff_rel_mean = mean(def.vol_diff_rel)),
    by = .(station,horizon,fc_method, fc_period)]
  
### PLOTTING OF FINGERPRINTS ----

##create radar plot function ----

radar_plots <- function(data, axis_name_offset, central_distance, 
                        fill_alpha, linetype_grid,alpha_map)
  {
  data <- data %>% select(c(start_diff_mean,
                            duration_diff_mean,
                            qmin_diff_mean,
                            def.vol_diff_rel_mean,
                            station,horizon,fc_method))
  data <- data %>% 
    select(-c(station,horizon)) %>% 
    rename(group = fc_method) 
  
  ### map the empty plot
    circle_coords <- function(r, n_axis = ncol(data) - 1){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r)
    }
    
    central_distance <- 0.2
    
    map <- map_df(seq(0, 1, 0.25) + central_distance, circle_coords) %>%
    ggplot(aes(x, y)) +
    geom_polygon(data = circle_coords(1 + central_distance), 
                 alpha = alpha_map, fill = "gray97") +
    geom_path(aes(group = r), lty = linetype_grid, alpha = 0.1) +
    theme_void()
    
    # calculate coordinates for the axis
    axis_coords <- function(n_axis){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
    x1 <- central_distance*cos(fi)
    y1 <- central_distance*sin(fi)
    x2 <- (1 + central_distance)*cos(fi)
    y2 <- (1 + central_distance)*sin(fi)
    
    tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
    }
    
    axis <- geom_line(data = axis_coords(ncol(data) - 1), 
                               aes(x, y, group = id), alpha = 0.3)
    map + axis
    
    # plot the data 
    axis_name_offset <- 0.2
    rescaled_coords <- function(r, n_axis){
    # fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    fi <- seq(0, 2 * pi, length.out = n_axis + 1) + pi/2#
    
    tibble(r, fi) %>% mutate(x = r*cos(fi), y = r*sin(fi)) %>% select(-fi)
    }
    
    text_data <- data %>%
    select(-group) %>%
    map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 0.25)) %>%
    mutate(r = seq(0, 1, 0.25)) %>%
    pivot_longer(-r, names_to = "parameter", values_to = "value") %>% 
    mutate(value = round(value, digits = 2))
    
    text_coords <- function(r, n_axis = ncol(data) - 1){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r = r - central_distance)
    }
    
    labels_data <- map_df(seq(0, 1, 0.25) + central_distance, text_coords) %>%
    bind_cols(text_data %>% select(-r))
    
    map + axis + 
    geom_text(data = labels_data, aes(x, y, label = value), alpha = 0.65) +
    geom_text(data = text_coords(1 + central_distance + 0.2), 
              aes(x, y), label = labels_data$parameter[1:(ncol(data)-1)])
    
    rescaled_data <- data %>% 
    # mutate(across(-c(station,horizon,fc_method), rescale)) %>%
    mutate(copy = pull(., 1)) %>%
    pivot_longer(-c(group), names_to = "parameter", values_to = "value") %>%
    group_by(group) %>%
    mutate(coords = rescaled_coords(value + central_distance, ncol(data) - 1)) %>%
    unnest(cols = c(coords)) 

    drawings <- map+axis + 
    geom_point(data = rescaled_data, 
               aes(x, y, group = group, col = group), 
               size = 3) +
    geom_path(data = rescaled_data, 
              aes(x, y, group = group, col = group), 
              size = 1)+
    geom_polygon(data = rescaled_data, 
                 aes(x, y, group = group, 
                     col = group, fill = group), 
                 size = 1, alpha = 0.1, show.legend = FALSE) +
    geom_text(data = labels_data, 
              aes(x, y, label = value), alpha = 0.65) +
    geom_text(data = text_coords(1 + central_distance + axis_name_offset), 
              aes(x, y), label = labels_data$parameter[1:(ncol(data)-1)]) +
    labs(col = "steelblue") +
    theme_void()
    
}

data <- summary_hist %>% filter(station == "Tauchenbach",
                                fc_period == "2003_2004",
                                horizon == 12)
## Plot Radar Plots ----
plot <- radar_plots(data = data,
            axis_name_offset = 0.2, 
            central_distance = 0.1, 
            fill_alpha = 0.1, 
            linetype_grid = 1,
            alpha_map = 0.01)
print(plot)

