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
rm(list=ls())

### name the models and dataset of interest ------
today <- Sys.Date()-12
# today <-
model_name <- c("test_new_stations")

dataset <- "lagged_TB" #TB = Tauchenbach
# station <- "tauchenbach"

load("results/2025-07-01/Ensemble_Model/Ensemble_Model_lagged_TB_forecasts_list.RData")
ensemble_df <-   purrr::imap_dfr(forecasts_list, ~ { 
  
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
})


load("results/2025-07-01/Climate_Upper_Benchmark_lambdamin/Climate_Upper_Benchmark_lambdamin_lagged_TB_forecasts_list.RData")
upper_BM_df <-   purrr::imap_dfr(forecasts_list, ~ { 
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    df <- .x
    
    df <- df %>% mutate(station = station, horizon = horizon, 
                        date = date, obs = obs, pred = pred, 
                        horizon = horizon,horizon_char = horizon_char,
                        fc_method = "upper_BM")
    
  })
})

load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_forecasts_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_final_model_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"n_coefs_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"results.RData"))

# load("data/flow_quantiles_list.RData")


# quantiles <- flow_quantiles_list[[paste0(station)]]

GOF_df <-   purrr::imap_dfr(GOF_list, ~ { 
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    df <- .x
    
    df <- df %>% mutate(station = station, horizon = fc_horizon, 
                        horizon = horizon,horizon_char = horizon_char,
                        fc_method = "fc_model")
    
  })
})

coefs_list_final <- list()

forecast_df <-   purrr::imap_dfr(forecasts_list, ~ { 
  
  station <- .y
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    df <- .x
    
    df <- df %>% mutate(station = station, horizon = horizon, 
                        horizon = horizon,horizon_char = horizon_char,
                        fc_method = "fc_model")
    
  })
})

cm_list <- list()

### Visualizing Cofficients --------------

n_horizons <- as.numeric(length(unique(names(forecasts_list))))

if (file.exists(paste0("results/",today,"/",model_name,"/coefficients/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/coefficients/")))
}


n_coefs <-   purrr::imap_dfr(n_coefs_list, ~ { 
  
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


# 
# for (h in fc_horizon) {
#   h <- 1
# coefficients plots ------
  #### COEFS HORIZONTAL BARS ----
  
#   coefs_df <- data.frame()
#   coefs_df <- coefs_list[[paste0("fc_horizon_",h)]] %>% 
#     t() %>%
#     as.data.frame()
#   lambdas <- unique(GOF_df$lambda)[1:nrow(coefs_df)]
#   
#   coefs_df$lambda <- lambdas
#   coefs_df <- pivot_longer(coefs_df,cols = - lambda, values_to = "value", names_to = "coefficient")
#   
#   png(filename = paste0("results/",model_name,"/coefficients/",model_name,"_coefs_opt_lambda_h-",h,".png"),
#       width = 1000, height = 600, units = "px")
#   opt_lambda <- GOF_df %>% 
#     setDT() %>% 
#     .[fc_horizon == h]  
#   if(nrow(opt_lambda) == 1){
#   opt_lambda <- opt_lambda$lambda.value
#     }
#   else {
#     opt_lambda <- opt_lambda$lambda[which.min(opt_lambda$RMSE)]
#     
#   }
#   coefs_df <- coefs_df %>% 
#     setDT() %>% 
#     .[lambda == opt_lambda] %>% 
#     .[value != 0] %>% 
#     .[coefficient != "(Intercept)"] 
#   
# 
# plot <-   coefs_df %>%
#   ggplot(aes(x = reorder(coefficient, value), y = value, fill = value)) +  # Map fill to value
#   geom_col(show.legend = FALSE, color = "black") +  # Remove legend if not needed
#   coord_flip() +  # Flip for better readability
#   theme_light(base_size = 14) +  # Clean light theme with larger base font
#   scale_fill_gradient2(  # Diverging color scale
#     low = "darkred",        # For negative values
#     mid = "white",      # Around zero
#     high = "forestgreen",      # For positive values
#     midpoint = 0        # Center at zero
#   ) +
#   labs(
#     title = paste0("Coefficients of Predictors for Elastic-Net Model ", model_name, " FC-Horizon = ", h),
#     y = "Coefficients of Predictors",
#     x = "Predictors"
#   ) +
#   theme(
#     axis.text = element_text(size = 14, color = "black"),  # Larger axis text
#     axis.title = element_text(size = 16, face = "bold"),  # Bold axis titles
#     plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Centered, bold title
#     panel.grid.major.y = element_blank(),  # Remove horizontal gridlines
#     panel.grid.minor = element_blank(),  # Remove minor gridlines
#     axis.ticks = element_line(color = "black"),  # Visible axis ticks
#     legend.position = "none"  # No legend (optional, you can turn it on if needed)
#   )
# 
# 
#   print(plot)
#   dev.off()
# 
#   coefs_df$fc_horizon <- h
#   coefs_list_final[[paste0("horizon_",h)]] <- coefs_df
# 
# 
#   model <- models_list[[paste0("horizon_",h)]]
#   model <- model$finalModel
#   betas <- as.matrix(model$beta)
#   lambdas = model$lambda
#   names(lambdas) = colnames(betas)
#   coefs <-  as.data.frame(betas) %>%
#     tibble::rownames_to_column("variable") %>%
#     pivot_longer(-variable) %>%
#     mutate(lambda=lambdas[name])
# 
#   png(file=paste0("results/",model_name,"/coefficients/",model_name,"_coefs_lineplot_h-",h,".png"),
#       width = 1000, height = 600, units = "px")

  #### COEFS LINES VS. LAMBDAS  ----
  
  # line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "solid", "dashed", "dotted", "dotdash",
  #                 "longdash", "twodash", "solid", "dashed", "dotted", "dotdash", "solid","dashed","solid")

  # Create the plot with default color palette and different line types
  # p <- ggplot(coefs, aes(x = lambda, y = value, col = variable, linetype = variable)) +
  #   geom_line(size = 1.2) +  # Make lines slightly thicker for better visibility
  #   scale_y_continuous(limits = c(min(coefs$value), max(coefs$value))) +  # Adjust y-axis to include both negative and positive values
  #   scale_x_log10() +  # Log scale for x-axis
  #   scale_linetype_manual(values = line_types) +  # Manual line types
  #   theme_light(base_size = 18) +
  #   ggtitle(paste0("Coefficients of Predictors vs. Lambda  in ", model_name, " FC-Horizon = ",h)) +
  #   xlab("log10 (Lambda)") +
  #   ylab("Beta Value") +
  #   theme(
  #     legend.position = "right",  # Legend on the right side for clarity
  #     legend.title = element_text(size = 14),
  #     legend.text = element_text(size = 12),
  #     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
  #     axis.text = element_text(size = 14),  # Increase axis text size
  #     axis.title = element_text(size = 16)  # Increase axis title size
  #   )
  # print(p)
  # dev.off()
  
  
  
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

# START LOOP ----
  for (s  in unique(coefs_df_all$station)) {
    
    s <- "kienstock"
    coefs_df <- coefs_df_all %>% filter(station ==s)
    top_coefs <- coefs_df_all %>% filter(station == s) %>% setDT() %>% 
    .[variable != "(Intercept)", .(coefs = sum(abs(coefs))), by = variable]
  
    top_coefs <- top_coefs %>%  arrange(desc(coefs))
  
    top_coefs <- top_coefs$variable[1:15]



  png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_coefs_bars_all_fc-h_",s,".png"),
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
      title = paste0("Station: ",s," | Importance of 20 Most Important Predictor Variables in ",model_name),
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
  
  
  png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_coefs_lines_shadings.png"),
      width = 1000, height = 600, units = "px")
  
    coefs_df %>%
      filter(variable %in% top_coefs[1:16]) %>%
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
           subtitle = paste0("Model: ", model_name, " from: ", Sys.Date()))+
      scale_x_continuous(breaks = seq(1,12,1))
    dev.off()
 #### Error BARS Version 2 ----
    
  png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_coefs_error_bars.png"),
        width = 1000, height = 600, units = "px")
    
    coefs_df %>%
      filter(variable %in% top_coefs[1:16]) %>%
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
        subtitle = paste0("Model: ", model_name, " from: ", Sys.Date())
      ) +
      theme_bw() +                                                            
      theme(
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "none")
    dev.off()
    
}
      
        
    
#### LAMBDA vs. RMSE Plots ----
 
lambda_df <- purrr::imap_dfr(results, ~ {
  
  station <- .y
  
  
  purrr::imap_dfr(.x, ~ {
    
    horizon_char <- .y
    
    

    df <- .x$global_optimization_results

        df <- df %>% mutate(horizon_char = horizon_char,
                        station = station) 

    })
  })

lambda_df$fc_horizon <- stringr::str_remove(lambda_df$horizon_char, "fc_horizon_") %>%
  as.numeric()

lambda_df$n_horizon <- lambda_df$fc_horizon %>% 
  factor(levels = sort(unique(.)))

lambda_df <- lambda_df %>% filter(station == s)

SE.factor <- results[[paste0(s)]][["fc_horizon_1"]][["params"]][["SE.factor"]]
  
lambda_min <- lambda_df %>%
    group_by(fc_horizon) %>%
    slice_min(RMSE, n = 1) %>%
    ungroup() %>%
    select(fc_horizon, lambda_min = lambda, RMSE = RMSE, RMSESD = RMSESD)
  
  lambda_min$threshold <- lambda_min$RMSE + lambda_min$RMSESD * SE.factor #labelled as SD but actaully represents the Standard Error
  
  lambda.1se_graf <- as.numeric(c(rep(NA,(max(lambda_min$fc_horizon)))))
  
  for(h in 1:nrow(lambda_min)){

      temp <- lambda_df[which(lambda_df$fc_horizon == h),]
      temp <- temp[temp$RMSE <= lambda_min$threshold[h],]
      lambda.1se_graf[h] <- temp$lambda[which.max(temp$lambda)]

  }
  
  lambda_1se_df <- tibble(
    fc_horizon = lambda_min$fc_horizon,
    lambda_1se = lambda.1se_graf  )
  
  lambda_lines  <- lambda_min %>%
    select(fc_horizon, lambda_min) %>%
    left_join(lambda_1se_df, by = "fc_horizon")
  
  # 
  # lambda.min <- lambda_df$lambda[which.min(lambda_df$RMSE)]
  # 
  # threshold <- min(lambda_df$RMSE) + (lambda_df$RMSESD[which.min(lambda_df$RMSE)] * SE.factor) #labelled as SD but actaully represents the Standard Error
  # 
  # temp <- lambda_df[lambda_df$RMSE <= threshold,]
  # 
  # lambda.1se <- temp$lambda[which.max(temp$lambda)]
  # 
  lambda_df$RMSE_lb <- lambda_df$RMSE-lambda_df$RMSESD
  lambda_df$RMSE_ub <- lambda_df$RMSE+lambda_df$RMSESD
  
# lambda_df %>% 
#   ggplot()+
#   geom_line(aes(x=log10(lambda),y = RMSE))+
#   facet_wrap(~fc_horizon)+
#   geom_line(aes(x=log10(lambda),y = RMSE_lb), color = "red", linetype = "dotted")+
#   geom_line(aes(x=log10(lambda),y = RMSE_ub), color = "red", linetype = "dotted")+
#   geom_vline(xintercept = c(log10(lambda.min), log10(lambda.1se)))
# 
  png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_lambda_plot_",s,".png"),
      width = 1000, height = 600, units = "px")
  
plot <- lambda_df %>%
  ggplot(aes(x = log10(lambda), y = RMSE)) +
  geom_line() +
  facet_wrap(~fc_horizon) +
  geom_line(aes(y = RMSE_lb), color = "darkred", linetype = "dotted") +
  geom_line(aes(y = RMSE_ub), color = "darkred", linetype = "dotted") +
  geom_vline(
    data = lambda_lines,
    aes(xintercept = log10(lambda_min)),
    linetype = "dashed", color = "steelblue"
  ) +
  geom_vline(
    data = lambda_lines,
    aes(xintercept = log10(lambda_1se)),
    linetype = "dashed", color = "forestgreen"
  ) +
  labs(title = paste0("RMSE across log10(lambda) per forecast horizon in ", model_name," | ",s, " | ",Sys.Date()),
       subtitle = "Blue = lambda.min, Green = lambda.1se",
       x = "log10(lambda)", y = "RMSE") +
  theme_minimal()
print(plot)
dev.off()

#### QUANTILE BASED EVALUATION ----
#   if (file.exists(paste0("results/",today,"/",model_name,"/quantile_evaluation/"))){
#   } else {
#     dir.create(file.path(paste0("results/",today,"/",model_name,"/quantile_evaluation/")))
#   }
#   
#   quant_eval_95 <- do.call(rbind,forecasts_list) %>% 
#     setDT()
#   quant_eval_95 <- quant_eval_95[,.(pred_Q95 = as.factor(pred < quantiles$Q95),
#                                     obs_Q95 = as.factor(obs < quantiles$Q95),
#                                     Date = Date,
#                                     horizon = horizon)]
#  for (h in 1:n_horizons) {
#    
#   cm <- caret::confusionMatrix(
#     data = quant_eval_95$pred_Q95[quant_eval_95$horizon == h], 
#     reference = quant_eval_95$obs_Q95[quant_eval_95$horizon == h])
#                                
#   cm_df <- data.frame(accuracy = cm[["overall"]][["Accuracy"]],
#                       kappa = cm[["overall"]][["Kappa"]],
#                       sensitivity = cm[["byClass"]][["Sensitivity"]],
#                       specificity = cm[["byClass"]][["Specificity"]],
#                       horizon = h)
#   cm_list[[paste0("fc_horizont",h)]] <- cm_df
#   
# }
# 
# #### Accuracy, kappa, Sensitivity, Specifitiy Comparison over horizons ----
#   
# cm_df_all <- do.call(rbind,cm_list) %>% 
#   setDT()
# cm_long <- pivot_longer(cm_df_all,cols = c("accuracy","kappa","sensitivity","specificity") ,
#              names_to = "characteristic", 
#              values_to = "value")
# cm_long$characteristic <- as.factor(cm_long$characteristic)
# 
# png(file=paste0("results/",today,"/",model_name,"/quantile_evaluation/",model_name,"_confusion_bars.png"),
#     width = 800, height = 600, units = "px")
# 
# plot <- cm_long %>% 
#   ggplot(aes(x = horizon, y = value)) +
#   geom_col(fill = "steelblue") +  # Optional color tweak
#   geom_text(aes(label = round(value, 2)), 
#             vjust = -0.5, 
#             color = "black", 
#             size = 5, 
#             fontface = "bold") +  # Value labels
#   facet_wrap(~ str_to_title(characteristic)) +  # Capitalize facet titles
#   theme_minimal(base_size = 14) +  # Increase base font size
#   theme(
#     axis.title.y = element_blank(),  # Remove Y-axis title
#     axis.text.x = element_text(size = 12, face = "bold"),
#     axis.text.y = element_text(size = 12),
#     strip.text = element_text(size = 14, face = "bold"),  # Facet label styling
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
#   ) +
#   labs(
#     x = "Horizon",
#     title = "Comparison Across Horizons"
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
# 
# dev.off()
# 
# #### RESIDUALS Analysis -----------
# 
# forecast_df <- forecast_df %>% 
#   setDT() %>% 
#   .[, res := (obs - pred)]
# ######################################################################### next plot 
# quantile_lines <- data.frame(
#   xintercept = c(
#                  quantiles$Q94,
#                  quantiles$Q95
#                   ,quantiles$Q96
#                  ),
#   Quantile = c(
#     "Q94",
#     "Q95" 
#     ,"Q96"
#     ))

# png(file=paste0("results/",today,"/",model_name,"/quantile_evaluation/",model_name,"_residuals_quantile_lines.png"),
#     width = 800, height = 600, units = "px")
# 
# plot <- forecast_df %>% 
#   ggplot(aes(x = obs, y = res, color = as.factor(horizon))) +
#   geom_point(alpha = 0.7) +
#   # facet_wrap(~horizon) +
#   geom_vline(data = quantile_lines, 
#              aes(xintercept = xintercept, color = Quantile), 
#              linetype = "dashed", size = 0.7) +
#   scale_color_manual(
#     name = "Legend",
#     values = c(
#       "Q94" = "yellow",
#       "Q95" = "orange",
#       "Q96" = "darkred",
#       "0" = "grey",  # adjust for horizons if needed
#       "1" = "steelblue",
#       "2" = "forestgreen",
#       "3" = "violet"
#     )
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "right",
#     strip.text = element_text(size = 14, face = "bold")
#   ) +
#   labs(x = "Observed Flow [m3/s]",
#      y = "Residuals [m3/s]",
#      color = "Horizon / Quantiles")+
#   geom_hline(yintercept = 0, size = 0.5)+
#   ggtitle(label = paste0("Forecasting Residuals by Horizon of model ",model_name), 
#           subtitle = "Residuals = Forecasting - Observed")
# print(plot)
# dev.off()
# 
# 
# 
# png(file=paste0("results/",today,"/",model_name,"/quantile_evaluation/",model_name,"_residuals_quantile_lines_split.png"),
#     width = 800, height = 600, units = "px")
# 
# plot <- forecast_df %>% 
#   ggplot(aes(x = obs, y = res, color = as.factor(horizon))) +
#   geom_point(alpha = 0.7) +
#   facet_wrap(~horizon) +
#   geom_vline(data = quantile_lines, 
#              aes(xintercept = xintercept, color = Quantile), 
#              linetype = "dashed", size = 0.7) +
#   scale_color_manual(
#     name = "Legend",
#     values = c(
#       "Q94" = "yellow",
#       "Q95" = "orange",
#       "Q96" = "darkred",
#       "0" = "grey",  # adjust for horizons if needed
#       "1" = "steelblue",
#       "2" = "forestgreen",
#       "3" = "violet"
#     )
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "right",
#     strip.text = element_text(size = 14, face = "bold")
#   ) +
#   labs(x = "Observed Flow [m3/s]",
#        y = "Residuals [m3/s]",
#        color = "Horizon / Quantiles")+
#   geom_hline(yintercept = 0, size = 0.5)+
#   ggtitle(label = "Forecasting Residuals by Horizon", 
#           subtitle = "Residuals = Forecasting - Observed")
# print(plot)
# dev.off()
# 
# forecast_df %>%
#   ggplot(aes(x=date, y= pred))+
#   geom_line(aes(alpha = 0.8))+
#   geom_line(aes(y=obs, linetype = "dotted"), color = "lightgrey")+
#   facet_wrap(~horizon)
# 


#### GOF vs. Horizons Plot ----------
if (file.exists(paste0("results/",today,"/",model_name,"/GOF/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/GOF/")))
}

start_date_GOF <- forecast_df %>% 
  filter(horizon == as.numeric(max(forecast_df$horizon))) 

start_date_GOF <- start_date_GOF$date[1]

load(file =  paste0("data/",s,"/Final_df_",s,"_weekly.RData"))

naive_season <- df %>%   filter(Date < start_date_GOF) %>%
 select(c("Date","flow_mean","week_year")) %>%  rename(date = Date, flow = flow_mean)

naive_season <- naive_season %>%  setDT() %>% .[, naive_season := mean(flow, na.rm = T), by = week_year]

mean_obs <- df %>%   filter(Date > start_date_GOF) %>% pull(flow_mean) %>% 
  mean()

# CALCULATION OF FLOW QUANTILES ----

quantiles <- sapply(1:100, function(x) quantile(df$flow_mean, probs = 1-(x/100))) %>% 
  data.frame(quant_flow = ., quantile = 1:100)

cum_seg_results <- NULL

mean_obs_tot <- df %>%   filter(Date > start_date_GOF) %>% pull(flow_mean) %>% 
  mean()

for(q in 1:100){
  
  # q <- 90
  
  cum_seq_th <- quantiles %>% filter(q == quantile) 
  cum_seg <- forecast_df %>% filter(obs < cum_seq_th$quant_flow)
  cum_seg <- cum_seg %>% setDT() %>% 
    .[, {
      mean_pred <- mean(pred)
      mean_obs_tot <- mean_obs_tot
      mean_obs_seg <- mean(obs)
      ssr <- sum((pred - mean_obs)^2)
      sstot <- sum((obs - mean_obs)^2)
      r2 <- 1-(ssr / sstot)
      RMSE = hydroGOF::rmse(sim = pred, obs = obs)
      r2_2 = hydroGOF::R2(sim = pred, obs = obs)
      
      .(r2= r2,RMSE= RMSE, mean_pred = mean_pred, mean_obs_seg = mean_obs_seg, mean_obs_tot = mean_obs_tot,r2_2 =r2_2)
    }, by = horizon]
  
  cum_seg$exceed_prob <- q
  cum_seg_results <- rbind(cum_seg, cum_seg_results)
  
}


cum_seg_results %>% 
  filter(horizon==5) %>% 
  ggplot() +
  # geom_line(aes(x = exceed_prob, y = mean_pred, color = "Predicted Mean"))+
  # geom_line(aes(x = exceed_prob, y = mean_obs, color = "Observed Mean", linetype="dotted"))+
  # geom_line(aes(x = exceed_prob, y = RMSE, color = "RMSE"))+
  geom_line(aes(x = exceed_prob, y = r2, color = "r2"))+
  geom_line(aes(x = exceed_prob, y = r2_2, color = "r2_2"))

quantile_y_intercept <- quantiles %>% filter(quantile %in% seq(10,100,10)) %>% pull(quant_flow)
ylims <- df %>% filter(Date > start_date_GOF) %>% pull(flow_mean) %>% range()



p1 <- df %>%
    filter(year > 2015) %>%
    ggplot() +
    geom_line(aes(x = Date, y = flow_mean, color = "Observed Flow [m3/s]"), linewidth = 1) +
    geom_hline(yintercept = quantile_y_intercept, linetype = "dashed", aes(color = "Flow Quantiles [m3/s]",)) +
    scale_color_manual(values = c("Observed Flow [m3/s]" = "steelblue","Flow Quantiles [m3/s]" = "lightgrey")) +
    lims(y = ylims)+
    theme_bw() +
    theme(legend.position = "top")+
    labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Clean legend title
    title = paste0("Hydrographs and Quantile based RMSE visualization \nModel: ", model_name," | ",s)
    # subtitle = "Grey line indicates Observed Flow"
  ) 

p2 <- cum_seg_results %>% 
  filter(horizon == 9) %>% 
  filter(exceed_prob %in% seq(10,100,10)) %>% 
  ggplot()+
  geom_point(aes(y = mean_obs_seg, x = RMSE)) +
  lims(y = ylims)+
  
  # geom_text(aes(label = quantile), hjust = 0) +
  # scale_y_continuous(limits = range(df$flow_mean, quant_data$quant_flow)) +
  theme_bw() +
  theme(
    plot.margin = margin(5, 0, 5, 0),
    axis.text.y = element_text(),
    axis.ticks.y = element_line()
  )


combined <- p1 + p2 + plot_layout(ncol = 2, widths = c(5, 1))
print(combined)

png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_quantile_eval_hydrograph_",s,".png"),
    width = 1000, height = 600, units = "px")


print(combined)
dev.off()




##### Hydrographs of predicted vs. observed flows ---------


if (file.exists(paste0("results/",today,"/",model_name,"/hydrographs/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/hydrographs/")))
}

png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_hydrogaphs_facet_",s,".png"),
    width = 1000, height = 600, units = "px")


plot <- forecast_df %>% 
  filter(lubridate::year(date) == 2021) %>% 
  filter(station == s) %>% 
  ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast lines colored by horizon
  geom_line(size = 1.2) +  # Slightly thicker forecast lines
  geom_line(aes(y = obs), color = "grey", linetype = "solid", size = 1.2) +  # Darker grey for observed line
  facet_grid(horizon ~ ., scales = "free_y") +  # Facet by horizon, free y-axis
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Rename legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s),
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


png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_hydrographs_stacked",s,".png"),
    width = 1000, height = 600, units = "px")

plot <- forecast_df %>% 
  filter(lubridate::year(date) == 2015) %>% 
  filter(station == s) %>% 
  ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast colored by horizon
  geom_line(size = 1.2, alpha = 0.9) +  # Thicker, slightly transparent forecast lines
  geom_line(aes(y = obs), color = "darkgrey", linetype = "solid", size = 1.4, alpha = 1) +  # Dark grey observed line
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Clean legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s),
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

# CREATING FORECAST_DF WITH ALL FC-METHODS ----

forecast_df <- df %>% rename(date = Date) %>%  select(c("week_year","date")) %>% 
  left_join(forecast_df,.,by = "date")

# forecast_df <- naive_season %>% select(c("week_year","naive_season")) %>% 
#   left_join(forecast_df,.,by= c("week_year"))

forecast_df <- naive_season %>%
  distinct(week_year, .keep_all = TRUE) %>%
  select(week_year, naive_season) %>%
  left_join(forecast_df,., by = "week_year")

forecast_df <- ensemble_df %>% filter(ensemble == "ensemble_median" & station == s) %>%  
  select(c("date","pred","horizon")) %>% 
  rename(ensemble_median = pred) %>% 
  left_join(forecast_df,.,by= (c("date","horizon")))

forecast_df <- upper_BM_df %>%  filter(station == s) %>% 
  select(c("date","pred","horizon")) %>% 
  rename(upper_benchmark = pred) %>% 
  left_join(forecast_df,.,by= (c("date","horizon")))

forecast_df <- forecast_df %>% setDT() %>% 
    .[, naive_lag := lag(obs, horizon), by = horizon]

forecast_df$res <- forecast_df$pred - forecast_df$obs

forecast_df_long <-forecast_df %>% rename(catchment_memory_model = pred) %>% 
  select(-"fc_method") %>% pivot_longer(cols = c("naive_season","upper_benchmark","ensemble_median",
                                                          "catchment_memory_model","naive_lag"), values_to = "pred",
                                                names_to = "fc_method")  



# quantiles <- sapply(1:100, function(x) quantile(df$flow_mean, probs = 1-(x/100))) %>% 
#   data.frame(quant_flow = ., quantile = 1:100)

fc_df_quant_GOF <- NULL

# mean_obs_tot <- df %>%   filter(Date > start_date_GOF) %>% pull(flow_mean) %>% 
#   mean()

for(q in 1:100){
  
  # q <- 90
  
  cum_seq_th <- quantiles %>% filter(q == quantile) 
  fc_df_quant <- forecast_df_long %>% filter(obs < cum_seq_th$quant_flow)
  fc_df_quant <- fc_df_quant %>% setDT() %>% 
    .[, {
      mean_pred <- mean(pred)
      mean_obs_tot <- mean_obs_tot
      mean_obs_seg <- mean(obs)
      ssr <- sum((pred - mean_obs)^2)
      sstot <- sum((obs - mean_obs)^2)
      r2 <- 1-(ssr / sstot)
      RMSE = hydroGOF::rmse(sim = pred, obs = obs)
      r2_2 = hydroGOF::R2(sim = pred, obs = obs)
      
      .(r2= r2,RMSE= RMSE, mean_pred = mean_pred, mean_obs_seg = mean_obs_seg, mean_obs_tot = mean_obs_tot,r2_2 =r2_2)
    }, by = .(horizon, fc_method)]
  
  fc_df_quant$exceed_prob <- q
  fc_df_quant_GOF <- rbind(fc_df_quant, fc_df_quant_GOF)
  
}




png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_hydrographs_stacked",s,".png"),
    width = 1000, height = 600, units = "px")

plot <-
  fc_df_quant_GOF %>% 
  ggplot() +
  geom_line(aes(x = exceed_prob, y = RMSE, color = fc_method), linewidth = 1)+
  facet_wrap(~horizon) +
  theme_bw()+
  lims(y = range(fc_df_quant_GOF$RMSE))+
  labs(
    y = "RMSE [m³/s]",
    x = "Exceedance Probability [%]",
    color = "Forecasting Model",  # Clean legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",s)
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




png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_stratified_GOF_",s,".png"),
    width = 1000, height = 600, units = "px")

plot <- fc_df_quant_GOF %>% filter(exceed_prob %in% c(1,50,80,95)) %>% 
  ggplot()+
  geom_line(aes(x=horizon, y = RMSE, color = fc_method),linewidth = 1) +
  theme_bw()+
  facet_wrap(~exceed_prob)+
  lims(y = range(fc_df_quant_GOF$RMSE))+
  labs(
    y = "RMSE [m³/s]",
    x = "Forecasting Horizon",
    color = "Forecasting Model",  # Clean legend title
    title = paste0("RMSE along Forecasting Horizons for different Flow Quantiles \nModel: ", model_name," | ",s)
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





# 
# strat_quantiles <- c("all_data","Q95", "Q90","Q50")
# results_strat_quant <- NULL
# GOF_long <- NULL
# GOF <- NULL
# GOF_ratios <- NULL
# forecast_df_quant <- NULL
# 
# for(quantile in strat_quantiles) {
#   
#   # quantile <- "Q90"
#   
#   forecast_df_temp <- forecast_df %>% na.omit()
#   forecast_df_temp$res <- forecast_df_temp$pred - forecast_df_temp$obs
#   
#   forecast_df_temp <- forecast_df_temp %>%  
#     filter(date >= start_date_GOF)
#   
#   threshold <- Inf
#   
#   if(quantile != "all_data"){
#     
#     threshold <- quantiles %>% select(quantile)
#     threshold <- threshold[1,1]
#   
#     }
#   
#   forecast_df_temp <- forecast_df_temp %>%  rename(catchment_memory = pred) %>% 
#     pivot_longer(cols = c(catchment_memory, naive_season,naive_lag,
#                           # ensemble_mean,
#                           ensemble_median,
#                           upper_benchmark),
#                  values_to = "pred",
#                  names_to = "fc_method")
#   
#   forecast_df_temp_quant <- forecast_df_temp %>% filter(obs < threshold)
#   
#   forecast_df_temp_quant$res <- forecast_df_temp_quant$pred - forecast_df_temp_quant$obs
#   
#   GOF_temp_all_data <- forecast_df_temp %>%  setDT() %>% 
#     .[, .(R2 = hydroGOF::R2(pred,obs),
#           RMSE = hydroGOF::rmse(pred, obs)
#           # MAE = hydroGOF::mae(pred,obs)
#           ), by = .(horizon, fc_method)]
#   
#   
#   GOF_fc_model <- GOF_temp_all_data %>% filter(fc_method == "catchment_memory")
#   
#   
#   GOF_wide <- GOF_temp_all_data %>%
#     # select(-("R2")) %>% 
#     pivot_wider(
#       names_from = fc_method,
#       values_from = c(RMSE,R2)
#     )
#   
#   GOF_wide$quantile <- quantile
#   
#   GOF_ratios_temp <- GOF_wide %>%
#     mutate(
#       RMSE_scaled_season = RMSE_naive_season / RMSE_catchment_memory,
#       RMSE_scaled_lag    = RMSE_naive_lag / RMSE_catchment_memory
#      
#     ) %>%
#     select(horizon, starts_with("RMSE_scal"), starts_with("MAE_scal"))
#   
#   GOF_temp <- merge(GOF_ratios_temp, GOF_fc_model, by = "horizon")
#   
#   GOF_long_temp <- GOF_temp %>% pivot_longer(cols = c(RMSE_scaled_season, RMSE_scaled_lag, 
#                                                       R2),
#                                    values_to = "values",
#                                    names_to = "GOF_metric") %>% 
#                               select(-c("RMSE"))
#   
#   GOF_long_temp$quantile <- quantile
#   GOF_temp$quantile <- quantile
#   
#   strat_quant <- forecast_df_temp_quant %>%  setDT() %>% 
#     .[, .(n = .N, 
#           R2_quant = 1 - (sum((pred-obs)^2) / (sum((mean(obs)-obs)^2))),
#           RMSE_quant = hydroGOF::rmse(pred, obs)), by = .(horizon,fc_method)]
#   
#   strat_quant <- GOF_temp_all_data %>% left_join(strat_quant,.,by = c("horizon", "fc_method"))
#   
#  
#   strat_quant$quantile <- quantile
#   
#   results_strat_quant <- rbind(results_strat_quant, strat_quant)
#   
#   forecast_df_temp_quant$quantile <- quantile
#   
#   forecast_df_quant <- rbind(forecast_df_temp_quant,forecast_df_quant)
#   GOF <- rbind(GOF, GOF_temp)
#   GOF_long <- rbind(GOF_long, GOF_long_temp)
#   rm(GOF_long_temp)
#   rm(GOF_temp)
# 
# }
# 
# 
# png(file=paste0("results/",today,"/",model_name,"/GOF/",model_name,"_GOF_vs_Horizons.png"),
#     width = 1000, height = 600, units = "px")
# 
# GOF_long %>% 
#   filter(!(GOF_metric %in% c("MAE_scaled_lag", "MAE_scaled_season"))) %>% 
#   ggplot()+
#   geom_line(aes(x=horizon, y=values, color = GOF_metric), linewidth = 1.5)+
#   geom_hline(yintercept = 1)+
#   theme_bw()+
#   labs(title = "GOF metrics vs. forecasting horizons",
#        subtitle = "Scaling was done by divding RMSE of the naive forecasts by the forecast of the model",
#        x = "Forecasting Horizons",
#        y = "GOF Values")+
#   scale_x_continuous(breaks = seq(1,n_horizons,1))+
#   scale_y_continuous(breaks = seq(0,5,0.5))
# 
# dev.off()




#### Stratifized Error Measures ----
# 
# ensemble_GOF <- ensemble_df %>% setDT() %>% .[ensemble %in% c("ensemble_mean","ensemble_median"), 
#                                               .(RMSE = hydroGOF::rmse(pred,obs),
#                                               R2 = hydroGOF:: R2(pred,obs)), by = .(ensemble,horizon)]
# 
# 
# png(file=paste0("results/",today,"/",model_name,"/GOF/",model_name,"_stratified_RMSE_naive_models.png"),
#     width = 1000, height = 600, units = "px")
# 
# plot <- results_strat_quant %>% 
#   filter(fc_method != "naive_seasosn") %>% 
#   filter(quantile %in% c("all_data")) %>% 
#   ggplot()+
#   geom_line(aes(x=horizon, y= RMSE_quant, color = fc_method), linewidth = 1.5)+
#   geom_hline(yintercept = 0)+
#   theme_bw()+
#   labs(title = "RMSE vs. Forecasting Horizons",
#        subtitle = "Comparison of Naive Models with Elastic Net Model",
#        x = "Forecasting Horizons",
#        y = "RMSE [m^3/s]")+
#   scale_x_continuous(breaks = seq(1,n_horizons,1))+
#   # lims(y = c(-3,3))+
#   # scale_y_continuous(breaks = seq(0,5,0.5))+
#   facet_wrap(~quantile)
# print(plot)
# dev.off()


# png(file=paste0("results/",today,"/",model_name,"/GOF/",model_name,"_stratified_RMSE_naive_models.png"),
#     width = 1000, height = 600, units = "px")
# 
# plot <- results_strat_quant %>% 
#   filter(fc_method != "naive_season") %>% 
#   filter(quantile %in% c("all_data","Q90")) %>% 
#   ggplot()+
#   geom_line(aes(x=horizon, y= R2_quant, color = fc_method), linewidth = 1.5)+
#   geom_hline(yintercept = 0)+
#   theme_bw()+
#   labs(title = "RMSE vs. Forecasting Horizons",
#        subtitle = "Comparison of Naive Models with Elastic Net Model",
#        x = "Forecasting Horizons",
#        y = "RMSE [m^3/s]")+
#   scale_x_continuous(breaks = seq(1,n_horizons,1))+
#   # lims(y = c(-3,3))+
#   # scale_y_continuous(breaks = seq(0,5,0.5))+
#   facet_wrap(~quantile)
# print(plot)
# dev.off()

png(file=paste0("results/",today,"/",model_name,"/GOF/",model_name,"_residuals_boxplot_model_comparison.png"),
    width = 1000, height = 600, units = "px")

plot <- forecast_df %>% filter(exceed_prob %in% c(1,50,80,95)) %>% 
  filter(fc_method != "naive_season") %>% 
  filter(horizon %in% seq(1,12,2)) %>% 
  ggplot() +
  geom_boxplot(aes(x=as.factor(horizon), y = res,fill =fc_method), color = "black" )+
  # facet_wrap(~quantile)+
  labs(title = "Residuals per Forecasting Horizons",
       subtitle = "Comparison of Naive Models with Elastic Net Model | Residuals = Predicted - Observed" ,
       x = "Forecasting Horizons [weeks]",
       y = "Residuals [m^3/s]")+
  theme_bw()
print(plot)
dev.off()


png(file=paste0("results/",today,"/",model_name,"/GOF/",model_name,"_residuals_naive_models.png"),
    width = 1000, height = 600, units = "px")

plot <- forecast_df_quant %>% 
  filter(quantile == "Q90") %>%
  ggplot() +
  geom_point(aes(x=obs, y = res,color =fc_method))+
  labs(title = "RMSE vs. Forecasting Horizons",
       subtitle = "Comparison of Naive Models with Elastic Net Model",
       x = "Forecasting Horizons",
       y = "Residuals [m^3/s]")+
  # facet_wrap(~quantile)+
  theme_bw()
print(plot)
dev.off()

# 
# temp %>%
#   filter(year(date) == 2016) %>%
#   ggplot(aes(x=date,y=naive_season))+
#   geom_line()

####
h <- 9

forecast_df_quant %>% 
  filter(lubridate::year(date) %in% 2018:2019) %>% 
  filter(fc_method %in% c(
    "naive_lag",
                          "catchment_memory",
    "upper_benchmark"
                          # ,"naive_season"
                          
  )) %>% 
  filter(horizon == h) %>% 
  
  ggplot(aes(x = date, y = pred, color = factor(fc_method))) +  # Forecast colored by horizon
  geom_line(size = 1, alpha = 0.9) +  # Thicker, slightly transparent forecast lines
  geom_line(aes(y = obs), color = "black", linetype = "solid", size = 1.4, alpha = 1) +  # Dark grey observed line
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Clean legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name),
    subtitle = paste0("Grey line indicates Observed Flow | Horizon = ",h)
  ) +
  theme_light(base_size = 14) +
  geom_hline(yintercept = quantiles$Q95, color = "grey", linetype = "dotted",linewidth = 1)+
  # Clean light theme with larger fonts
  theme(
    legend.position = "bottom",  # Keep legend on the right
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


#### Continuous Ranked Probability Score CRPS ----

#plotting distribution of residuals 
forecast_df_temp$res <- forecast_df_temp$pred-forecast_df_temp$obs

forecast_df_temp %>%
  filter(horizon == 1) %>% 
  ggplot()+
  geom_histogram(aes(x=res),color = "black",fill = "steelblue")+
  facet_wrap(~fc_method)+
  geom_vline(xintercept = 0, color = "darkred")

forecast_df_temp %>%
  filter(horizon == 1) %>% 
  ggplot()+
  stat_ecdf(aes(x=res,color = fc_method),fill = "steelblue",geom="step",size = 1.5)+
  # facet_wrap(~fc_method)+
  geom_vline(xintercept = 0, color = "darkred")

forecast_df_temp %>%
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

forecast_df_temp %>%
  filter(horizon == 1) %>% 
  ggplot()+
  geom_point(aes(x=obs,y=res),fill = "steelblue")+
  # facet_wrap(~fc_method)+
  geom_vline(xintercept = 0, color = "darkred")



#### Summary Table ----------

summary_table <- GOF %>% 
  filter(quantile == "all_data") %>% 
  select(c("horizon","R2","RMSE","RMSE_scaled_season","RMSE_scaled_lag"))

n_coefs <- rownames_to_column(n_coefs)

names(n_coefs) <- c("horizon", "n_coefs")

n_coefs$horizon <- gsub("fc_horizon_", "", n_coefs$horizon)

n_coefs$horizon <- as.numeric(n_coefs$horizon)

summary_table$horizon <- as.numeric(summary_table$horizon)

summary_table <- merge(summary_table, n_coefs, by = "horizon")

# summary_table$lambda.applied <- summary_table$lambda.value[1]
setcolorder(summary_table, c("horizon", "R2", "RMSE", "RMSE_scaled_season","RMSE_scaled_lag", 
                             # "lambda.applied",
                             "n_coefs"))

# Create a basic gt table
gt_table <- gt(summary_table) %>%
  tab_header(
    title = "Summary Statistics of Monthly Forecasting Performance",
    subtitle = paste0("Model: ", model_name," ",as.Date(Sys.time()))
  ) %>%
  fmt_number(
    columns = c(R2, RMSE, RMSE_scaled_season,RMSE_scaled_lag),
    decimals = 2
  ) %>%
  cols_label(
    horizon = md("Forecasting<br>Horizon<br>(weeks)"),
    R2 = md("R²"),
    RMSE = md("RMSE"),
    # lambda.value = md("Optimal<br>Lambda"),
    # lambda.applied = md("Applied<br>Lambda"),
    n_coefs = md("Optimal Number<br>of Coefficients"),
    RMSE_scaled_season = md("RMSE_scaled_season"),
    RMSE_scaled_lag = md("RMSE_scaled_lag")
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
gtsave(gt_table, paste0("results/",today,"/",model_name,"/summary_table_forecasts.png"))


# Print the table


