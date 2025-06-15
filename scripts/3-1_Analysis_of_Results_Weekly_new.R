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
rm(list=ls())

### name the models and dataset of interest ------
today <- Sys.Date() -1
# today <-
model_name <- c("Var_Sel_test_1_SE_sqrt_k")
dataset <- "lagged_TB" #TB = Tauchenbach
station <- "tauchenbach"

load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_forecasts_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_final_model_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"n_coefs_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"results.RData"))

load("data/flow_quantiles_list.RData")

quantiles <- flow_quantiles_list[[paste0(station)]]
GOF_df <- do.call(rbind,GOF_list)
coefs_list_final <- list()
forecast_df <- do.call(rbind,forecasts_list)
cm_list <- list()

### Visualizing Cofficients --------------

n_horizons <- as.numeric(length(unique(names(forecasts_list))))

if (file.exists(paste0("results/",today,"/",model_name,"/coefficients/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/coefficients/")))
}
n_coefs <- do.call(rbind,n_coefs_list)

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
  
  coefs_list <- lapply(coefs_list,rownames_to_column)
  
  coefs_df <- purrr::imap_dfr(coefs_list, ~bind_rows(.x) %>%  mutate(horizon = .y)) %>% 
    rename(coefs = s1)
  
  # coefs_df$horizon <- as.factor(coefs_df$horizon)
  
  coefs_df$n_horizon <- stringr::str_remove(coefs_df$horizon, "fc_horizon_") %>%
    as.numeric() %>%
    factor(levels = sort(unique(.)))
  
  # levels(coefs_df$horizon) <- seq(1,length(unique(coefs_df$horizon)),1)
  
  coefs_df <- coefs_df %>% rename(variable = rowname)
  
  # levels(coefs_df$n_horizon) <- seq(1,length(unique(coefs_df$horizon)),1)
  
  coefs_df$variable <- as.factor(coefs_df$variable)
  
  top_coefs <- coefs_df %>% setDT() %>% 
    .[variable != "(Intercept)", .(coefs = sum(coefs)), by = variable]
  
  top_coefs <- top_coefs %>%  arrange(desc(coefs))
  
  top_coefs <- top_coefs$variable[1:20]


  png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_coefs_bars_all_fc-h.png"),
      width = 1000, height = 600, units = "px")
  
  plot <- coefs_df %>%
    filter(variable %in% top_coefs) %>%
    mutate(variable = factor(variable, levels = top_coefs)) %>% 
    ggplot(aes(x = n_horizon, y = coefs, fill = n_horizon)) +
    geom_col(color = "black", width = 0.9) +
    facet_wrap(~variable, scales = "fixed") +
    labs(
      x = "Forecasting Horizon",
      y = "Coefficient of Predictors",
      fill = "Forecasting Horizon",
      title = paste0("Importance of 20 Most Important Predictor Variables in ",model_name),
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
  
  setDT(coefs_df)
  coefs_df %>% 
    .[, mean_coef := mean(abs(coefs)), by = variable]
  
  coefs_df$mean_coef[which(coefs_df$variable == "(Intercept)")] <-  0

  coefs_df$mean_coef <- coefs_df$mean_coef/max(coefs_df$mean_coef)
  coefs_df <- as_tibble(coefs_df)
  # png(file=paste0("results/",model_name,"/coefficients/",model_name,"_coefs_bars_all_fc-h.png"),
  #     width = 1000, height = 600, units = "px")
  # 
  cwb <- coefs_df %>% filter(str_detect(variable, "cwb")) %>% 
    # setDT() %>% 
    # .[coefs != 0 & variable != "(Intercept)"] %>%
    ggplot() +
    geom_line(aes(x = n_horizon, y = coefs, group = variable, color = variable, alpha = mean_coef), linewidth = 0.9) +
    # facet_wrap(~variable, scales = "fixed") +
    labs(
      x = "Forecasting Horizon",
      y = "Coefficient of Predictors",
      fill = "Forecasting Horizon"
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
  print(cwb)
  dev.off()
  
  #### Coefficient Consistency vs. horizons ----
  
  
  coefs_means <- purrr::imap_dfr(
    results,
    ~ {
      tibble(
        term = names(.x$coef_means),
        estimate = .x$coef_means,
        fc_horizon = as.integer(gsub("fc_horizon_", "", .y))
      )
    }
  )
  
  coefs_sd  <- purrr::imap_dfr(
    results,
    ~ {
      tibble(
        term = names(.x$coef_sds),
        estimate = .x$coef_sds,
        fc_horizon = as.integer(gsub("fc_horizon_", "", .y))
      )
    }
  )

  coefs_df  <- coefs_df %>%   mutate(fc_horizon = as.numeric(n_horizon))
  
  coefs_means  <- coefs_means %>%  rename(mean_coef_CV = estimate, variable = term)
  
  coefs_sd  <- coefs_sd %>%  rename(sd_coef = estimate, variable = term)
  
  coefs_means <- coefs_means %>%  mutate(fc_horizon = as.numeric(fc_horizon))
  
  coefs_sd <- coefs_sd %>%  mutate(fc_horizon = as.numeric(fc_horizon))
  
  coefs_df <- left_join(coefs_df, coefs_means, by = c("variable", "fc_horizon"))
  coefs_df <- left_join(coefs_df, coefs_sd, by = c("variable", "fc_horizon"))
  
  coefs_df$coefs_ub <-  coefs_df$mean_coef_CV + coefs_df$sd_coef 
  coefs_df$coefs_lb <-  coefs_df$mean_coef_CV - coefs_df$sd_coef 
  
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
      facet_wrap(~variable, scales = "fixed")+
      theme_bw()+
      labs(x= "Forecasting Horizon [weeks]",
           y = "Variable Coefficients",
           title = "Variable Coefficients vs. Forecasting Horizons",
           subtitle = paste0("Model: ", model_name, "from: ", Sys.Date()))+
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
      facet_wrap(~variable, scales = "fixed") +                                
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
    

      
        
    
#### LAMBDA vs. RMSE Plots ----
  
lambda_df <- imap_dfr(
    results,
    ~ mutate(.x$global_optimization_results, fc_horizon = as.integer(gsub("fc_horizon_", "", .y)))  )

  SE.factor <- results[["fc_horizon_1"]][["params"]][["SE.factor"]]
  
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
lambda_df %>%
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
  labs(title = paste0("RMSE across log10(lambda) per forecast horizon in ", model_name, Sys.Date()),
       subtitle = "Blue = lambda.min, Green = lambda.1se",
       x = "log10(lambda)", y = "RMSE") +
  theme_minimal()
  

#### QUANTILE BASED EVALUATION ----
  if (file.exists(paste0("results/",today,"/",model_name,"/quantile_evaluation/"))){
  } else {
    dir.create(file.path(paste0("results/",today,"/",model_name,"/quantile_evaluation/")))
  }
  
  quant_eval_95 <- do.call(rbind,forecasts_list) %>% 
    setDT()
  quant_eval_95 <- quant_eval_95[,.(pred_Q95 = as.factor(pred < quantiles$Q95),
                                    obs_Q95 = as.factor(obs < quantiles$Q95),
                                    Date = Date,
                                    horizon = horizon)]
 for (h in 1:n_horizons) {
   
  cm <- caret::confusionMatrix(
    data = quant_eval_95$pred_Q95[quant_eval_95$horizon == h], 
    reference = quant_eval_95$obs_Q95[quant_eval_95$horizon == h])
                               
  cm_df <- data.frame(accuracy = cm[["overall"]][["Accuracy"]],
                      kappa = cm[["overall"]][["Kappa"]],
                      sensitivity = cm[["byClass"]][["Sensitivity"]],
                      specificity = cm[["byClass"]][["Specificity"]],
                      horizon = h)
  cm_list[[paste0("fc_horizont",h)]] <- cm_df
  
}

#### Accuracy, kappa, Sensitivity, Specifitiy Comparison over horizons ----
  
cm_df_all <- do.call(rbind,cm_list) %>% 
  setDT()
cm_long <- pivot_longer(cm_df_all,cols = c("accuracy","kappa","sensitivity","specificity") ,
             names_to = "characteristic", 
             values_to = "value")
cm_long$characteristic <- as.factor(cm_long$characteristic)

png(file=paste0("results/",today,"/",model_name,"/quantile_evaluation/",model_name,"_confusion_bars.png"),
    width = 800, height = 600, units = "px")

plot <- cm_long %>% 
  ggplot(aes(x = horizon, y = value)) +
  geom_col(fill = "steelblue") +  # Optional color tweak
  geom_text(aes(label = round(value, 2)), 
            vjust = -0.5, 
            color = "black", 
            size = 5, 
            fontface = "bold") +  # Value labels
  facet_wrap(~ str_to_title(characteristic)) +  # Capitalize facet titles
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    axis.title.y = element_blank(),  # Remove Y-axis title
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),  # Facet label styling
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Horizon",
    title = "Comparison Across Horizons"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

dev.off()

#### RESIDUALS Analysis -----------

forecast_df <- forecast_df %>% 
  setDT() %>% 
  .[, res := (obs - pred)]
######################################################################### next plot 
quantile_lines <- data.frame(
  xintercept = c(
                 # quantiles$Q94, 
                 quantiles$Q95
                  # ,quantiles$Q96
                 ),
  Quantile = c(
    # "Q94", 
    "Q95" 
    # ,"Q96"
    ))

png(file=paste0("results/",today,"/",model_name,"/quantile_evaluation/",model_name,"_residuals_quantile_lines"),
    width = 800, height = 600, units = "px")

plot <- forecast_df %>% 
  ggplot(aes(x = obs, y = res, color = as.factor(horizon))) +
  geom_point(alpha = 0.7) +
  # facet_wrap(~horizon) +
  geom_vline(data = quantile_lines, 
             aes(xintercept = xintercept, color = Quantile), 
             linetype = "dashed", size = 0.7) +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Q94" = "yellow",
      "Q95" = "orange",
      "Q96" = "darkred",
      "0" = "grey",  # adjust for horizons if needed
      "1" = "steelblue",
      "2" = "forestgreen",
      "3" = "violet"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold")
  ) +
  labs(x = "Observed Flow [m3/s]",
     y = "Residuals [m3/s]",
     color = "Horizon / Quantiles")+
  geom_hline(yintercept = 0, size = 0.5)+
  ggtitle(label = paste0("Forecasting Residuals by Horizon of model ",model_name), 
          subtitle = "Residuals = Forecasting - Observed")
print(plot)
dev.off()



png(file=paste0("results/",today,"/",model_name,"/quantile_evaluation/",model_name,"_residuals_quantile_lines_split"),
    width = 800, height = 600, units = "px")

plot <- forecast_df %>% 
  ggplot(aes(x = obs, y = res, color = as.factor(horizon))) +
  geom_point(alpha = 0.7) +
  facet_wrap(~horizon) +
  geom_vline(data = quantile_lines, 
             aes(xintercept = xintercept, color = Quantile), 
             linetype = "dashed", size = 0.7) +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Q94" = "yellow",
      "Q95" = "orange",
      "Q96" = "darkred",
      "0" = "grey",  # adjust for horizons if needed
      "1" = "steelblue",
      "2" = "forestgreen",
      "3" = "violet"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold")
  ) +
  labs(x = "Observed Flow [m3/s]",
       y = "Residuals [m3/s]",
       color = "Horizon / Quantiles")+
  geom_hline(yintercept = 0, size = 0.5)+
  ggtitle(label = "Forecasting Residuals by Horizon", 
          subtitle = "Residuals = Forecasting - Observed")
print(plot)
dev.off()

forecast_df %>%
  ggplot(aes(x=date, y= pred))+
  geom_line(aes(alpha = 0.8))+
  geom_line(aes(y=obs, linetype = "dotted"), color = "lightgrey")+
  facet_wrap(~horizon)



##### Hydrographs of predicted vs. observed flows ---------


if (file.exists(paste0("results/",today,"/",model_name,"/hydrographs/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/hydrographs/")))
}

png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_hydrogaphs_facet.png"),
    width = 1000, height = 600, units = "px")


plot <- forecast_df %>% 
  filter(lubridate::year(date) == 2021) %>% 
  ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast lines colored by horizon
  geom_line(size = 1.2) +  # Slightly thicker forecast lines
  geom_line(aes(y = obs), color = "grey", linetype = "solid", size = 1.2) +  # Darker grey for observed line
  facet_grid(horizon ~ ., scales = "free_y") +  # Facet by horizon, free y-axis
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Rename legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name),
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


png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_hydrographs_stacked.png"),
    width = 1000, height = 600, units = "px")

plot <- forecast_df %>% 
  filter(lubridate::year(date) == 2015) %>% 
  
  ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast colored by horizon
  geom_line(size = 1.2, alpha = 0.9) +  # Thicker, slightly transparent forecast lines
  geom_line(aes(y = obs), color = "darkgrey", linetype = "solid", size = 1.4, alpha = 1) +  # Dark grey observed line
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Clean legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name),
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

print(plot)
dev.off()

#### GOF vs. Horizons Plot ----------
if (file.exists(paste0("results/",today,"/",model_name,"/GOF/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/GOF/")))
}

load(file = paste0("data/tauchenbach/Final_df_Tauchenbach_weekly.RData"))

temp <- df %>%  select(c("Date","flow_min","week_year")) %>%  rename(date = Date, flow = flow_min)

temp <- temp %>%  setDT() %>% .[, naive_season := mean(flow, na.rm = T), by = week_year]

forecast_df <- temp %>% select(c("date","naive_season")) %>% 
  right_join(forecast_df,temp, by = "date")

forecast_df <- forecast_df %>% setDT() %>% 
  .[, naive_lag := lag(obs, horizon), by = horizon]


forecast_df_temp <- forecast_df %>% na.omit()

start_date_GOF <- forecast_df_temp %>% 
  filter(horizon == as.numeric(max(forecast_df_temp$horizon))) 

start_date_GOF <- start_date_GOF$date[1]
                                         
forecast_df_temp <- forecast_df %>%  
  filter(date >= start_date_GOF)

forecast_df_temp <- forecast_df_temp %>%  rename(model = pred) %>% 
  pivot_longer(cols = c(model, naive_season,naive_lag),
                        values_to = "pred",
                        names_to = "fc_method")

GOF_temp <- forecast_df_temp %>%  setDT() %>% 
  .[, .(R2 = hydroGOF::R2(pred,obs),
        RMSE = hydroGOF::rmse(pred, obs),
        MAE = hydroGOF::mae(pred,obs)), by = .(horizon, fc_method)]


GOF <- GOF_temp %>% filter(fc_method == "model")


GOF_wide <- GOF_temp %>%
  select(-("R2")) %>% 
  pivot_wider(
    names_from = fc_method,
    values_from = c(RMSE, MAE)
  )

GOF_ratios <- GOF_wide %>%
  mutate(
    RMSE_scaled_season = RMSE_naive_season / RMSE_model,
    RMSE_scaled_lag    = RMSE_naive_lag / RMSE_model,
    MAE_scaled_season  = MAE_naive_season / MAE_model,
    MAE_scaled_lag     = MAE_naive_lag / MAE_model
  ) %>%
  select(horizon, starts_with("RMSE_scal"), starts_with("MAE_scal"))

GOF <- merge(GOF_ratios, GOF, by = "horizon")

GOF_long <- GOF %>% pivot_longer(cols = c(RMSE_scaled_season, RMSE_scaled_lag, MAE_scaled_season,
                                          MAE_scaled_lag,R2),
                         values_to = "values",
                         names_to = "GOF_metric") %>% 
  select(-c("RMSE","MAE"))

png(file=paste0("results/",today,"/",model_name,"/GOF/",model_name,"_GOF_vs_Horizons.png"),
    width = 1000, height = 600, units = "px")

GOF_long %>% 
  filter(!(GOF_metric %in% c("MAE_scaled_lag", "MAE_scaled_season"))) %>% 
  ggplot()+
  geom_line(aes(x=horizon, y=values, color = GOF_metric), linewidth = 1.5)+
  geom_hline(yintercept = 1)+
  theme_bw()+
  labs(title = "GOF metrics vs. forecasting horizons",
       subtitle = "Scaling was done by divding RMSE of the naive forecasts by the forecast of the model",
       x = "Forecasting Horizons",
       y = "GOF Values")+
  scale_x_continuous(breaks = seq(1,n_horizons,1))+
  scale_y_continuous(breaks = seq(0,5,0.5))

dev.off()
#### Summary Table ----------

summary_table <- GOF %>% 
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
    horizon = md("Forecasting Horizon<br>(months)"),
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


