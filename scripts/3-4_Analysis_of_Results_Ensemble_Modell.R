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


stations_list <- c(
  "tauchenbach",
  "kienstock",
  "flattach",
  "uttendorf")

for(station in stations_list){
  
### name the models and dataset of interest ------
today <- Sys.Date() 
# today <-
model_name <- c("Ensemble_Model")
dataset <- "lagged_TB" #TB = Tauchenbach
# station <- "kienstock"

load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_final_model_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"n_coefs_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_forecasts_list.RData"))
# load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))
load(paste0("results/",today,"/",model_name,"/",model_name,"_",dataset,"results.RData"))

load("data/flow_quantiles_list.RData")

quantiles <- flow_quantiles_list[[paste0(station)]]
# GOF_df <- do.call(rbind,GOF_list[[paste0(station)]])
coefs_list_final <- list()
forecast_df <- do.call(rbind,forecasts_list[[paste0(station)]])
cm_list <- list()



### Visualizing Cofficients --------------

n_horizons <- as.numeric(length(unique(names(forecasts_list))))

if (file.exists(paste0("results/",today,"/",model_name,"/coefficients/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/coefficients/")))
}
n_coefs <- do.call(rbind,n_coefs_list[[paste0(station)]])


#### COEFS VERTICAL BARS ---------

coefs_list <- lapply(coefs_list[[paste0(station)]],rownames_to_column)

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


png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_coefs_bars_all_fc-h",station,".png"),
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
    title = paste0("Importance of 20 Most Important Predictor Variables in ",model_name," | ",station),
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
    fill = "Forecasting Horizon",
    title = paste0("welcher plot???"," | ",station)
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


png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_coefs_lines_shadings_",station,".png"),
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
       title = paste0("Variable Coefficients vs. Forecasting Horizons"," | ",station),
       subtitle = paste0("Model: ", model_name, "from: ", Sys.Date()))+
  scale_x_continuous(breaks = seq(1,12,1))
dev.off()
#### Error BARS Version 2 ----

png(file=paste0("results/",today,"/",model_name,"/coefficients/",model_name,"_coefs_error_bars_",station,".png"),
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



#### QUANTILE BASED EVALUATION ----
# if (file.exists(paste0("results/",today,"/",model_name,"/quantile_evaluation/"))){
# } else {
#   dir.create(file.path(paste0("results/",today,"/",model_name,"/quantile_evaluation/")))
# }
# 
# quant_eval_95 <- do.call(rbind,forecasts_list) %>% 
#   setDT()
# quant_eval_95 <- quant_eval_95[,.(pred_Q95 = as.factor(pred < quantiles$Q95),
#                                   obs_Q95 = as.factor(obs < quantiles$Q95),
#                                   Date = Date,
#                                   horizon = horizon)]
# for (h in 1:n_horizons) {
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
#                         names_to = "characteristic", 
#                         values_to = "value")
# cm_long$characteristic <- as.factor(cm_long$characteristic)
# 
# png(file=paste0("results/",today,"/",model_name,"/quantile_evaluation/",model_name,"_confusion_bars_",station,".png"),
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
#     title = paste0("Comparison Across Horizons"," | ",station),
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
# 
# dev.off()
# 
# quantile_lines <- data.frame(
#   xintercept = c(
#     quantiles$Q94,
#     quantiles$Q95
#     ,quantiles$Q96
#   ),
#   Quantile = c(
#     "Q94",
#     "Q95" 
#     ,"Q96"
#   ))




##### Hydrographs of predicted vs. observed flows ---------


if (file.exists(paste0("results/",today,"/",model_name,"/hydrographs/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/hydrographs/")))
}

png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_hydrogaphs_facet_",station,".png"),
    width = 1000, height = 600, units = "px")


plot <- forecast_df %>% 
  filter(lubridate::year(date) == 2021) %>% 
  filter(ensemble == "ensemble_median") %>% 
  ggplot(aes(x = date, y = pred, color = factor(horizon))) +  # Forecast lines colored by horizon
  geom_line(size = 1.2) +  # Slightly thicker forecast lines
  geom_line(aes(y = obs), color = "grey", linetype = "solid", size = 1.2) +  # Darker grey for observed line
  facet_grid(horizon ~ ., scales = "free_y") +  # Facet by horizon, free y-axis
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Forecasting Horizon",  # Rename legend title
    title = paste0("Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",station),
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



#### Ensemble Hydrograph PLOt ----
ensemble_shade <- forecast_df %>% setDT() %>% .[!ensemble %in% c("ensemble_mean","ensemble_median"),
                                                .(pred_min = min(pred),
                                                  pred_max = max(pred),
                                                  ensemble_mean = mean(pred),
                                                  ensemble_median = median(pred),
                                                  obs = first(obs)), by = .(date,horizon) ]
for(h in c(1,3,6,9,12)){  
png(file=paste0("results/",today,"/",model_name,"/hydrographs/",model_name,"_ensemble_hydrograph",station,"_horizon_",h,".png"),
    width = 1000, height = 600, units = "px")

plot <- ensemble_shade %>% 
  filter(horizon == h) %>%
  ggplot() +
  geom_ribbon(aes(x = date, ymin = pred_min, ymax = pred_max, fill = "ensemble range"), alpha = 0.8) +
  geom_line(aes(x = date, y = ensemble_median, color = "ensemble_median"), linewidth = 1) +
  geom_line(aes(x = date, y = obs, color = "observed flow"), linewidth = 1) +
  geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted") +
  
  scale_color_manual(
    values = c(
      "observed flow" = "steelblue",
      "ensemble_median" = "lightcoral"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "ensemble range" = "grey70"
    )
  ) +
  
  labs(
    y = "Monthly Low Flow [m³/s]",
    x = "Date",
    color = "Legend",
    fill = "Legend",
    title = paste0("Ensemble Hydrographs for Different Forecasting Horizons\nModel: ", model_name," | ",station,"_horizon_",h),
    subtitle = paste0("Grey ribbon indicates ensemble range | Horizon = ",h)
  ) +
  
  theme_light(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85")
  )

print(plot)
dev.off()
}

}

# 
# # Choose desired horizons and order them descending
# horizons_to_plot <- c(12, 9, 5, 1)
# 
# # Filter and sort data accordingly
# ensemble_shade_filtered <- ensemble_shade %>%
#   filter(horizon %in% horizons_to_plot) %>%
#   mutate(horizon = factor(horizon, levels = horizons_to_plot)) %>%  # ensure ordering
#   arrange(desc(horizon))  # to plot high horizons first
# 
# # Base ggplot
# ggplot(ensemble_shade_filtered, aes(x = date)) +
#   
#   # Plot ribbons in descending order of horizon
#   geom_ribbon(aes(ymin = pred_min, ymax = pred_max, group = horizon),
#               fill = "gray20", alpha = 0.2) +  # lightest for horizon 12
#   
#   geom_ribbon(data = ensemble_shade_filtered %>% filter(horizon == 9),
#               aes(ymin = pred_min, ymax = pred_max),
#               fill = "9", alpha = 0.4) +
#   
#   geom_ribbon(data = ensemble_shade_filtered %>% filter(horizon == 5),
#               aes(ymin = pred_min, ymax = pred_max),
#               fill = "5", alpha = 0.8) +
#   
#   geom_ribbon(data = ensemble_shade_filtered %>% filter(horizon == 1),
#               aes(ymin = pred_min, ymax = pred_max),
#               fill = "1", alpha = 1) +
#   
#   # Median or mean forecast lines (use one or both)
#   geom_line(aes(y = ensemble_mean, color = factor(horizon)), linewidth = 1) +
#   
#   # Observed line
#   # geom_line(aes(y = obs), color = "black", size = 1.2, linetype = "solid") +
#   
#   # Custom styling
#   labs(
#     y = "Monthly Low Flow [m³/s]",
#     x = "Date",
#     color = "Forecast Horizon",
#     title = paste0("Ensemble Hydrographs for Forecasting Horizons\nModel: ", model_name),
#     subtitle = "Darker shades indicate shorter horizons"
#   ) +
#   scale_color_manual(values = c("12" = "darkred", "9" = "forestgreen", "5" = "yellow", "1" = "steelblue")) +
#   theme_light(base_size = 14) +
#   geom_hline(yintercept = quantiles$Q95, color = "black", linetype = "dotted") +
#   theme(
#     legend.position = "right",
#     legend.title = element_text(size = 14, face = "bold"),
#     legend.text = element_text(size = 12),
#     axis.text = element_text(size = 12, color = "black"),
#     axis.title = element_text(size = 14, face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     plot.subtitle = element_text(size = 12, hjust = 0.5),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_line(color = "grey85")
#   )
