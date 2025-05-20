library(ggplot2)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(tibble)
library(gt)
library(webshot2)
library(data.table)
library(RColorBrewer)

rm(list=ls())

### name the models and dataset of interest ------

model_name <- c("EN-TSRW")
dataset <- "lagged_TB" #TB = Tauchenbach


load(paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_model_fit.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_fc_horizon.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_best_tune.RData"))

GOF_df <- do.call(rbind,GOF_fc_list)

### Visualizing Cofficients --------------

if (file.exists(paste0("results/",model_name,"/coefficients/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/coefficients/")))
}


coefs_df <- do.call(rbind, lapply(names(coefs_list_fc), function(lambda) {
  data.frame(
    fc_horizon = lambda,
    coefficient = rownames(coefs_list_fc[[lambda]]),  # Extract row names (coefficient names)
    value = coefs_list_fc[[lambda]][, 1]  # Extract values from the first column
  )
})) %>% 
  as.data.frame()

for (h in fc_horizon) {
  coefs_df <- coefs_list_fc[[paste0("horizon")]]
  
  png(filename = paste0("results/",model_name,"/coefficients/",model_name,"_coefs_opt_lambda_h-",h,".png"),
    width = 1000, height = 600, units = "px")

plot <- coefs_df %>% 
  setDT() %>% 
  .[lambda == GOF_df$lambda[which.min(GOF_df$RMSE)]] %>% 
  .[value != 0] %>% 
  .[coefficient != "(Intercept)"] %>% 
  ggplot(aes(x = reorder(coefficient, value), y = value, fill = coefficient)) +
  geom_col(show.legend = FALSE) +  # Remove legend
  coord_flip() +  # Flip for better readability
  theme_minimal() +  # Clean theme
  labs(
    title = paste0("Coefficients of Predictors for Elastic-Net Model ",model_name),
    y = "Coefficients of Predictors ",
    x = "Predictors"
  ) +
  theme(
    axis.text.x = element_text(size = 10),  # Bring back x-axis labels
    axis.ticks.x = element_line(),  # Keep x-axis ticks
    legend.position = "none"  # Ensure legend is removed
  )
print(plot)
dev.off()
  
}


head(coefs_df)

for (h in 1:3) {
  model <- models_list[[paste0("horizon_",h)]]
  
betas <- as.matrix(model$beta)
lambdas = model$lambda
names(lambdas) = colnames(betas) 
coefs <-  as.data.frame(betas) %>%
  tibble::rownames_to_column("variable") %>%
  pivot_longer(-variable) %>%
  mutate(lambda=lambdas[name])

# coefs <- subset(coefs,coefs$value > 0.01) # in case only the important variables are plotted


png(file=paste0("results/",model_name,"/coefficients/",model_name,"_coefs_lineplot_h-",h,".png"),
    width = 1000, height = 600, units = "px")


line_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "solid", "dashed", "dotted", "dotdash",
                "longdash", "twodash", "solid", "dashed", "dotted", "dotdash")

# Create the plot with default color palette and different line types
p <- ggplot(coefs, aes(x = lambda, y = value, col = variable, linetype = variable)) +
  geom_line(size = 1.2) +  # Make lines slightly thicker for better visibility
  scale_y_continuous(limits = c(min(coefs$value), max(coefs$value))) +  # Adjust y-axis to include both negative and positive values
  scale_x_log10() +  # Log scale for x-axis
  scale_linetype_manual(values = line_types) +  # Manual line types
  theme_light(base_size = 18) +
  ggtitle(paste0("Coefficients of Predictors vs. Lambda  in ", model_name, " FC-Horizon = ",h)) +
  xlab("log10 (Lambda)") +
  ylab("Beta Value") +
  theme(
    legend.position = "right",  # Legend on the right side for clarity
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    axis.text = element_text(size = 14),  # Increase axis text size
    axis.title = element_text(size = 16)  # Increase axis title size
  )
print(p)
dev.off()


}



#### RESIDUALS Analysis -----------
# for residuals analysis, first a certain k is selected

pred_eval_df <- pred_eval_df %>%
  na.omit() %>%
  setDT() %>%
  .[, `:=`(
    res_t1 = pred_t1 - y_obs,
    res_t2 = pred_t2 - y_obs,
    res_t3 = pred_t3 - y_obs
  )]

if (file.exists(paste0("results/",model_name,"/residuals/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/residuals/")))
}


######################################################################### next plot 

pred_eval_df_long <- pred_eval_df %>%
  na.omit() %>%
  setDT() %>%
  .[, `:=`(
    res_t1 = pred_t1 - y_obs,
    res_t2 = pred_t2 - y_obs,
    res_t3 = pred_t3 - y_obs
  )] %>%
  melt(id.vars = c("Date", "y_obs"), 
       measure.vars = patterns("^pred_t", "^res_t"), 
       variable.name = "t", 
       value.name = c("pred", "res"))

pred_eval_df_long %>% 
  ggplot(aes(x=y_obs, y= res, color = t))+
  geom_line()
  # geom_line(aes(y=y_obs, color = "black"))



pred_eval_df_long %>% 
  ggplot(aes(x=Date, y= pred, color = t))+
  geom_line(aes(alpha = 0.8))+
  geom_line(aes(y=y_obs, color = "black", linetype = "dotted"))+
  facet_wrap(~t)



##### Hydrographs of predicted vs. observed flows 

if (file.exists(paste0("results/",model_name,"/hydrographs/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/hydrographs/")))
}

png(file=paste0("results/",model_name,"/hydrographs/",model_name,"_hydrogaphs_facet.png"),
    width = 1000, height = 600, units = "px")


pred_eval_df_long %>% 
  ggplot(aes(x = Date, y = pred, color = factor(t))) +  # Ensure t is a factor
  geom_line(size = 1) +  # Set line size
  geom_line(aes(y = y_obs), color = "grey", linetype = "solid", size = 1) + # Grey observed line
  facet_grid(t ~ ., scales = "free_y") +  # Stack plots vertically
  labs(
    y = "Monthly Low Flow [m³/s]",
    color = "Forecasting Horizon"  # Rename legend title for t
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",   # Adjust legend placement if needed
    strip.text = element_text(face = "bold") # Make facet labels bold
  )+
  ggtitle(label = paste0("Hydrographs for different forecasting horizons for model ",model_name))
dev.off()


png(file=paste0("results/",model_name,"/hydrographs/",model_name,"_hydrographs_stacked.png"),
    width = 1000, height = 600, units = "px")


pred_eval_df_long %>% 
  ggplot(aes(x = Date, y = pred, color = factor(t))) +  # Ensure t is a factor
  geom_line(size = 1) +  # Set line size
  geom_line(aes(y = y_obs), color = "grey", linetype = "solid", size = 1) + # Grey observed line
  # facet_grid(t ~ ., scales = "free_y") +  # Stack plots vertically
  labs(
    y = "Monthly Low Flow [m³/s]",
    color = "Forecasting Horizon"  # Rename legend title for t
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",   # Adjust legend placement if needed
    strip.text = element_text(face = "bold") # Make facet labels bold
  )+
  ggtitle(label = paste0("Hydrographs for different forecasting horizons for model ",model_name))
dev.off()





#### Summary Table ----------

summary_table <- GOF_df[, .SD[which.min(RMSE)], by = fc_horizon, .SDcols = c("lambda.value", "n_coefs", "R2", "RMSE")]
summary_table$lambda.applied <- summary_table$lambda.value[1]
setcolorder(summary_table, c("fc_horizon", "R2", "RMSE", "lambda.value", "lambda.applied", "n_coefs"))

# Create a basic gt table
gt_table <- gt(summary_table) %>%
  tab_header(
    title = "Summary Statistics of Monthly Forecasting Performance",
    subtitle = paste0("Model: ", model_name)
  ) %>%
  fmt_number(
    columns = c(R2, RMSE),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(lambda.value, lambda.applied),
    decimals = 4
  ) %>%
  cols_label(
    fc_horizon = md("Forecasting Horizon<br>(months)"),
    R2 = md("R²"),
    RMSE = md("RMSE"),
    lambda.value = md("Optimal<br>Lambda"),
    lambda.applied = md("Applied<br>Lambda"),
    n_coefs = md("Optimal Number<br>of Coefficients")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_footnote(
    footnote = "For forcasting of all months, the paramters for lamdba and number of coefficients of the forecasting horizon = 1 and not the optimal values were applied.",
    locations = cells_title(groups = "title")
  )


gtsave(gt_table, paste0("results/",model_name,"/summary_table_forecasts.png"))
# Print the table