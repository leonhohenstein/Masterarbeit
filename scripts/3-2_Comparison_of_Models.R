library(tidyverse)
library(ggplot2)

rm(list=ls())

model_name <- c("EN_LOO-1","EN-2","ENO-1", "ENO-DT")
dataset <- "lagged_TB" #TB = Tauchenbach

#### Loading the optimal parameter of the models and store them as df -----
params_list <- list()
for (model in model_name) {
  for (set in dataset) {
    
    load(file = paste0("results/",model,"/",model,"_",set,"_best_params_list.RData"))
    
    params_list[[paste0(model,"_",set)]] <- params
    
  }
}

params_df <- do.call(rbind, lapply(names(params_list), function(model) {
  data.frame(
    model = model,
    value = params_list[[model]]
      )%>% 
    rownames_to_column(var = "info")
    }))

params_df_wide <- do.call(cbind, lapply(names(params_list), function(model) {
  data.frame(
    model = model,
    value = params_list[[model]]
  ) %>% 
    rownames_to_column(var = "info")
}))

params_df %>% 
  select(c("model","value.MAE","info")) %>% 
  filter(params_df$info  %in% c("value.R2","value.mNSE","value.MAE"))
  
GOFs <- c("R2","MAE")
GOF_summary_list <- list()

for (model in model_name) {
  for (set in dataset) {
    
    temp <- params_list[[paste0(model, "_", set)]] %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "info") %>%   
      select(c("info", GOFs))  
    
    temp <- temp[temp$info %in% c("value.lambda","value.alpha","value.MAE", "value.R2"), ] %>% 
      t() %>% 
      as.data.frame()  # Convert matrix back to data frame
    
    colnames(temp) <- c("value.alpha","value.lambda","MAE", "R2")  
    temp$model <- model
    
    # Convert row names into a column once, avoiding duplicate "info" column
    temp <- temp %>% rownames_to_column(var = "info")  
    temp <- temp[-which(temp$info == "info"), ]  # Remove redundant row if necessary
    
    # Fix the row naming to avoid overwriting `info`
    temp$info <- c(paste0(model, "-", GOFs))  # Assign correct row names
    
    GOF_summary_list[[paste0(model, set)]] <- temp
  }
}

GOF_summary_df <- do.call(rbind, GOF_summary_list)
GOF_long <- GOF_summary_df %>% 
  pivot_longer(c("MAE","R2"),names_to = "GOF_criteria", values_to = "GOF_value")
GOF_long$GOF_value<- as.numeric(GOF_long$GOF_value)


png(file=paste0("results/comparison_of_models/bar_plot_GOF_vs_models.png"),
    width = 1000, height = 600, units = "px")

GOF_long %>%
  filter(GOF_criteria %in% c("R2", "MAE")) %>%  # Keep only R2 and MAE
  ggplot(aes(x = info, y = GOF_value, fill = model)) +  
  geom_bar(stat = "identity", position = position_dodge()) +  # Bars side by side
  facet_wrap(~GOF_criteria, scales = "free") +  # Separate plots for R2 and MAE
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x labels for readability
  labs(x = "Model name and GOF_criterium to selection of best parameters (alpha and lambda)"
       , y = "GOF Value", fill = "Alpha Value", title = "Comparison of R2 and MAE")

dev.off()
#### Loading thGOf_value#### Loading the predictions of the models and store them as df -----

results_list <- list()

for (model in model_name) {
  for (set in dataset) {
    
    load(file = paste0("results/",model,"/",model,"_",set,"_best_pred_list.RData"))

    results_list[[paste0(model,"-",set)]] <- best_pred_list
  }
}


results_df <-  do.call(rbind, lapply(names(results_list), function(model) {
  do.call(rbind, lapply(names(results_list[[model]]), function(GOF_criteria) {
    data.frame(
      model = model,
      GOF_criteria = GOF_criteria,
      value = results_list[[model]][[GOF_criteria]]
    )
  }))
})) %>% 
  as.data.frame() #%>% 
  # rename_with(~ gsub("\\value.", "", .), everything())



################# PLOTTING ###################################

# results_df[which(predictions_df$alpha == alpha),] %>%
results_df <- results_df %>%
  pivot_longer(cols = c(value.qsim, value.qobs),  # Select columns to pivot
               names_to = "flow_type",  # New column indicating source (qsim or qobs)
               values_to = "flow") %>%  # New column containing values
  mutate(flow_type = ifelse(flow_type == "value.qsim", "Predicted Flow", "Observed Flow"))

results_df %>%
  # filter(value.Date > "2013-01-01" & value.GOF_criteria == "R2") %>% 
  filter(value.Date > "2013-01-01" &
           model == c("EN_LOO-1-lagged_TB", "EN-3-lagged_TB")) %>% 
  
  ggplot(aes(x = value.Date, y = flow )) +
  # geom_smooth(method = "loess", se = FALSE) +
  geom_line(aes(color = model, linetype = flow_type))+ 
  labs(title = "Smoothed Residuals vs Q_obs for Different lambdas",
    x = "Observed Flow",
    y = "Residuals",
    color = "model"
  ) +
  theme_light()+
  geom_hline(yintercept=0)

