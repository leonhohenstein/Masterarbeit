

library(ggplot2)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(tibble)
library(gt)
library(webshot2)
library(data.table)

rm(list=ls())

### name the models and dataset of interest ------

model_name <- c("EN-TS-No_Flow")
dataset <- "lagged_TB" #TB = Tauchenbach


load(paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_df.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_fits_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_df.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_lambda_definition.RData"))

coefs_df <-  do.call(rbind, lapply(names(coefs_list), function(iteration) {
  do.call(rbind, lapply(names(coefs_list[[iteration]]), function(lambda) {
    data.frame(
      iteration = iteration,
      lambda = lambda,
      value = coefs_list[[iteration]][[lambda]]
    )
  }))
})) %>% 
  as.data.frame()
colnames(predictions_df) <- c("Date","y_obs",lambda_names)

#### save optimal params based on different GOF criteria ----

GOF_criteria <- c("R2","mNSE","RMSE","MAE")

params_R2 <- GOF_df[which.max(GOF_df$R2),] 
params_mNSE <- GOF_df[which.max(GOF_df$mNSE),]
params_RMSE <- GOF_df[which.min(GOF_df$RMSE),]
params_MAE <- GOF_df[which.min(GOF_df$MAE),]


params <- rbind(params_R2,params_mNSE,params_RMSE,params_MAE) %>% 
  t() %>% 
  as.data.frame() 
colnames(params) <- GOF_criteria

save(params,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_best_params_list.RData"))


### Visualizing Cofficients --------------

coefficients_table_long <- do.call(rbind, coefs_list)
coefficients_table_long <- as.data.frame(do.call(cbind, coefs_list))

## Lineplot log

if (file.exists(paste0("results/",model_name,"/coefficients/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/coefficients/")))
}

for (i in seq(1,nrow(predictions_df),by=20)) {
  
  selected_fit <- fits_list[[paste0("iteration-",i)]]
  betas <- as.matrix(selected_fit$glmnet.fit$beta)
  lambdas = selected_fit$lambda
  names(lambdas) = colnames(betas) 
  coefs <-  as.data.frame(betas) %>%
    tibble::rownames_to_column("variable") %>%
    pivot_longer(-variable) %>%
    mutate(lambda=lambdas[name])
  
  # coefs <- subset(coefs,coefs$value > 0.01) # in case only the important variables are plotted
  
  
  png(file=paste0("results/",model_name,"/coefficients/",model_name,"-iteration-",i,"_coefs_lineplot_log.png"),
      width = 1000, height = 600, units = "px")
  
  plot <-  ggplot(coefs,aes(x=lambda,y=value,col=variable)) +
    geom_line(size = 2) +
    # scale_x_continuous(limits=c(0,200))+
    scale_y_continuous(limits=c(0,max(coefs$value)))+
    scale_x_log10()+
    theme_light(base_size = 18)+
    ggtitle(paste0("Coefficients vs. Lambdas for different CV-iterations in ",model_name, ": iteration = ",i))+
    xlab("Lambda")+
    ylab("Beta Value")
  
  print(plot)
  
  dev.off()
  
  png(file=paste0("results/",model_name,"/coefficients/",model_name,"-iteration-",i,"_coefs_classic_view.png"),
      width = 1000, height = 600, units = "px")
  
  plot(selected_fit, main = paste0("Coefficients of Elastic Net Regression of iteration = ",i))
  
  dev.off() 
}

best_coefs_df <- coefs_list[["iteration-148"]]$lambda_4 %>% 
  t()
 
library(tidyr)
library(dplyr)
library(ggplot2)

# Assuming best_coefs is the data frame you've shared:
# Gather the coefficient columns into a long format
best_coefs_long <- best_coefs %>%
  select(-(c(alpha,paste0("(Intercept)"),year,.id,lambda))) %>%  # Remove non-coefficient columns (adjust as needed)
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot as a bar chart with variable names
ggplot(best_coefs_long, aes(x = reorder(variable, value), y = value)) +
  geom_col(fill = "steelblue") +  # Bar chart with color
  theme_minimal() +
  xlab("Variable") +
  ylab("Coefficient Value") +
  ggtitle("Best Coefficients from Iteration-148") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

#### GOODNESS OF FIT (GOF) estimations and plots --------


for (i in 1:nrow(coefs_df)) {
  
  coefs_df <- coefs_df %>%
    mutate(ncoefs_used = rowSums(select(., -c("iteration", "lambda"))!= 0))
  
}


GOF_df$ncoefs_mean <- coefs_df %>% 
  as.data.table() %>%
  .[,.(mean_value = mean(ncoefs_used)), by = lambda] %>% 
  .[,mean_value]

GOF_df$ncoefs_sd <- coefs_df %>% 
  as.data.table() %>%
  .[,.(sd = sd(ncoefs_used)), by = lambda]%>% 
  .[,sd]


GOF_df <- GOF_df %>% 
  mutate(lambda = factor(lambda, levels = c("lambda_1",   "lambda_2",   "lambda_3",   
                                            "lambda_4",   "lambda_5",   "lambda_6",   
                                            "lambda_7", "lambda_8", "lambda.min", "lambda.1se")))

GOF_long <- GOF_df %>%
  pivot_longer(
    cols = -c(lambda, ncoefs_mean,ncoefs_sd),     
    names_to = "GOF_criteria",  # Name for the new column that contains variable names
    values_to = "GOF_value"     # Name for the new column that contains the values
  )
GOF_long$lambda <- as.factor(GOF_long$lambda)



####################    PLot GOF estimates for different lambdas    ##################    ##################
#answers: what is the optimal lambda?

png(file=paste0("results/",model_name,"/",model_name,"_GOF_by_lambda_ncoefs.png"),
    width = 1000, height = 600, units = "px")
scale_factor <- max(GOF_long$ncoefs_mean, na.rm = TRUE) / max(GOF_long$GOF_value, na.rm = TRUE)

ggplot(subset(GOF_long, GOF_criteria %in% c("R2", "mNSE", "KGE"))) +
  # Bar plot (Primary Y-axis)
  geom_col(data = subset(GOF_long, GOF_criteria %in% c("R2")), aes(y = ncoefs_mean, x = lambda), 
           fill = "steelblue", alpha = 0.4)+
  # Line plot (Secondary Y-axis)
  geom_line(aes(y = GOF_value * scale_factor, x = lambda, 
                group = GOF_criteria, color = GOF_criteria), size = 1.2) +
  ggtitle(label = paste0("GOF by lambda for model ",model_name))+
  
  
  # Scale X-axis
  scale_x_discrete(labels = c("1","2","3","4","5","6","7","8","min","1se")) +
  
  # Scale Y-axis with Secondary Axis
  scale_y_continuous(
    name = "Number of Coefficients",  # Primary y-axis label
    sec.axis = sec_axis(~ . / scale_factor, name = "GOF estimates") # Secondary y-axis
  ) +
    # Theme and legend adjustments
  theme_minimal() +
  theme(legend.position = "bottom")
dev.off()
########################## Plotting Error measurements

png(file=paste0("results/",model_name,"/",model_name,"_Errors_by_lambda.png"),
    width = 1000, height = 600, units = "px")

subset(GOF_long,GOF_criteria %in% c("MSE","MAE")) %>% 
  ggplot(aes(y = GOF_value, x = lambda, 
             group = GOF_criteria, 
             color = GOF_criteria))+
  geom_line()+
  ggtitle(label = paste0("Errors by lambda for model ",model_name))+
  scale_x_discrete(labels = c("1","2","3","4","5","6","7","8","1se","min"))

dev.off()



png(file=paste0("results/",model_name,"/",model_name,"_Errors_by_lambda_ncoefs.png"),
    width = 1000, height = 600, units = "px")

scale_factor <- GOF_long %>%
  filter(GOF_criteria %in% c("MSE", "MAE")) %>%
  summarise(scale_factor = max(ncoefs_mean, na.rm = TRUE) / max(GOF_value, na.rm = TRUE)) %>%
  pull(scale_factor)

ggplot(subset(GOF_long, GOF_criteria %in% c("MSE","MAE"))) +
  # Bar plot (Primary Y-axis)
  geom_col(data = subset(GOF_long, GOF_criteria %in% c("R2")), aes(y = ncoefs_mean, x = lambda), 
           fill = "steelblue", alpha = 0.4)+
  # Line plot (Secondary Y-axis)
  geom_line(aes(y = GOF_value * scale_factor, x = lambda, 
                group = GOF_criteria, color = GOF_criteria), size = 1.2) +
  scale_x_discrete(labels = c("1","2","3","4","5","6","7","8","min","1se")) +
  scale_y_continuous(
    name = "Number of Coefficients",  # Primary y-axis label
    sec.axis = sec_axis(~ . / scale_factor, name = "GOF estimates") # Secondary y-axis
  ) +
  ggtitle(label = paste0("Errors by lambda for model ",model_name))+
  theme_minimal() +
  theme(legend.position = "bottom")

dev.off()


#### RESIDUALS Analysis -----------
# for residuals analysis, first a certain k is selected
predictions_df$Date <- as.Date(predictions_df$Date , origin = "1970-01-01", tz = "UTC")

lambda = "lambda_4"
predictions_df <- as.data.table(predictions_df)

dt <- as.data.table(predictions_df)
# Compute residuals for each lambda column
residuals_dt <- dt[, lapply(.SD, function(x) y_obs - x), .SDcols = patterns("^lambda")]

# Add the Date column back to the residuals table
residuals_dt[, Date := dt$Date]
residuals_dt[, y_obs := predictions_df$y_obs]

# Reorder columns to have Date first
setcolorder(residuals_dt, "Date")

if (file.exists(paste0("results/",model_name,"/residuals/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/residuals/")))
}


png(file=paste0("results/",model_name,"/residuals/",model_name,"_residuals_vs_obs.png"),
    width = 1000, height = 600, units = "px")


residuals_dt %>%
  ggplot(aes(x = y_obs, y = lambda_5)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +  # Customizing points
  geom_smooth(method = "loess", color = "red", size = 1, linetype = "solid") +  # LOESS smoothing
  ggtitle(paste0("Residuals vs. observed values for model ",model_name)) +  # Title with lambda info
  labs(x = "Observed Flow [m^3/s]", y = "Residuals [m^3]", 
       subtitle = "LOESS smoothing applied") +  # Axis labels & smoothing method
  theme_minimal(base_size = 15) +  # Clean theme and font size
  theme(legend.position = "none")  # Remove legend
dev.off()



png(file=paste0("results/",model_name,"/residuals/",model_name,"_residuals_vs_time.png"),
    width = 1000, height = 600, units = "px")

residuals_dt %>%
  ggplot(aes(x = Date, y = lambda_5)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +  # Customizing points
  geom_smooth(method = "loess", color = "red", size = 1, linetype = "solid") +  # LOESS smoothing
  ggtitle(paste0("Residuals over time for model",model_name)) +  # Title with lambda info
  labs(x = "Date", y = "Predicted Values [m^3]", 
       subtitle = "LOESS smoothing applied") +  # Axis labels & smoothing method
  theme_minimal(base_size = 15) +  # Clean theme and font size
  theme(legend.position = "none")
dev.off()


##### Hydrographs of predicted vs. observed flows 

if (file.exists(paste0("results/",model_name,"/hydrographs/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/hydrographs/")))
}

predictions_df_long <- predictions_df %>% 
  pivot_longer(cols = lambda_names, values_to = "q_pred", names_to = "lambda")

for (y in lambda_names) {

  
png(file=paste0("results/",model_name,"/hydrographs/",model_name,"_pred_vs_obs_,",y,".png"),
      width = 1000, height = 600, units = "px")
  
plot <-  predictions_df_long %>% 
    filter(lambda==y) %>% 
  
  ggplot(aes(x = Date)) +
  geom_line(aes(x=Date,y=y_obs), color = "green")+
  geom_line(aes(x=Date,y=q_pred), color = "red")+ 
  ggtitle(paste0("Predicted vs. Observed Monthly Min Flow in model ",model_name,"and lambda = ",y)) +  # Title with lambda info
  labs(x = "Date", y = "Monthly Minimum Flow [m^3]", 
       subtitle = "") +  # Axis labels & smoothing method
  theme_minimal(base_size = 15) +  # Clean theme and font size
  theme(legend.position = "none")
  print(plot)
  
  dev.off()
}



### Create a table with the summary statistics of the 3 best lambdas -----
lambdas_table <- c("lambda_4","lambda_6","lambda.1se")  #define lambdas that should be analyzed more deeply

summary_table <- GOF_df[GOF_df$lambda %in% lambdas_table,] %>% 
  select( c("lambda","R2","RMSE","ncoefs_mean","ncoefs_sd"))


summary_table <- as.data.frame(summary_table)
summary_table$name.lambda <- lambdas_table


# Create a basic gt table
gt_table <- gt(summary_table) %>%
  tab_header(
    title = "Summary statistics of the 3 selected lambdas",
    subtitle = paste0("Model: ",model_name)
  ) %>%
  fmt_number(
    columns = c(R2,RMSE,ncoefs_mean,ncoefs_sd),
    decimals = 2)   %>%
  cols_label(
    lambda = "Name of Lambda",
    R2 = "R2",
    RMSE = "RMSE",
    ncoefs_mean = "Average Number of Coefficients",
    ncoefs_sd = "SD of Coefficients",
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_footnote(
    footnote = "SD = Standard Deviation between different fits of CV-Folds",
    locations = cells_title(groups = "title")
  )

gtsave(gt_table, paste0("results/",model_name,"/summary_table_lambdas.png"))
# Print the table
gt_table
















