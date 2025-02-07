library(ggplot2)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(tibble)


rm(list=ls())

### name the models and dataset of interest ------

model_name <- c("EN_KFCV-1")
dataset <- "lagged_TB" #TB = Tauchenbach


load(paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_fits_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))


coefs_df <-  do.call(rbind, lapply(names(coefs_list), function(alpha) {
      do.call(rbind, lapply(names(coefs_list[[alpha]]), function(lambda) {
        data.frame(
          alpha = alpha,
          lambda = lambda,
          value = coefs_list[[alpha]][[lambda]]
        )
      }))
    })) %>% 
        as.data.frame()

predictions_df <-  do.call(rbind, lapply(names(predictions_list), function(alpha) {
  do.call(rbind, lapply(names(predictions_list[[alpha]]), function(lambda) {
    data.frame(
      alpha = alpha,
      lambda = lambda,
      value = predictions_list[[alpha]][[lambda]]
    )
  }))
})) %>% 
  as.data.frame()
colnames(predictions_df) <- c("alpha","lambda","qsim","qobs","Date")

GOF_df <-  do.call(rbind, lapply(names(predictions_list), function(alpha) {
  do.call(rbind, lapply(names(predictions_list[[alpha]]), function(lambda) {
    data.frame(
      alpha = alpha,
      lambda = lambda,
      value = GOF_list[[alpha]][[lambda]]
    )
  }))
})) %>% 
  as.data.frame()



######### Store best fits, predictions, residuals and GOF for optimal GOFs###### --------



#give the optimal combination of lamdba and alpha based optimal GOF values
GOF_criteria <- c("R2","mNSE","MSE","MAE")

params_R2 <- GOF_df[which.max(GOF_df$value.R2),] 
params_mNSE <- GOF_df[which.max(GOF_df$value.mNSE),]
params_MSE <- GOF_df[which.min(GOF_df$value.MSE),]
params_MAE <- GOF_df[which.min(GOF_df$value.MAE),]


params <- rbind(params_R2,params_mNSE,params_MSE,params_MAE) %>% 
  t() %>% 
  as.data.frame() 
colnames(params) <- GOF_criteria

save(params,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_best_params_list.RData"))

best_pred_list <- list()
for (criteria in GOF_criteria) {
  
  temp <- params %>% 
    select(paste0(criteria))
  
  temp <- filter(predictions_df, alpha == temp["alpha",] 
                 & lambda == temp["lambda",])
  
  temp$GOF_criteria <- criteria
  temp$model <- model_name
  temp$dataset <- dataset
  
  best_pred_list[[paste0(criteria)]] <- temp
}

save(best_pred_list,file = paste0("results/",model_name,"/",model_name,"_",dataset,"_best_pred_list.RData"))

### Visualizing Cofficients --------------

coefficients_table_long <- do.call(rbind, coefs_list)
coefficients_table_long <- as.data.frame(do.call(cbind, coefs_list))

## Lineplot log

if (file.exists(paste0("results/",model_name,"/coefficients/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/coefficients/")))
}

for (a in 0:10) {
  alpha <- a/10
  
  selected_fit <- fits_list[[paste0("alpha_",alpha)]]
  betas <- as.matrix(selected_fit$glmnet.fit$beta)
  lambdas = selected_fit$lambda
  names(lambdas) = colnames(betas) 
  coefs <-  as.data.frame(betas) %>%
    tibble::rownames_to_column("variable") %>%
    pivot_longer(-variable) %>%
    mutate(lambda=lambdas[name])
  
  # coefs <- subset(coefs,coefs$value > 0.01) # in case only the important variables are plotted
  
  
  png(file=paste0("results/",model_name,"/coefficients/",model_name,"-alpha-",alpha,"_coefs_lineplot_log.png"),
      width = 1000, height = 600, units = "px")
  
plot <-  ggplot(coefs,aes(x=lambda,y=value,col=variable)) +
    geom_line(size = 2) +
    # scale_x_continuous(limits=c(0,200))+
    scale_y_continuous(limits=c(0,max(coefs$value)))+
    scale_x_log10()+
    theme_light(base_size = 18)+
    ggtitle(paste0("Coefficients vs. Lambdas in ",model_name, ": alpha = ",alpha))+
    xlab("Lambda")+
    ylab("Beta Value")
    
    print(plot)
  
  dev.off()
  
}



# ## Barplot 
# 
# coefs_df_long <- coefs_df %>%
#   pivot_longer(
#     cols = -c(alpha,lambda,value.alpha,value.lambda),       # Keep the 'alpha' column intact
#     names_to = "variables",  # Name for the new column that contains variable names
#     values_to = "values_of_variables"     # Name for the new column that contains the values
#   ) 
# 
# coefs_df_long %>% 
#   subset(.,coefs_df_long$lambda == "lambda10") #in case only the important variables are plotted
# 
#   ggplot(data= coefs_df_long, aes(fill=variables, y=values_of_variables, x=alpha)) + 
#   geom_bar(position="dodge", stat="identity")+
#   # scale_x_continuous(
#   # breaks = seq(-0.1, 1, by = 0.1)   # Restrict the range of the x-axis to 0 to 1
#   # ) 
# 
# 
# 
# 
# dev.off()



#### GOODNESS OF FIT (GOF) estimations and plots --------
# 
# GOF <- GOF %>% #removing surplus date columns
#   select(-contains(c( "fit.name")))

GOF_long <- GOF_df %>%
  pivot_longer(
    cols = -c(alpha, lambda, value.lambda, value.alpha,value.fit.name),       # Keep the 'alpha' column intact
    names_to = "GOF_criteria",  # Name for the new column that contains variable names
    values_to = "GOF_value"     # Name for the new column that contains the values
  )
####################    PLot GOF estimates for different lambdas    ##################    ##################
#answers: what is the optimal lambda?

png(file=paste0("results/",model_name,"/",model_name,"_GOF_by_lambda.png"),
    width = 1000, height = 600, units = "px")

subset(GOF_long,GOF_criteria %in% c("value.R2","value.mNSE","value.KGE")) %>% 
  ggplot(aes(y = GOF_value, x = lambda, 
                      group = GOF_criteria, 
                      color = GOF_criteria))+
  geom_line()+
  scale_x_discrete(labels = c("1","2","3","4","5","6","7","8","1se","min"))+
  facet_wrap(~alpha, scales = "free")

dev.off()

png(file=paste0("results/",model_name,"/",model_name,"_MSE_by_lambda.png"),
    width = 1000, height = 600, units = "px")

# GOF_long[which(GOF_long$GOF_criteria == c("value.MSE","value.MAE")),] %>%
#   # select(-contains(c( "MSE"))) %>%
  
subset(GOF_long,GOF_criteria %in% c("value.MSE","value.MAE")) %>% 
  ggplot(aes(y = GOF_value, x = lambda, 
             group = GOF_criteria, 
             color = GOF_criteria))+
  geom_line()+
  scale_x_discrete(labels = c("1","2","3","4","5","6","7","8","1se","min"))+
  facet_wrap(~alpha, scales = "free")

dev.off()
# 
# ggplot(GOF_long, aes(fill=GOF_criteria, y=GOF_value, x=alpha)) + 
#   geom_bar(position="stack", stat="identity")+
#   scale_x_continuous(
#     breaks = seq(-0.1, 1, by = 0.1)   # Restrict the range of the x-axis to 0 to 1
#   ) 

## create plot with the lambda.min and lambda.1se values inidcated by vlines
plot_data <- subset(GOF_long, GOF_criteria %in% c("value.MSE", "value.MAE"))

# Extract lambda values for vertical lines, per alpha group
vline_data_9 <- GOF_long %>%
  filter(lambda == "lambda9") %>%
  select(alpha, value.lambda) %>%
  distinct()  # Ensure unique lambda per alpha
vline_data_10 <- GOF_long %>%
  filter(lambda == "lambda9") %>%
  select(alpha, value.lambda) %>%
  distinct() 
# Create the plot


png(file=paste0("results/",model_name,"/",model_name,"_GOF_by_lambda_special.png"),
    width = 1000, height = 600, units = "px")

ggplot(plot_data, aes(y = GOF_value, x = log(value.lambda), 
                      group = GOF_criteria, 
                      color = GOF_criteria)) +
  geom_line() +
  facet_wrap(~alpha, scales = "free") +  # Ensure separate graphs for each alpha
  geom_vline(data = vline_data_9, aes(xintercept = log(value.lambda), linetype = "lambda.1se"), 
             color = "red") +
  geom_vline(data = vline_data_10, aes(xintercept = log(value.lambda), linetype = "lambda.min"), 
             color = "blue") +
  scale_linetype_manual(name = "Special Lambda", values = c("lambda.1se" = "solid", "lambda.min" = "dashed")) +
  theme(legend.position = "bottom")+
  ggtitle(paste0("Error values vs. lamdas in ",model_name))
dev.off()

#### RESIDUALS Analysis -----------
# for residuals analysis, first a certain alpha is selected
predictions_df$alpha <- as.factor(predictions_df$alpha)
predictions_df$residuals <- predictions_df$qsim - predictions_df$qobs


if (file.exists(paste0("results/",model_name,"/residuals/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/residuals/")))
}

#select alpha and lambdas to be analyzed:
alpha <- "alpha_1"
lambda <- "lambda5"

#########################################################################
predictions_df[which(predictions_df$alpha == alpha &
                       predictions_df$lambda == lambda),] %>%
  
    ggplot((aes(x=qsim,y=residuals)))+
    geom_point()+  
    geom_smooth()

predictions_df[which(predictions_df$alpha == alpha &
                       predictions_df$lambda == lambda),] %>%
  
  ggplot((aes(x=qobs,y=residuals)))+
  geom_point()+  
  geom_smooth()


png(file=paste0("results/",model_name,"/residuals/",model_name,"-",alpha,"-",lambda,"-sim_vs_obs.png"),
    width = 1000, height = 600, units = "px")

predictions_df[which(predictions_df$alpha == alpha &
                       predictions_df$lambda == lambda),] %>%
  
  ggplot((aes(x=Date,y=residuals)))+
  geom_point()+  
  geom_smooth()+
  geom_line(aes(x=Date,y=qobs), color = "green")+
  geom_line(aes(x=Date,y=qsim), color = "red")+
  theme_light()+
  ggtitle(label = paste0("Residuals over time for ", alpha," and ", lambda, " in ",model_name))
  
  dev.off()
#########################################################################
png(file=paste0("results/",model_name,"/residuals/",model_name,"-residuals_vs_qobs.png"),
      width = 1000, height = 600, units = "px")
  
ggplot(predictions_df, aes(x = qobs, y = residuals, color = alpha)) +
  geom_smooth(method = "loess", se = FALSE) + # Add smoothed lines without confidence intervals
  labs(
    title = paste0("Residuals vs. Observed Flow ", alpha," and ", lambda, " in ",model_name),
    x = "Observed Flow",
    y = "Residuals",
    color = "Alpha"
  ) +
  theme_light()+
  geom_hline(yintercept=0)
dev.off()

#########################################################################
png(file=paste0("results/",model_name,"/residuals/",model_name,"-residuals_vs_qsim.png"),
    width = 1000, height = 600, units = "px")  
  
  ggplot(predictions_df, aes(x = qsim, y = residuals, color = alpha)) +
    geom_smooth(method = "loess", se = FALSE) + # Add smoothed lines without confidence intervals
    labs(
      title = paste0("Residuals vs. Predicted Flow ", alpha," and ", lambda, " in ",model_name),
      x = "Simulated Flow",
      y = "Residuals",
      color = "Alpha"
    ) +
    theme_light()+
    geom_hline(yintercept=0)
  dev.off()
  
  #########################################################################
alpha <- "alpha_0.5"
 
  
png(file=paste0("results/",model_name,"/residuals/",model_name,"-residuals_vs_qobs-by_lambdas.png"),
      width = 1000, height = 600, units = "px")   

predictions_df[which(predictions_df$alpha == alpha),] %>%
  
ggplot(aes(x = qobs, y = residuals, color = lambda)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_point()+# Add smoothed lines without confidence intervals
  labs(
    title = paste0("Residuals for different lambdas and ", alpha, " in ",model_name),
    x = "Observed Flow",
    y = "Residuals",
    color = "lambda"
  ) +
  theme_light()+
  geom_hline(yintercept=0)

dev.off()

