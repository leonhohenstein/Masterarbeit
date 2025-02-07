library(ggplot2)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(tibble)


rm(list=ls())

### name the models and dataset of interest ------

model_name <- c("ENO-1")
dataset <- "lagged_TB" #TB = Tauchenbach


load(paste0("results/",model_name,"/",model_name,"_",dataset,"_coefs_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_predictions_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_fits_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_GOF_list.RData"))
load(paste0("results/",model_name,"/",model_name,"_",dataset,"_lambda_definition.RData"))

coefs_df <-  do.call(rbind, lapply(names(coefs_list), function(k) {
      do.call(rbind, lapply(names(coefs_list[[k]]), function(lambda) {
        data.frame(
          k = k,
          lambda = lambda,
          value = coefs_list[[k]][[lambda]]
        )
      }))
    })) %>% 
        as.data.frame()

predictions_df <-  do.call(rbind, lapply(names(predictions_list), function(k) {
  do.call(rbind, lapply(names(predictions_list[[k]]), function(lambda) {
    data.frame(
      k = k,
      lambda = lambda,
      value = predictions_list[[k]][[lambda]]
    )
  }))
})) %>% 
  as.data.frame()
colnames(predictions_df) <- c("k","lambda","qsim","qobs","Date")

GOF_df <-  do.call(rbind, lapply(names(predictions_list), function(k) {
  do.call(rbind, lapply(names(predictions_list[[k]]), function(lambda) {
    data.frame(
      k = k,
      lambda = lambda,
      value = GOF_list[[k]][[lambda]]
    )
  }))
})) %>% 
  as.data.frame()



######### Store best fits, predictions, residuals and GOF for optimal GOFs###### --------



#give the optimal combination of lamdba and k based optimal GOF values
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

fold_comparison <- list()

for (i in 1:10) {
  fold <- paste0("Fold-",i)
  temp <- GOF_df %>% 
    subset(.,GOF_df$k == fold)
  fold_comparison[[fold]] <- temp[which.max(temp$value.R2),]
  
}
fold_comparison_df <- do.call(rbind,fold_comparison)


best_pred_list <- list()
for (criteria in GOF_criteria) {
  
  temp <- params %>% 
    select(paste0(criteria))
  
  temp <- filter(predictions_df, k == temp["k",] 
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

for (k in 1:10) {

  selected_fit <- fits_list[[paste0("Fold-",k)]]
  betas <- as.matrix(selected_fit$glmnet.fit$beta)
  lambdas = selected_fit$lambda
  names(lambdas) = colnames(betas) 
  coefs <-  as.data.frame(betas) %>%
    tibble::rownames_to_column("variable") %>%
    pivot_longer(-variable) %>%
    mutate(lambda=lambdas[name])
  
  # coefs <- subset(coefs,coefs$value > 0.01) # in case only the important variables are plotted
  
  
  png(file=paste0("results/",model_name,"/coefficients/",model_name,"-fold-",k,"_coefs_lineplot_log.png"),
      width = 1000, height = 600, units = "px")
  
plot <-  ggplot(coefs,aes(x=lambda,y=value,col=variable)) +
    geom_line(size = 2) +
    # scale_x_continuous(limits=c(0,200))+
    scale_y_continuous(limits=c(0,max(coefs$value)))+
    scale_x_log10()+
    theme_light(base_size = 18)+
    ggtitle(paste0("Coefficients vs. Lambdas for different CV-Folds in ",model_name, ": k = ",k))+
    xlab("Lambda")+
    ylab("Beta Value")
    
    print(plot)
  
  dev.off()
  
png(file=paste0("results/",model_name,"/coefficients/",model_name,"-fold-",k,"_coefs_classic_view.png"),
      width = 1000, height = 600, units = "px")

plot(selected_fit, main = paste0("Coefficients of Elastic Net Regression of Fold = ",k))

dev.off() 
}


for (i in 1:nrow(coefs_df)) {
 
  coefs_df <- coefs_df %>%
    mutate(ncoefs_used = rowSums(select(., -c("k", "lambda", "value.lambda", "value.alpha"))!= 0))
 
    }



#### GOODNESS OF FIT (GOF) estimations and plots --------
# 
# GOF <- GOF %>% #removing surplus date columns
#   select(-contains(c( "fit.name")))
GOF_df$ncoefs_used <- coefs_df$ncoefs_used
GOF_df <- GOF_df %>% 
  mutate(lambda = factor(lambda, levels = c("lambda_1",   "lambda_2",   "lambda_3",   
                                            "lambda_4",   "lambda_5",   "lambda_6",   
                                            "lambda_7", "lambda_8", "lambda.min", "lambda.1se")))

GOF_long <- GOF_df %>%
  pivot_longer(
    cols = -c(k, lambda, value.lambda, value.alpha,value.fit.name,ncoefs_used),       # Keep the 'k' column intact
    names_to = "GOF_criteria",  # Name for the new column that contains variable names
    values_to = "GOF_value"     # Name for the new column that contains the values
  )
GOF_long$lambda <- as.factor(GOF_long$lambda)
GOF_long$value.fit.name <- as.factor(GOF_long$value.fit.name)
GOF_long$value.alpha <- as.factor(GOF_long$value.alpha)



####################    PLot GOF estimates for different lambdas    ##################    ##################
#answers: what is the optimal lambda?

png(file=paste0("results/",model_name,"/",model_name,"_GOF_by_lambda.png"),
    width = 1000, height = 600, units = "px")

subset(GOF_long,GOF_criteria %in% c("value.R2","value.mNSE","value.KGE")) %>% 
  ggplot()+
  geom_col(aes(y = ncoefs_used, x = lambda), fill = "steelblue", alpha = 0.6) +
  geom_line(aes(y = GOF_value, x = lambda, 
                      group = GOF_criteria, 
                      color = GOF_criteria))+
  scale_x_discrete(labels = c("1","2","3","4","5","6","7","8","min","1se"))+
  facet_wrap(~k, scales = "free")

dev.off()

######################################    next plot


png(file=paste0("results/",model_name,"/",model_name,"_GOF_by_lambda_ncoefs.png"),
    width = 1000, height = 600, units = "px")
scale_factor <- max(GOF_long$ncoefs_used, na.rm = TRUE) / max(GOF_long$GOF_value, na.rm = TRUE)

ggplot(subset(GOF_long, GOF_criteria %in% c("value.R2", "value.mNSE", "value.KGE"))) +
  # Bar plot (Primary Y-axis)
  geom_col(data = subset(GOF_long, GOF_criteria %in% c("value.R2")), aes(y = ncoefs_used, x = lambda), 
           fill = "steelblue", alpha = 0.4)+
  # Line plot (Secondary Y-axis)
  geom_line(aes(y = GOF_value * scale_factor, x = lambda, 
                group = GOF_criteria, color = GOF_criteria), size = 1.2) +
  
  # Scale X-axis
  scale_x_discrete(labels = c("1","2","3","4","5","6","7","8","min","1se")) +
  
  # Scale Y-axis with Secondary Axis
  scale_y_continuous(
    name = "Number of Coefficients",  # Primary y-axis label
    sec.axis = sec_axis(~ . / scale_factor, name = "GOF estimates") # Secondary y-axis
  ) +
  
  # Facet by k
  facet_wrap(~k, scales = "free") +
  
  # Theme and legend adjustments
  theme_minimal() +
  theme(legend.position = "bottom")
dev.off()
########################## Plotting Error measurements

png(file=paste0("results/",model_name,"/",model_name,"_Errors_by_lambda.png"),
    width = 1000, height = 600, units = "px")

subset(GOF_long,GOF_criteria %in% c("value.MSE","value.MAE")) %>% 
  ggplot(aes(y = GOF_value, x = lambda, 
             group = GOF_criteria, 
             color = GOF_criteria))+
  geom_line()+
  scale_x_discrete(labels = c("1","2","3","4","5","6","7","8","1se","min"))+
  facet_wrap(~k, scales = "free")

dev.off()



png(file=paste0("results/",model_name,"/",model_name,"_Errors_by_lambda_ncoefs.png"),
    width = 1000, height = 600, units = "px")

scale_factor <- GOF_long %>%
  filter(GOF_criteria %in% c("value.MSE", "value.MAE")) %>%
  summarise(scale_factor = max(ncoefs_used, na.rm = TRUE) / max(GOF_value, na.rm = TRUE)) %>%
  pull(scale_factor)

ggplot(subset(GOF_long, GOF_criteria %in% c("value.MSE","value.MAE"))) +
  # Bar plot (Primary Y-axis)
  geom_col(data = subset(GOF_long, GOF_criteria %in% c("value.R2")), aes(y = ncoefs_used, x = lambda), 
           fill = "steelblue", alpha = 0.4)+
  # Line plot (Secondary Y-axis)
  geom_line(aes(y = GOF_value * scale_factor, x = lambda, 
                group = GOF_criteria, color = GOF_criteria), size = 1.2) +
  scale_x_discrete(labels = c("1","2","3","4","5","6","7","8","min","1se")) +
  scale_y_continuous(
    name = "Number of Coefficients",  # Primary y-axis label
    sec.axis = sec_axis(~ . / scale_factor, name = "GOF estimates") # Secondary y-axis
  ) +
  facet_wrap(~k, scales = "free") +
  theme_minimal() +
  theme(legend.position = "bottom")

dev.off()


#### RESIDUALS Analysis -----------
# for residuals analysis, first a certain k is selected
predictions_df$k <- as.factor(predictions_df$k)
predictions_df$residuals <- predictions_df$qsim - predictions_df$qobs


if (file.exists(paste0("results/",model_name,"/residuals/"))){
} else {
  dir.create(file.path(paste0("results/",model_name,"/residuals/")))
}

#select k and lambdas to be analyzed:

k <- "Fold-2"
lambda <- "lambda.min"

######################################################################### next plot 
predictions_df[which(predictions_df$k == k &
                       predictions_df$lambda == lambda),] %>%
  
    ggplot((aes(x=qsim,y=residuals)))+
    geom_point()+  
    geom_smooth()

predictions_df[which(predictions_df$k == k &
                       predictions_df$lambda == lambda),] %>%
  
  ggplot((aes(x=qobs,y=residuals)))+
  geom_point()+  
  geom_smooth()


##### Hydrographs of predicted vs. observed flows 

for (nfold in 1:10) {
  for (lambda in lambda_names) {

    
#select k and lambdas to be analyzed:

k <- paste0("Fold-",nfold)  
# k <- "Fold-3"
# lambda <- "lambda.min"
png(file=paste0("results/",model_name,"/hydrographs/",model_name,"-Fold_",k,"-",lambda,"-sim_vs_obs.png"),
    width = 1000, height = 600, units = "px")

plot <- predictions_df[which(predictions_df$k == k &
                       predictions_df$lambda == lambda &
                 predictions_df$Date > "2013-01-01" ),] %>%
  
  ggplot((aes(x=Date,y=residuals)))+
  geom_point()+  
  geom_smooth()+
  geom_line(aes(x=Date,y=qobs), color = "green")+
  geom_line(aes(x=Date,y=qsim), color = "red")+
  theme_light()+
  ggtitle(label = paste0("Residuals over time for ", k," and ", lambda, " in model ",model_name))
  
print(plot)

  dev.off()
  
  }
}
######################################################################### next plot 
png(file=paste0("results/",model_name,"/residuals/",model_name,"-residuals_vs_qobs.png"),
      width = 1000, height = 600, units = "px")
  
ggplot(predictions_df, aes(x = qobs, y = residuals, color = k)) +
  geom_smooth(method = "loess", se = FALSE) + # Add smoothed lines without confidence intervals
  labs(
    title = paste0("Residuals (pred - obs) vs. Observed Flow ", k," and ", lambda, " in ",model_name),
    x = "Observed Flow",
    y = "Residuals",
    color = "k"
  ) +
  theme_light()+
  geom_hline(yintercept=0)
dev.off()

######################################################################### next plot
png(file=paste0("results/",model_name,"/residuals/",model_name,"-residuals_vs_qsim.png"),
    width = 1000, height = 600, units = "px")  
  
  ggplot(predictions_df, aes(x = qsim, y = residuals, color = k)) +
    geom_smooth(method = "loess", se = FALSE) + # Add smoothed lines without confidence intervals
    labs(
      title = paste0("Residuals (pred - obs) vs. Predicted Flow ", k," and ", lambda, " in ",model_name),
      x = "Predicted Flow",
      y = "Residuals",
      color = "k"
    ) +
    theme_light()+
    geom_hline(yintercept=0)
  dev.off()
  
  #########################################################################
k <- "Fold-6"
 
  
png(file=paste0("results/",model_name,"/residuals/",model_name,"-residuals_vs_qobs-by_lambdas_points.png"),
      width = 1000, height = 600, units = "px")   

predictions_df[which(predictions_df$k == k),] %>%
  
ggplot(aes(x = qobs, y = residuals, color = lambda)) +
  geom_smooth(method = "loess", se = FALSE) +
  # geom_point()+# Add smoothed lines without confidence intervals
  labs(
    title = paste0("Residuals (pred - obs) for different lambdas and ", k, " in ",model_name),
    x = "Observed Flow",
    y = "Residuals",
    color = "lambda"
  ) +
  theme_light()+
  geom_hline(yintercept=0)

dev.off()

