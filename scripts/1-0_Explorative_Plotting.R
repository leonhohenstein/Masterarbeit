### Script for Explorative Statistics ####
library(ggplot2)
library(tidyverse)
library(rlang)
library(tidyquant)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(rlang)
library(TTR)#used to easily calculate running SD and runnign Variances
library(zoo)  # for rollmean

rm(list=ls())

# model_name <- c("EN_2")
dataset <- c("lagged_TB")
catchment <- "tauchenbach"


load(file = paste0("data/tauchenbach/models/",dataset,"_data.RData"))
load(file = paste0("data/tauchenbach/Final_df_Tauchenbach_weekly.RData"))


df <- df %>%
  arrange(Date) %>%
  mutate(flow_ma100 = zoo::rollmean(flow_min, k = 100, fill = NA, align = "center"))

# Plot
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = flow_min), color = "steelblue", size = 0.7, alpha = 0.6) +  # Raw flow
  geom_line(aes(y = flow_ma100), color = "darkred", size = 1) +            # Moving average
  geom_smooth(aes(y = flow_min), method = "lm", color = "darkgreen", se = FALSE, linewidth = 1) +  # Trend line
  labs(
    title = "Weekly Flow Time Series with Moving Average and Trend of Tauchenbach",
    x = "Date",
    y = "Minimum Flow [m³/s]",
    # caption = "Blue = raw flow, Red = 100-week MA, Green = linear trend"
  ) +
  theme_minimal(base_size = 14)+
  lims(y=c(0,3))+
  theme_bw()

df %>% select(c("flow_min","Date")) %>% na.omit() %>% 
  ggplot(aes(x=Date,y=flow_min))+
  geom_line()

df %>% 
  ggplot()+
  geom_line(aes(x=Date,y=flow_min))


attach(df)

### Explorative Statistics ###



# General Parameters for the plotting later on --------------
vars <-  c(                     #just to call all the available variables 
           "WB_1month.WB_abs",
           "WB_1month.WB_rel",
           "WB_2month.WB_abs",
           "WB_2month.WB_rel",
           "WB_3month.WB_abs",
           "WB_3month.WB_rel",
           "WB_6month.WB_abs",
           "WB_6month.WB_rel",
           "WB_12month.WB_abs",
           "WB_12month.WB_rel","Date",
           "month",
           "year",
           "T_min_mean_monthly",
           "T_max_mean_monthly",
           "snowcover_sum_monthly",
           "precipitation_sum_monthly",
           "sunshine_mean_monthly",
           "ETP_sum_monthly",
           "flow_min",
           "baseflow_min",
           "baseflow_max_monthly",
           "baseflow_mean_monthly")

# Checking for Seasonality -----------------------
ggplot(df, aes(x=month, y=paste0("flow_min"))) + 
  geom_boxplot()

plot_fun <- function(x) {
  boxplots <- ggplot(data = df, aes(x = month, y = paste0(x), group = month)) +
    geom_line(aes(color = month)) +
    geom_point(size = 0.5) +
    geom_text(aes(label = round(month)), size = 3) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title = x) +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  return(boxplots)
  }

variables <- c("WB_1month.WB_abs","WB_1month.WB_rel")

lapply(variables, plot_fun)

ggplot(df, aes(x=Date, y=flow_min)) + 
  geom_line()+
  geom_line(aes(x=Date,y=df$WB_1month.WB_abs))

names(df)

variables <- c("WB_1month.WB_abs",
               "WB_1month.WB_rel",
               "WB_2month.WB_abs"         
)

ggplot(df, aes(month,WB_6month.WB_abs)) +
  geom_line(color="gray") + 
  geom_point() +
  theme_bw()+
  geom_ma(ma_fun = SMA, n = 12, color = "red", linewidth = 1, linetype = 1)  # Plot x-month SMA
  
# 01_expl_BP_by_month (explorative Boxplots) plots for analysing seasonality trends by grouping by month ------
expl_BP_by_month_vars <- c(                 #just to call all the available variables 
  "WB_1month.WB_abs",
  "WB_2month.WB_abs",
  "WB_3month.WB_abs",
  "WB_6month.WB_abs",
  "WB_12month.WB_abs",
  "T_min_mean_monthly",
  "T_max_mean_monthly",
  "snowcover_sum_monthly",
  "precipitation_sum_monthly",
  "sunshine_mean_monthly",
  "ETP_sum_monthly",
  "flow_min",
  "BF_min_monthly",
  "BF_max_monthly",
  "BF_mean_monthly")

# expl_BP_by_month <- function(var) {
#   plot <- ggplot(df, aes(x = month, y = .data[[var]])) +  # Use dynamic mapping with .data
#     geom_boxplot(color = "gray") + 
#     geom_point() +
#     theme_bw() +
#     labs(title = var, y = var) 
#   # create folder to save the plots to
#   if (dir.exists("output")) { } 
#   else {dir.create("output")}
#   
#   # save plots to the 'output' folder
#   ggsave(filename = paste0("explorative_plots/01_expl_BP_by_month_",
#                            expl_BP_by_month_vars,
#                            ".png"),
#          plot = plot,
#          width = 11, height = 8.5, units = "cm")
#   
#   
#   }
# 
# expl_BP_by_month <- lapply(expl_BP_by_month_vars,
#                                     expl_BP_by_month)

# write a loop instead of a fucntion,
for (var in expl_BP_by_month_vars) {
  
  png(file=paste0("explorative_plots/tauchenbach/01_expl_BP_monthly_",catchment,"-",var,".png"),
      width = 1000, height = 600, units = "px")
  
plot <-  df %>% 
    select(var,month) %>% 
    
    ggplot(aes(x = month,y = !!sym(var))) +
      geom_boxplot(color="black") + 
      theme_bw(base_size = 16)+
      ggtitle(label = paste0(var))

  print(plot)
  dev.off()
  
}



# Ridgelines ---------------------------------------------------------------
### Plotting Ridgelines for seasonality in Low-Flow Distributions ###

ggplot(df, aes(x = flow_min, y = month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Monthly_Min_Flow", option = "m^3/s") +
  labs(title = 'Blablacar') +
  scale_x_continuous(limits = c(0,1.5))+
  theme_ipsum() +
  theme(
    legend.position="right",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
# Plotting Multiple densities ---------------
  ggplot(data=df, aes(x=flow_min, group=month, fill=month)) +
    geom_density(adjust=1.5, alpha=.4) +
    theme_ipsum()

# Histogram and Distributions ---------------

df %>% 
  ggplot(aes(x=flow_min))+
  geom_histogram( binwidth=0.1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 3") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))


#### Analyze for heteroscedasticity ---------
#write a function to control the alignment of the Moving Averages
# align = "right" corresponds to using a window based on offsets of -2, -1, 0, i.e. point before prior, prior and current point. The current point is the rightmost end of the window. Note that rollapplyr with an r on the end is the same as specifying align = "right"
# align = "center" corresponds to using a window based on offsets of -1, 0, 1, i.e. prior point current point and next point. The current point is the center of the window. This is the default for align arguments inside many functions.
# align = "left" corresponds to using a window based on offsets of 0, 1, 2 i.e. current point, next point and point after that. The current point is the leftmost point of the window.
#example: base_ma(vec, n = 3, align = "right")

base_ma <- function(x, n, align = c("right", "center")) {
  align <- match.arg(align)
  if (align == "right") {
    side <- 1 
  } else if (align == "center") {
    side <- 2
  }
  as.numeric(stats::filter(x, rep(1 / n, n), sides = side))
}
df$MA_50 <- base_ma(df$flow_min,50,align = "center")
df$MA_100 <- base_ma(df$flow_min,100,align = "center")
rownames_to_column(df,var="nrow")


png(file=paste0("explorative_plots/",catchment,"/trend_MA_stationarity_with_flow.png"),
    width = 1000, height = 600, units = "px")

df %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = flow_min, color = "Flow Min Monthly"), alpha = 0.5) +
  # geom_line(aes(y = MA_50, color = "MA 50"), linewidth = 1.5, alpha = 0.5) +
  geom_line(aes(y = MA_100, color = "MA 100"), linewidth = 1.2, alpha = 0.5) +
  stat_smooth(aes(y = flow_min, color = "Trend Line"),  
              method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  ggtitle(label = paste0("Monthly Min Flow moving averages and trend for ", catchment))+
  scale_color_manual(values = c("Flow Min Monthly" = "steelblue", 
                                # "MA 50" = "red", 
                                "MA 100" = "black", 
                                "Trend Line" = "forestgreen")) +
  theme_light()+
  labs(color = "Legend") 

dev.off()

  # geom_ma(ma_fun = SMA, n = 100, colour = "black", linetype = "solid", linewidth = 2, align == "center")

df$SD <- runSD(df$flow_min, n = 50)
df$Var <- runVar(df$flow_min, n = 50)

df %>% 
  ggplot(aes(x = Date)) +
  geom_line(aes(y = flow_min, color = "Flow Min Monthly"), alpha = 0.5) +
  # geom_line(aes(y = MA_50, color = "MA 50"), linewidth = 1.5, alpha = 0.5) +
  geom_line(aes(y = MA_100, color = "MA 100"), linewidth = 1.2, alpha = 0.5) +
  stat_smooth(aes(y = flow_min, color = "Trend Line"),  
              method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  ggtitle(label = paste0("Monthly Min Flow moving averages and trend for ", catchment))+
  scale_color_manual(values = c("Flow Min Monthly" = "steelblue", 
                                # "MA 50" = "red", 
                                "MA 100" = "black", 
                                "Trend Line" = "forestgreen")) +
  theme_light()+
  labs(color = "Legend") 

df %>% 
  ggplot(aes(Date))+
  geom_line(aes(y = flow_min, color = "Flow Min Monthly"), alpha = 0.5) +
  # geom_line(aes(y=Var,color = "Var"),alpha =0.5, linewidth = 1.5)+
  geom_line(aes(y=SD,color = "SD"),alpha =0.5, linewidth = 1.5)+
  ggtitle(label = paste0("Monthly Min Flow moving averages and trend for ", catchment))+
  scale_color_manual(values = c("Flow Min Monthly" = "steelblue", 
                                # "MA 50" = "red", 
                                "SD" = "black")) +
                                # ,"Var" = "forestgreen"
                                
  theme_light()+
  labs(color = "Legend") 
  

 
  
