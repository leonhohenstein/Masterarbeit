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
library(scales)  
rm(list=ls())

# today <- Sys.Date()-5
today <- as.Date("2025-11-14")
model_name <- c("sqrt_interaction_no_seas")
stations_list <- c("Tauchenbach","Kienstock","Uttendorf","Flattach")
fc_years_list <- list(2015:2016
                      ,2003:2004
                      ,2015:2021
)

load(file = (paste0("results/",today,"/",model_name,"/hist_fc_df_all.RData")))
load(file = (paste0("results/",today,"/",model_name,"/quantiles_all.RData")))
prec_df_all <- NULL
prec_seas_all <- NULL
lf_seas_all <- NULL
flow_obs_all <- NULL
for(s in unique(hist_fc_df_all$station)){
  # 
  # s <- "Uttendorf"
  
  load(file =  paste0("data/",s,"/Final_df_",s,"_weekly.RData"))
  quantile <- quantiles_all %>% filter(exceed_prob == 80 & station == s) %>% pull(quant_flow) %>% mean()
  lf_seas <- df %>% setDT() %>% 
    .[flow_mean < quantile, .(n_drought_weeks = .N), by = month ]
  df$year <- lubridate::year(df$Date)

  prec_seas <- df %>% setDT() %>% 
    .[, .(prec_monthly = sum(precipitation),
          swe_melted_monthly = sum(swe_melted),
          swe_added_monthly = sum(swe_added),
          rET_monthly = sum(rET),
          flow_mean = mean(flow_mean)), by = .(month,year) ]
  
  prec_seas <- prec_seas[, .(prec_monthly = mean(prec_monthly),
                             swe_melted_monthly = mean(swe_melted_monthly),
                             swe_added_monthly = mean(swe_added_monthly),
                             rET_monthly = mean(rET_monthly),
                             flow_mean = mean(flow_mean)), by = month]
  
  flow_obs <- df %>% select(flow_mean,Date) %>% rename(date = Date) %>% mutate(station = s)
  
  df <- df %>% rename("date" = "Date") %>% 
    select(date,precipitation,swe_added, swe_melted) %>% mutate(station = paste0(s))
  lf_seas$station <- s
  prec_seas$station <- s
  
  prec_df_all <- rbind(df,prec_df_all)
  lf_seas_all <- rbind(lf_seas,lf_seas_all)
  prec_seas_all <- rbind(prec_seas_all,prec_seas)
  flow_obs_all <- rbind(flow_obs_all,flow_obs)
  
  
}

colors_models <- thematic::okabe_ito(8)

color_legend <- data.frame(y = rep(1,8), x = 1:8, c = c("a","b","c","d","e","f","g","h"))
color_legend %>%
  ggplot()+
  geom_col(aes(x = x, y = y, fill = c))+
  scale_fill_manual(values = c("a" = colors_models[1],
                    "b" = colors_models[2],
                    "c" = colors_models[3],
                    "d" = colors_models[4],
                    "e" = colors_models[5],
                    "f" = colors_models[6],
                    "g" = colors_models[7],
                    "h" = colors_models[8]))

# Seasonality in Climate Variables Plot (Flow vs. Snow, ETP, Prec)----
# 
# prec_seas_all <- prec_seas_all %>% group_by(station) %>% na.omit() %>% 
#   mutate(MQ = mean(flow_mean), 
#          Q_max = max(flow_mean), 
#          P_max = max(prec_monthly))
# 
# prec_seas_all$P_max <- ceiling(prec_seas_all$P_max / 50) * 50
# prec_seas_all$x_min <- prec_seas_all$month-0.45
# prec_seas_all$x_max <- prec_seas_all$month+0.45
# P_max_all <- max(prec_seas_all$P_max)
# Q_max_all <- max(prec_seas_all$Q_max)
# scale_factor_all <- P_max_all/Q_max_all
# 
# prec_seas_all <- prec_seas_all %>% mutate(scaling_factor = Q_max / P_max)
# prec_seas_all <- prec_seas_all %>% mutate(y_max = P_max_all,
#                                           y_min = (P_max_all - prec_monthly),
#                                           flow_scaled = flow_mean / scaling_factor)

# 
# prec_seas_all %>% 
#   ggplot() +
#   geom_rect(aes(xmin = x_min, xmax = x_max, 
#                                   ymin = y_min, ymax = y_max, fill = "Precipitation"), 
#             alpha = 0.6, inherit.aes = FALSE, color = "black")+
#   geom_line(aes(x =month,y = (flow_mean),color = "Flow [m^3]"), linewidth = 1)+
#   geom_line(aes(x= month, y = rET_monthly, color = "reference Evapotranspiration [mm?]"), linewidth = 1)+
#   geom_line(aes(x= month, y = (swe_added_monthly+swe_melted_monthly), color = "Snow Balance [mm?]"), linewidth = 1)+
#   scale_fill_manual(
#     values = c("Precipitation" = "steelblue"),
#     name = "Legend")+
#   scale_color_manual(
#     values = c("Flow [m^3]" = "black",
#                "reference Evapotranspiration [mm?]" = "red2",
#                "Snow Balance [mm?]" = "forestgreen"),
#     name = "Legend")  +
#   scale_y_continuous(
#     name = "Discharge [m³/s]",
#     sec.axis = sec_axis(
#       ~ . * scale_factor_all,  # convert back to mm
#       name = "Precipitation [mm/month]",
#       breaks = seq(0, P_max_all, by = 50)
#     )
#   )+
#   # scale_y_continuous(
#   #   name = "Discharge [m³/s]",
#   #   sec.axis = sec_axis(
#   #     ~ (Q_max_all - .) / scale_factor_all,  # reverse transformation
#   #     # ~ P_max - ((. - (0.5*Q_max_all)) / scale_factor_all),
#   #     name = "Precipitation [mm/month]",
#   #     breaks = seq(0, P_max_all, by = 50) ))+
#   scale_x_continuous(breaks = seq(1,12,1))+
#   
#   facet_wrap(~station, scales = "free_y")+
#   theme_bw()
#   

# 
# 
# 
# prec_seas_all <- prec_seas_all %>%
#   group_by(station) %>%
#   mutate(
#     Q_max = max(flow_mean, na.rm = TRUE),
#     P_max = max(prec_monthly, na.rm = TRUE),
#     scaling_factor = Q_max / P_max,   # per station scaling factor
#     flow_scaled = flow_mean / scaling_factor
#   ) %>%
#   ungroup()
# 
# P_max_all <- max(prec_seas_all$P_max, na.rm = TRUE)
# Q_max_all <- max(prec_seas_all$Q_max, na.rm = TRUE)
# scale_factor_all <- P_max_all / Q_max_all
# 
# prec_seas_all %>% 
#   ggplot() +
#   geom_rect(aes(xmin = month - 0.45, xmax = month + 0.45,
#                 ymin = 0, ymax = prec_monthly,
#                 fill = "Precipitation"), alpha = 0.6, color = "black") +
#   geom_line(aes(x = month, y = flow_scaled, color = "Flow [m³/s]"), linewidth = 1) +
#   geom_line(aes(x = month, y = rET_monthly, color = "reference Evapotranspiration [mm]"), linewidth = 1) +
#   geom_line(aes(x = month, y = swe_added_monthly + swe_melted_monthly, color = "Snow Balance [mm]"), linewidth = 1) +
#   geom_line(aes(x = month, y = swe_added_monthly, color = "Snow Added [mm]"), linewidth = 1) +
#   geom_line(aes(x = month, y = swe_melted_monthly, color = "Snow Melt [mm]"), linewidth = 1) +
#   
#   scale_fill_manual(values = c("Precipitation" = "steelblue"), name = "Legend") +
#   scale_color_manual(values = c("Flow [m³/s]" = "black",
#                                 "reference Evapotranspiration [mm]" = "red2",
#                                 "Snow Balance [mm]" = "forestgreen",
#                                 "Snow Added [mm]" = "grey",
#                                 "Snow Melt [mm]" = "orange"),
# 
#                      name = "Legend") +
#   scale_y_continuous(
#     name = "Discharge [m³/s]",
#     sec.axis = sec_axis(~ . * scale_factor_all,
#                         name = "Precipitation [mm/month]",
#                         breaks = seq(0, P_max_all, by = 50))
#   ) +
#   scale_x_continuous(breaks = 1:12) +
#   facet_wrap(~station, scales = "free_y") +
#   theme_bw()
# 
# prec_seas_all %>% 
#   ggplot()+
#   geom_line(aes(x=month, y = prec_monthly,color = station))
# 
# prec_seas_all %>% 
#   ggplot()+
#   geom_line(aes(x=month, y = swe_added_monthly + swe_melted_monthly,color = station), linewidth = 1)+
#   theme_bw()+
#   scale_x_continuous(breaks = seq(1,12,1))

# Timing of Droughts vs. Snow etc. ----

# 
# snow <- prec_seas_all %>%
#   ggplot()+
#   geom_point(aes(y = (swe_added_monthly+swe_melted_monthly), x = month,  color = station))+
#   geom_smooth(aes(y = (swe_added_monthly+swe_melted_monthly), x = month,color = station), se = F,method = "loess",span = 0.3)+
#   labs(x = "Month", y = "Snow Balance (Added - Melted)")+
#   theme_bw()+
#   facet_wrap(~station,nrow = 1)+
#   scale_x_continuous(breaks = seq(1,12,1))
# 
# rain<- prec_seas_all %>%
#   ggplot()+
#   geom_col(aes(y = prec_monthly, x = month), fill = "steelblue",color = "black")+
#   
#   labs(x = "Month", y = "Precipitation [mm]")+
#   theme_bw()+
#   facet_wrap(~station,nrow = 1)+
#   scale_x_continuous(breaks = seq(1,12,1))
# 
# 
# droughts <- lf_seas_all %>%
#   ggplot() +
#   geom_col(aes(y = n_drought_weeks, x = month), fill = "orange",color = "black")+
#   facet_wrap(~station,nrow = 1)+
#   labs(x = "Month", y = "Weeks below Q80 / Month")+
#   theme_bw()+
#   scale_x_continuous(breaks = seq(1,12,1))
# 
# (snow +
#     theme(axis.title.x = element_blank(),
#           axis.text.x  = element_blank(),
#           axis.ticks.x = element_blank(),
#           plot.margin  = margin(0,5,0,5),
#           legend.text=element_text(size=9),
#           legend.position = "none")) /
#   
#   (rain +
#      theme(axis.title.x = element_blank(),
#            axis.text.x  = element_blank(),
#            axis.ticks.x = element_blank(),
#            plot.margin  = margin(0,5,0,5),
#            legend.text=element_text(size=6),
#            legend.position = "none")) /
#   
#   (droughts +
#      theme(plot.margin = margin(0,5,0,5),
#            legend.text=element_text(size=6)))

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
    
    Q80 <- unique(Q80)
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

#Calculate Drought Events for the entire Test Period (2015_2021) (Einordnung) ----

grey_events <-flow_obs_all %>% rename(flow = flow_mean) %>% 
  filter(lubridate::year(date) %in% 1980:2021) %>% 
  mutate(fc_period = "2015_2021", fc_method = "observed_flow", horizon = 0, pred_obs = "obs") %>% 
  .[, calculate_drought_objects(.SD,
                                             quantiles_all,
                                             ratio_pooling = 0.1,
                                             tmin_pooling = 14),
                 by = .(station),
                 .SDcols = c("date", "flow", "station", "fc_period","fc_method","horizon","pred_obs")]

grey_events <- grey_events %>% select(c("date",  "discharge" ,       
                                     "threshold",    "def.increase", 
                                     "event.no", "event.orig","station")) 

grey_events <- grey_events[event.no != 0, .(duration = .N,
                                            def.vol = sum(def.increase),
                                            qmin = min(discharge),
                                            # start_month = lubridate::month(first(date)),
                                            start_date = first(date)), by = .(event.no,station)]
grey_events <- grey_events %>% mutate(severity = (def.vol/duration))
# grey_events$seasonality <- 0.5*sin( ((grey_events$start_month-4)*pi/6)) +0.5

#transform absolute Qmin values of observed events into their relative magnitute compared to the stations Q80 flow
grey_events <- quantiles_all %>%  filter(exceed_prob == 80 & fc_period == "2015_2021") %>% select(-exceed_prob) %>% 
  left_join(grey_events,., by = c("station")) %>% rename(Q80 = quant_flow)

grey_events <- quantiles_all %>%  filter(exceed_prob == 50 & fc_period == "2015_2021") %>% select(-exceed_prob) %>% 
  left_join(grey_events,., by = c("station"))%>% rename(Q50 = quant_flow)

grey_events$min_flow_index <- 1-(grey_events$qmin/grey_events$Q80)


historic_events <- data.frame(station = c("Tauchenbach","Kienstock","Flattach","Uttendorf",
                                          "Tauchenbach","Kienstock","Flattach","Uttendorf"),
                              start_date = c(as.Date(c("2003-03-30","2003-10-26","2006-01-01","2008-10-19",
                                                       "2015-08-02","2015-08-30","2017-01-29","2016-02-21"))))
historic_events$historic_event <- c(rep("event_1",4),rep("event_2",4))
grey_events <- left_join(grey_events, historic_events, by = c("start_date","station"))
grey_events$historic_event[which(is.na(grey_events$historic_event)==T)] <- "grey_event"

percentiles <- grey_events %>% 
  group_by(station) %>% 
  summarize(severity = quantile(severity, probs = c(0.1)),
            min_flow_index = quantile(min_flow_index, probs = 0.1),
            duration = quantile(duration, probs = 0.1),
            qmin = quantile(qmin, probs = 0.1),
            def.vol = quantile(def.vol, probs = 0.1),
            start_date = as.Date("2025-11-18")) %>% 
  mutate(historic_event = "Q10")

percentiles <- grey_events %>% 
  group_by(station) %>% 
  summarize(severity = quantile(severity, probs = c(0.5)),
            min_flow_index = quantile(min_flow_index, probs = 0.5),
            duration = quantile(duration, probs = 0.5),
            qmin = quantile(qmin, probs = 0.5),
            def.vol = quantile(def.vol, probs = 0.5),
            start_date = as.Date("2025-11-19")) %>% 
  mutate(historic_event = "median") %>% rbind(.,percentiles)

percentiles <- grey_events %>% 
  group_by(station) %>% 
  summarize(severity = quantile(severity, probs = c(0.9)),
            min_flow_index = quantile(min_flow_index, probs = 0.9),
            duration = quantile(duration, probs = 0.9),
            qmin = quantile(qmin, probs = 0.9),
            def.vol = quantile(def.vol, probs = 0.9),
            start_date = as.Date("2025-11-20")) %>% 
  mutate(historic_event = "Q90") %>% rbind(.,percentiles)

percentiles <- quantiles_all %>%  filter(exceed_prob == 80 & fc_period == "2015_2021") %>% select(-exceed_prob) %>% 
  left_join(percentiles,., by = c("station")) %>% rename(Q80 = "quant_flow")
percentiles <- quantiles_all %>%  filter(exceed_prob == 50 & fc_period == "2015_2021") %>% select(-exceed_prob) %>% 
  left_join(percentiles,., by = c("station"))%>% rename(Q50 = "quant_flow")

grey_events <- rbind(grey_events,percentiles, fill = T) 

grey_events_global <- grey_events
grey_events_global$def.vol_index <- grey_events_global$def.vol/grey_events$Q50
grey_events_global$severity_index <- grey_events_global$def.vol_index/grey_events_global$duration
grey_events_global <- grey_events_global %>% mutate(min_flow_index = min_flow_index/max(min_flow_index),
                                             duration = duration/max(duration), 
                                             def.vol_index = def.vol_index/max(def.vol_index),
                                             severity_index = severity_index/max(severity_index))

grey_events_local <- grey_events
grey_events_local$def.vol_index <- grey_events_local$def.vol/grey_events_local$Q50
grey_events_local$severity_index <- grey_events_local$def.vol_index/grey_events_local$duration

grey_events_local <- grey_events_local %>% group_by(station) %>% 
  mutate(min_flow_index = (min_flow_index/max(min_flow_index)),
         duration = (duration/max(duration)), 
         def.vol_index = def.vol_index/max(def.vol_index),
         severity_index = severity_index/max(severity_index)) %>% 
  ungroup() %>% 
  select(station,min_flow_index,def.vol_index,severity_index,historic_event,duration,start_date)

#FINGERPRINT PLOT - EINORDNUNG ANEKDOTAL EVENTS----

axis_name_offset <-  0.2
central_distance = 0.05
fill_alpha = 0.1
linetype_grid = 1
alpha_map = 0.01

event_einordnung <- function(data_all,axis_name_offset,
                             central_distance,
                             fill_alpha,
                             linetype_grid,
                             alpha_map){
drawings_all <- NULL
  
  for (s in c("Tauchenbach","Kienstock","Uttendorf","Flattach")) {
    
  data <- data_all
  # data <- data %>% filter(station == "Tauchenbach")
  data <- data %>% filter(station == s 
                          # & duration > temp
                          )
  
  temp <- quantile(data$duration, probs = 0.2)

  data <- data %>% select(min_flow_index, start_date,def.vol_index, duration, severity_index,historic_event)
  # data <- data %>% select(min_flow_index, def.vol, duration, severity, start_date,historic_event)
  
  params <- setdiff(names(data), c("historic_event","start_date"))
  k <- length(params)
  
  data_scaled <- data %>%
    mutate(across(all_of(params), ~ scales::rescale(., to = c(0,1))))
  
   # angle lookup per parameter (fixed order)
  angles <- tibble(
    parameter = params,
    fi = seq(0, 2*pi, length.out = k + 1)[1:k] + pi/2
  )
  
  # long + coordinates
  plot_df <- data_scaled %>%
    pivot_longer(all_of(params), names_to = "parameter", values_to = "value") %>%
    left_join(angles, by = "parameter") %>%
    mutate(r = central_distance + value,
           x = r * cos(fi),
           y = r * sin(fi)) %>%
    arrange(start_date, fi) %>%
    group_by(start_date) %>%
    group_modify(~ bind_rows(.x, .x[1,])) %>%
    ungroup()
  
  text_data <- data %>%
    select(-c(start_date,historic_event)) %>%
    map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 0.25)) %>%
    mutate(r = seq(0, 1, 0.25)) %>%
    pivot_longer(-r, names_to = "parameter", values_to = "value") %>% 
    mutate(value = round(value, digits = 2))
  
  text_coords <- function(r, n_axis = ncol(data) - 2){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r = r - central_distance)
  }
  
  labels_data <- map_df(seq(0, 1, 0.25) + central_distance, text_coords) %>%
    bind_cols(text_data %>% select(-r))
  
  
  circle_coords <- function(r, n_axis = ncol(data) - 2){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r)
  }
  
  
  map <- map_df(seq(0, 1, 1) + central_distance, circle_coords) %>% #adjust the number of lines in the map by the 3rd seq argument
    ggplot(aes(x, y)) +
    geom_polygon(data = circle_coords(1 + central_distance), 
                 alpha = alpha_map, fill = "grey") +
    geom_path(aes(group = r), lty = linetype_grid, alpha = 0.3) +
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
  
  axis <- geom_line(data = axis_coords(ncol(data) - 2), 
                    aes(x, y, group = id), alpha = 0.3)
  map + axis
  
  drawings <- map + axis + 
    # geom_point(data = plot_df, aes(x, y, group = start_date),
    #            color = "grey50", size = 1.8, show.legend = FALSE, alpha = 0.3) +
    geom_path(data = plot_df, aes(x, y, group = start_date, color = historic_event,linewidth = historic_event),
               show.legend = FALSE) +
    # geom_polygon(data = plot_df, aes(x, y, group = start_date),
    #              fill = "grey70", alpha = 0.1, color = NA, show.legend = FALSE)+
    # geom_text(data = labels_data,
    #           aes(x, y, label = value), alpha = 0.65) +
    geom_text(data = text_coords(1 + central_distance + axis_name_offset),
              aes(x, y), label = labels_data$parameter[1:(ncol(data)-2)]) +
    labs(subtitle = paste0("Station ",s, " | n = ",nrow(data)))+
    
    scale_color_manual(
      values = c("grey_event" = colors_models[5],
                 "event_1" = colors_models[3],
                 "event_2" = colors_models[1]),
      name = "Legend")+
    scale_linewidth_manual(values = c("grey_event" = 0.3,
                                      "event_1" = 1,
                                      "event_2" = 1),
                           name = "Legend")

  drawings_all[[paste0(s)]] <- drawings
  
  }

  plot <- drawings_all[["Tauchenbach"]] / drawings_all[["Kienstock"]] - drawings_all[["Flattach"]]  / drawings_all[["Uttendorf"]] 
  print(plot)
}

# event_einordnung_local <- drawings_all[["Tauchenbach"]] / drawings_all[["Kienstock"]] - drawings_all[["Flattach"]]  / drawings_all[["Uttendorf"]] 
  
event_einordnung(data_all = grey_events_local,axis_name_offset,
                 central_distance,
                 fill_alpha,
                 linetype_grid,
                 alpha_map)

#FINGERPRINT PLOT - EINORDNUNG MEDIAN Q90/Q10----

axis_name_offset <-  0.2
central_distance = 0.05
fill_alpha = 0.1
linetype_grid = 1
alpha_map = 0.01

event_einordnung_percentiles <- function(data_all,axis_name_offset,
                             central_distance,
                             fill_alpha,
                             linetype_grid,
                             alpha_map){
  drawings_all <- NULL
  
  for (s in c("Tauchenbach","Kienstock","Uttendorf","Flattach")) {
    
    data <- data_all
    # data <- data %>% filter(station == "Tauchenbach")
    data <- data %>% filter(station == s 
                            & !historic_event %in% c("event_1","event_2","grey_event")
                            # & duration > temp
    )
    
    temp <- quantile(data$duration, probs = 0.2)
    
    data <- data <- data %>% 
      select(
        min_flow_index,
        def.vol_index,
        duration,
        severity_index,
        start_date,
        historic_event
      )
    # data <- data %>% select(min_flow_index, def.vol, duration, severity, start_date,historic_event)
    temp <- data.frame(min_flow_index = rep(1,4),
                       def.vol_index = rep(1,4),
                       severity_index = rep(1,4),
                       duration = rep(1,4),
                       start_date = as.Date("2025-06-07"),
                       historic_event = "border")
    data <- rbind(data,temp)
    
    params <- c("min_flow_index","def.vol_index","duration","severity_index")
    k <- length(params)
    
    data_scaled <- data %>%
      mutate(across(all_of(params), ~ scales::rescale(., to = c(0,1))))

    # angle lookup per parameter (fixed order)
    angles <- tibble(
      parameter = params,
      fi = seq(0, 2*pi, length.out = k + 1)[1:k] + pi/2
    )
    
    # long + coordinates
    plot_df <- data_scaled %>%
      pivot_longer(all_of(params), names_to = "parameter", values_to = "value") %>%
      left_join(angles, by = "parameter") %>%
      mutate(r = central_distance + value,
             x = r * cos(fi),
             y = r * sin(fi)) %>%
      arrange(start_date, fi) %>%
      group_by(start_date) %>%
      group_modify(~ bind_rows(.x, .x[1,])) %>%
      ungroup()
    
    text_data <- data %>%
      select(-c(start_date,historic_event)) %>%
      map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 0.25)) %>%
      mutate(r = seq(0, 1, 0.25)) %>%
      pivot_longer(-r, names_to = "parameter", values_to = "value") %>% 
      mutate(value = round(value, digits = 2))
    
    text_coords <- function(r, n_axis = ncol(data) - 2){
      fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
      x <- r*cos(fi)
      y <- r*sin(fi)
      
      tibble(x, y, r = r - central_distance)
    }
    
    labels_data <- map_df(seq(0, 1, 0.25) + central_distance, text_coords) %>%
      bind_cols(text_data %>% select(-r))
    
    
    circle_coords <- function(r, n_axis = ncol(data) - 2){
      fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
      x <- r*cos(fi)
      y <- r*sin(fi)
      
      tibble(x, y, r)
    }
    
    
    map <- map_df(seq(0, 1, 1) + central_distance, circle_coords) %>% #adjust the number of lines in the map by the 3rd seq argument
      ggplot(aes(x, y)) +
      geom_polygon(data = circle_coords(1 + central_distance), 
                   alpha = alpha_map, fill = "grey") +
      geom_path(aes(group = r), lty = linetype_grid, alpha = 0.3) +
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
    
    axis <- geom_line(data = axis_coords(ncol(data) - 2), 
                      aes(x, y, group = id), alpha = 0.3)
    map + axis
    
    drawings <- map + axis + 
      # geom_point(data = plot_df, aes(x, y, group = start_date),
      #            color = "grey50", size = 1.8, show.legend = FALSE, alpha = 0.3) +
      geom_path(data = plot_df, aes(x, y, group = start_date, color = historic_event,linewidth = historic_event),
                show.legend = FALSE) +
      # geom_polygon(data = plot_df, aes(x, y, group = start_date),
      #              fill = "grey70", alpha = 0.1, color = NA, show.legend = FALSE)+
      # geom_text(data = labels_data,
      #           aes(x, y, label = value), alpha = 0.65) +
      geom_text(data = text_coords(1 + central_distance + axis_name_offset),
                aes(x, y), label = labels_data$parameter[1:(ncol(data)-2)]) +
      labs(subtitle = paste0("Station ",s, " | n = ",nrow(data)))+

      scale_color_manual(
        values = c("grey_event" = colors_models[5],
                   "Q90" = colors_models[6],
                   "median" = colors_models[1],
                   "Q10" = colors_models[2]),
        name = "Legend")+
      scale_linewidth_manual(values = c("grey_event" = 0.3,
                                        "Q90" = 1,
                                        "Q10" = 1,
                                        "median" = 1),
                             name = "Legend")
    
    drawings_all[[paste0(s)]] <- drawings
    
  }
  
  plot <- drawings_all[["Tauchenbach"]] / drawings_all[["Kienstock"]] - drawings_all[["Flattach"]]  / drawings_all[["Uttendorf"]] 
  print(plot)
}



event_einordnung_percentiles(data_all = grey_events_global,axis_name_offset,
                 central_distance,
                 fill_alpha,
                 linetype_grid,
                 alpha_map)

test <- grey_events_local %>% filter(historic_event %in% c("Q90","Q10","median") &
                                        station == "Tauchenbach")

# FINGERPRINT - VERLGEICH OBS PRED EVENTS ----

axis_name_offset <-  0.2
central_distance = 0.05
fill_alpha = 0.1
linetype_grid = 1
alpha_map = 0.01

event_einordnung <- function(data_all,axis_name_offset,
                             central_distance,
                             fill_alpha,
                             linetype_grid,
                             alpha_map){
  drawings_all <- NULL
  
  for (s in c("Tauchenbach","Kienstock","Uttendorf","Flattach")) {
    
    data <- data_all
    # data <- data %>% filter(station == "Tauchenbach")
    data <- data %>% filter(station == s 
                            # & duration > temp
    )
    
    temp <- quantile(data$duration, probs = 0.2)
    
    data <- data %>% select(min_flow_index, start_date,def.vol_index, duration, severity_index,historic_event)
    # data <- data %>% select(min_flow_index, def.vol, duration, severity, start_date,historic_event)
    
    params <- setdiff(names(data), c("historic_event","start_date"))
    k <- length(params)
    
    data_scaled <- data %>%
      mutate(across(all_of(params), ~ scales::rescale(., to = c(0,1))))
    
    # angle lookup per parameter (fixed order)
    angles <- tibble(
      parameter = params,
      fi = seq(0, 2*pi, length.out = k + 1)[1:k] + pi/2
    )
    
    # long + coordinates
    plot_df <- data_scaled %>%
      pivot_longer(all_of(params), names_to = "parameter", values_to = "value") %>%
      left_join(angles, by = "parameter") %>%
      mutate(r = central_distance + value,
             x = r * cos(fi),
             y = r * sin(fi)) %>%
      arrange(start_date, fi) %>%
      group_by(start_date) %>%
      group_modify(~ bind_rows(.x, .x[1,])) %>%
      ungroup()
    
    text_data <- data %>%
      select(-c(start_date,historic_event)) %>%
      map_df(~ min(.) + (max(.) - min(.)) * seq(0, 1, 0.25)) %>%
      mutate(r = seq(0, 1, 0.25)) %>%
      pivot_longer(-r, names_to = "parameter", values_to = "value") %>% 
      mutate(value = round(value, digits = 2))
    
    text_coords <- function(r, n_axis = ncol(data) - 2){
      fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
      x <- r*cos(fi)
      y <- r*sin(fi)
      
      tibble(x, y, r = r - central_distance)
    }
    
    labels_data <- map_df(seq(0, 1, 0.25) + central_distance, text_coords) %>%
      bind_cols(text_data %>% select(-r))
    
    
    circle_coords <- function(r, n_axis = ncol(data) - 2){
      fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
      x <- r*cos(fi)
      y <- r*sin(fi)
      
      tibble(x, y, r)
    }
    
    
    map <- map_df(seq(0, 1, 1) + central_distance, circle_coords) %>% #adjust the number of lines in the map by the 3rd seq argument
      ggplot(aes(x, y)) +
      geom_polygon(data = circle_coords(1 + central_distance), 
                   alpha = alpha_map, fill = "grey") +
      geom_path(aes(group = r), lty = linetype_grid, alpha = 0.3) +
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
    
    axis <- geom_line(data = axis_coords(ncol(data) - 2), 
                      aes(x, y, group = id), alpha = 0.3)
    map + axis
    
    drawings <- map + axis + 
      # geom_point(data = plot_df, aes(x, y, group = start_date),
      #            color = "grey50", size = 1.8, show.legend = FALSE, alpha = 0.3) +
      geom_path(data = plot_df, aes(x, y, group = start_date, color = historic_event,linewidth = historic_event),
                show.legend = FALSE) +
      # geom_polygon(data = plot_df, aes(x, y, group = start_date),
      #              fill = "grey70", alpha = 0.1, color = NA, show.legend = FALSE)+
      # geom_text(data = labels_data,
      #           aes(x, y, label = value), alpha = 0.65) +
      geom_text(data = text_coords(1 + central_distance + axis_name_offset),
                aes(x, y), label = labels_data$parameter[1:(ncol(data)-2)]) +
      labs(subtitle = paste0("Station ",s, " | n = ",nrow(data)))+
      
      scale_color_manual(
        values = c("grey_event" = "grey",
                   "event_1" = "darkred",
                   "event_2" = "forestgreen"),
        name = "Legend")+
      scale_linewidth_manual(values = c("grey_event" = 0.3,
                                        "event_1" = 1,
                                        "event_2" = 1),
                             name = "Legend")
    
    drawings_all[[paste0(s)]] <- drawings
    
  }
  
  plot <- drawings_all[["Tauchenbach"]] / drawings_all[["Kienstock"]] - drawings_all[["Flattach"]]  / drawings_all[["Uttendorf"]] 
  print(plot)
}


# DER REST -----

# setDT(hist_fc_df_all)
# load(file = (paste0("results/",today,"/",model_name,"/forecast_df_complete.RData")))
# 
# forecast_df_complete_long <- forecast_df_complete %>% rename(catchment_memory = pred) %>% select(-fc_method) %>%
#   pivot_longer(cols = c(catchment_memory, naive_season,naive_lag,ensemble_median,upper_benchmark),
#                values_to = "flow",names_to = "fc_method") %>% mutate(pred_obs = "pred_obs")
# 
# forecast_df_complete_long <- forecast_df_complete_long %>% select(-c(fc_method,flow)) %>% distinct() %>% 
#   mutate(fc_method = "observed",flow = obs) %>% 
#   rbind(.,forecast_df_complete_long)
# 
# drought_objects <-
#   forecast_df_complete_long %>% setDT() %>% .[, calculate_drought_objects(.SD,
#                                              quantiles_all,
#                                              ratio_pooling = 0.1,
#                                              tmin_pooling = 14),
#                  by = .(station, fc_method, horizon, fc_period,pred_obs),
#                  .SDcols = c("date", "flow", "station", "fc_method", "horizon", "fc_period","pred_obs")]
# 
# data <- drought_objects %>% select(c("date",  "discharge" ,
#                                      "threshold",    "def.increase",
#                                      "event.no", "event.orig"))
# 
# data$fc_method <- drought_objects$fc_method
# data$fc_period <- drought_objects$fc_period
# data$station <- drought_objects$station
# data$horizon <- drought_objects$horizon
# data$pred_obs <- drought_objects$pred_obs
# 
# drought_objects <- data
# 
# save(drought_objects, file = (paste0("results/",today,"/",model_name,"/drought_object.RData")))
load(file = (paste0("results/",today,"/",model_name,"/drought_object.RData")))

# Confustion matrix/ Contingency Table - Precision / Recall ----

wide_confusion <- drought_objects %>%
  pivot_wider(
    id_cols     = all_of(c("date","station","fc_method","fc_period","horizon","threshold")),    
    names_from  = pred_obs,
    values_from =  event.no,
    names_prefix = "event_no_"
    # values_fn   = list(event.no = max),  # bei Duplikaten z.B. max nehmen
    # values_fill = 0                      # fehlende als 0 statt NA
  ) 

confusion_df <- wide_confusion %>%
  setDT() %>%
  .[, .(TN = sum(event_no_pred == 0 & event_no_obs == 0),
        TP = sum(event_no_pred > 0 & event_no_obs > 0),
        FN = sum(event_no_pred == 0 & event_no_obs > 0),
        FP = sum(event_no_pred > 0 & event_no_obs == 0)),
    by = .(station, fc_method, horizon, fc_period), ]

confusion_df <- confusion_df %>% mutate(precision = TP/(TP+FP),
                                        recall = TP/(TP+FN))
confusion_df[is.na(confusion_df)] <- 0

#Evaluating predicted Drought Event characteristics ----
test <- drought_objects %>% setDT() %>% .[, .(start = min(date),
                                              end = last(date)), by = fc_method]
test <- data.frame(sapply(c("catchment_memory", "naive_season", "naive_lag", "ensemble_median","upper_benchmark"),
       function(x){
  obs <- drought_objects %>% filter(fc_method == "observed") %>% mutate(pred_obs = "obs")
  pred <- drought_objects %>% filter(fc_method == x) %>% mutate(pred_obs = "pred")
  comb <- rbind(obs,pred)
  comb <- comb %>%
    setDT() %>%
    .[, `:=`(
      start    = fifelse(event.no > 0, first(date), as.Date(NA)),
      end      = fifelse(event.no > 0, last(date),  as.Date(NA)),
      duration = fifelse(event.no > 0, as.integer(last(date) - first(date) + 1), 0),
      date     = date,
      def.vol      = fifelse(event.no > 0, sum(def.increase),  NA),
      qmin      = fifelse(event.no > 0, min(discharge),  NA)),
      by = .(station, fc_method, horizon, fc_period,event.no,pred_obs)]

  wide <- comb %>%
    pivot_wider(
      id_cols     = all_of(c("date","station","fc_method","fc_period","horizon","threshold")),    
      names_from  = pred_obs,
      values_from =  c(event.no, discharge, def.increase, event.orig,start,end,duration,qmin,def.increase,def.vol),
      names_prefix = "") 
  return(wide)
}))



drought_objects_pred_obs <- drought_objects


data <- drought_objects %>%
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

wide <- wide %>% setDT() %>% 
  .[, `:=` (start_pred = na.locf(start_pred, fromLast = TRUE, na.rm = F),
            end_pred = na.locf(end_pred, fromLast = TRUE, na.rm = F))
    ,by = .(station, fc_method, horizon, fc_period), ]

wide <- wide %>% mutate(duration_pred_test = end_pred - start_pred + 1)


wide <- wide %>% setDT() %>% 
  .[, `:=` (qmin_pred = min(discharge_pred),
            def.vol_pred = ((threshold-discharge_pred)*60*60*24))
    ,by = .(station, fc_method, horizon, fc_period,event.no_obs), ]

plot_data_wide <- wide

wide <- wide %>% filter(event.no_obs != 0)

wide <- quantiles_all %>% filter(exceed_prob == 80) %>% select(Q80 = quant_flow,station,fc_period) %>% 
  left_join(wide, . , by = c("station","fc_period"))
wide <- quantiles_all %>% filter(exceed_prob == 50) %>% select(Q50 = quant_flow,station,fc_period) %>% 
  left_join(wide, . , by = c("station","fc_period"))

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
           # start_diff_index = as.numeric((start_pred - start_obs) / duration_obs),
           # duration_diff_index = (duration_pred - duration_obs) / duration_obs,
           # qmin_diff_index = (qmin_pred - qmin_obs) / Q80,
           # def.vol_diff_index = (sum(def.increase_pred) - sum(def.increase_obs)) / (Q50*60*60*24*7),
           # severity_diff_index = (ifelse(duration_pred == 0,0,(sum(def.increase_pred)/duration_pred)) - (sum(def.increase_obs)/duration_obs)) / (Q50*60*60*24*7),
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

# pred_obs_diff$def.vol_diff_mean_perc = (pred_obs_diff$def.vol_diff/(pred_obs_diff$Q80*60*60*24*pred_obs_diff$duration_obs))*100

#def.vol_diff_perc = durchschnittliche abweichung zwischen vorhergesagtem und beobachtetem defizitvolumen
#wie viel prozent des Q80-Flows wurde jeden Tag für das Event Falsch vorhergesagt

# pred_obs_diff$def.vol_diff_rel = (pred_obs_diff$def.vol_diff/(pred_obs_diff$Q80))

# pred_obs_diff <- pred_obs_diff %>% mutate(start_diff = as.numeric(start_diff))

names(pred_obs_diff)


## Skill Scores based on Upper BM performance ----

skill_scores <- pred_obs_diff %>% select(horizon, fc_method, station, fc_period, 
                                         start_diff, 
                                         duration_diff,
                                         qmin_diff,
                                         def.vol_diff,
                                         start,
                                         event_name) 
test <- skill_scores %>% filter(start == as.Date("2015-12-27") & 
                                  horizon == 1)
test <- wide %>% filter(date == as.Date("2015-12-27") & 
                                  horizon == 1)

  
skill_scores <- skill_scores %>% setDT() %>%
  .[, .(n_events = .N,
        start_diff = as.numeric(median(start_diff)),
        duration_diff = mean(duration_diff),
        qmin_diff = mean(qmin_diff),
        def.vol_diff = mean(def.vol_diff)),
    by = .(station,horizon,fc_method, fc_period)]

# pred_obs_diff_scaled <- copy(pred_obs_diff)
# 
# pred_obs_diff_scaled[, (vars) := lapply(.SD, min_max_norm), .SDcols = vars,
#                      by = .(station, horizon)]



# summary_hist <- left_join(summary_hist, confusion_df, by = c("station", "fc_method", "horizon", "fc_period"))

skill_scores <- skill_scores %>% filter(fc_method == "Climate_Upper_BM") %>% 
  rename(start_diff_BM = start_diff,
         duration_diff_BM = duration_diff,
         qmin_diff_BM = qmin_diff, 
         def.vol_diff_BM = def.vol_diff) %>% 
  # select(-c(start_diff, duration_diff,  qmin_diff, def.vol_diff)) %>% 
  left_join(skill_scores, ., by = c("station", "fc_period", "horizon"), relationship = "many-to-many") %>% 
  rename(fc_method = fc_method.x) %>% 
  select(-fc_method.y)

skill_scores <- skill_scores %>% 
  mutate("SS-start_diff_BM" = start_diff_BM/start_diff,
         "SS-duration_diff_BM" = duration_diff_BM/duration_diff,
         "SS-qmin_diff_BM" = qmin_diff_BM/qmin_diff,
         "SS-def.vol_diff_BM" = def.vol_diff_BM/def.vol_diff)

fingerprint_data <- skill_scores %>% select(c("SS-start_diff_BM","SS-duration_diff_BM","SS-qmin_diff_BM","SS-def.vol_diff_BM",
                                             horizon, fc_method,fc_period,station)) %>% 
  left_join(confusion_df, ., by = c("station","horizon", "fc_method", "fc_period"))


# PLOTTING OF FINGERPRINTS ----

##create radar plot function ----

radar_plots <- function(data, axis_name_offset, central_distance, 
                        fill_alpha, linetype_grid,alpha_map)
{
  data <- data %>% select(c("SS-start_diff_BM","SS-duration_diff_BM","SS-qmin_diff_BM","SS-def.vol_diff_BM",precision, recall,
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

data <- fingerprint_data %>% filter(station == "Uttendorf",
                                fc_period == "2015_2021",
                                horizon == 3)

## Plot Radar Plots ----
plot <- radar_plots(data = data,
                    axis_name_offset = 0.2, 
                    central_distance = 0.05, 
                    fill_alpha = 0.1, 
                    linetype_grid = 1,
                    alpha_map = 0.01)
print(plot)

### Stations-comparison -----


# Calculating Point Forecasts----

#get fixed start date for each event at each station

temp <- plot_data_wide 

temp$def.increase_pred[which(temp$def.increase_pred < 0) ] <- 0

plot_data_wide <- temp %>%
  group_by(station, horizon, fc_method, fc_period) %>%
  mutate(def.vol_pred_cum = cumsum(def.increase_pred)) %>%
  ungroup() %>%   # ⬅️ remove grouping
  select(station, horizon, fc_method, fc_period, def.vol_pred_cum,date) %>%
  left_join(plot_data_wide, by = c("station", "horizon", "fc_method", "fc_period","date"))


prec_df_all_monthly <- prec_df_all %>% setDT() %>% .[, .(month = lubridate::month(date),
                                                         year = lubridate::year(date),
                                                         station = station,
                                                         precipitation = precipitation,
                                                         swe_added = swe_added,
                                                         swe_melted = swe_melted),]

prec_df_all_monthly <- prec_df_all_monthly[, .(precipitation_monthly_sum = sum(precipitation),
                                               swe_melted_monthly_sum = sum(swe_melted),
                                               swe_added_monthly_sum = sum(swe_added)), 
                                           by = .(month,year,station)]
plot_data <- plot_data_wide
plot_data$year <- lubridate::year(plot_data$date)
plot_data$month <- lubridate::month(plot_data$date)

plot_data <- left_join(plot_data, prec_df_all_monthly, by = c("station","month","year"))

plot_data <- plot_data %>% filter(fc_method == "Climate_Upper_BM") %>% 
  mutate(fc_method = "observed_flow", discharge_pred = discharge_obs, def.increase_pred = def.increase_obs, 
         event.no_pred = event.no_obs, start_pred = start_obs, end_pred = end_obs, duration_pred = duration_obs,
         def.vol_pred = def.vol_obs, fc_period = fc_period,horizon = horizon) %>% 
  rbind(.,plot_data)

plot_data <- plot_data[seq(1,nrow(plot_data),by = 7),]
plot_data$month <- lubridate::month(plot_data$date)

# plot_data[, precipitation_monthly := ]

if (file.exists(paste0("results/",today,"/",model_name,"/historic_events/"))){
} else {
  dir.create(file.path(paste0("results/",today,"/",model_name,"/historic_events/")))
}



fc_years_list <- list(c(2015:2016),c(2003:2004))

for (s in unique(wide$station)) {
  for (fc_years in fc_years_list) {
    
    fc_years <- c(2015:2016)
    
    fc_years_text <- paste0(min(fc_years),"_",max(fc_years))
    
    if (file.exists(paste0("results/",today,"/",model_name,"/historic_events/",fc_years_text))){
    } else {
      dir.create(file.path(paste0("results/",today,"/",model_name,"/historic_events/",fc_years_text)))
    }
    
    # s <- "Flattach"
    data <- plot_data %>% filter(fc_period == fc_years_text &
                              station == s & horizon ==h)
    
    data_prec <- prec_df_all_monthly %>% filter( station == s & year %in% fc_years)
    data_prec$date <- as.Date(paste0(data_prec$year,"-",data_prec$month,"-15"))
    data_prec$x_min <- as.Date(paste0(data_prec$year,"-",data_prec$month,"-5"))
    data_prec$x_max <- as.Date(paste0(data_prec$year,"-",data_prec$month,"-25"))

    Q_max <- max(c(data$discharge_obs,data$discharge_pred))
    P_max <- max(data_prec$precipitation_monthly_sum, na.rm = T)
    P_max <- ceiling(P_max / 50) * 50
    scale_factor <- Q_max / P_max
    # data_prec$y_max <- (P_max *scale_factor) + (0.5*Q_max)
    # data_prec$y_min <- (P_max - data_prec$precipitation_monthly_sum) *scale_factor + (0.5*Q_max)
    data_prec$y_max <- (P_max *scale_factor) 
    data_prec$y_min <- (P_max - data_prec$precipitation_monthly_sum) *scale_factor 
    
      
    # data_prec$prec_dummy <- P_max
    # data_prec$prec_hanging <- P_max - data_prec$precipitation_monthly_sum

    data_flow <- flow_obs_all %>% filter(station ==s & lubridate::year(date) %in% fc_years)
    Q80_y_intercept <- quantiles_all %>% filter(station == s &
                                                  fc_period == fc_years_text &
                                                  exceed_prob == 80) %>% pull(quant_flow)
    
    p_obs <-
      ggplot() + 
      geom_rect(data = data_prec, aes(xmin = x_min, xmax = x_max, 
                                      ymin = y_min, ymax = y_max, fill = "Precipitation"), 
                alpha = 0.8, inherit.aes = FALSE)+
      geom_hline(aes(yintercept = Q80_y_intercept, color = "Q80 Threshold"), linetype = "dotted") +
      scale_fill_manual(
        values = c("Precipitation" = "steelblue"),
        name = "Legend")+
      geom_hline(yintercept = Q80_y_intercept, linetype = "dotted") +
      scale_color_manual(
        values = c("Observed Flow" = "black",
                   "Q80 Threshold" = "darkred"),
        name = "Legend")  +
      scale_y_continuous(
        name = "Discharge [m³/s]",
        sec.axis = sec_axis(
          ~ (Q_max - .) / scale_factor,  # reverse transformation
          # ~ P_max - ((. - (0.5*Q_max)) / scale_factor),
          name = "Precipitation [mm/month]",
          breaks = seq(0, P_max, by = 50) ))+
      geom_line(
        data = data_flow,
        aes(x = date, y = flow_mean, color = "Observed Flow")
      ) +
      theme_bw() +
      scale_x_date(breaks = seq(as.Date(paste0(fc_years[1],"-01-01")),
                                as.Date(paste0(fc_years[2],"-12-31")), by = "3 month") )+
      lims(x = c(as.Date(paste0(fc_years[1],"-01-01")),
                 as.Date(paste0(fc_years[2],"-12-31"))))+
      labs(title = paste0("Historic Drought Event - Station: ",s," Period: ",fc_years_text))
    
    png(filename = paste0("explorative_plots/historic_events/",s,"_",fc_years_text,".png"),
        width = 1200, height = 600, units = "px")
    print(p_obs)
    dev.off()
    
    
    for (h in unique(plot_data_wide$horizon)) {
      
    drought_data <- plot_data %>% filter(fc_period == fc_years_text &
                                     station == s & horizon ==h)

    segments <- ggplot(drought_data, aes(x = start_pred, xend = end_pred, y = fc_method, yend = fc_method, color = fc_method)) +
      geom_segment(linewidth = 5, lineend = "round") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        panel.grid = element_blank()
      ) +
      labs(x = "Date", y = NULL) +
      scale_color_manual(values = c("no_seasonality" = "steelblue",
                                    # "naive_season" = "darkred",
                                    "observed_flow" = "black",
                                    "naive_lag" = "orange",
                                    "Climate_Upper_BM" = "forestgreen",
                                    "Ensemble_Model" = "red2"),
                         name = "Forecasting Models")+
      scale_x_date(breaks = seq(as.Date(paste0(fc_years[1],"-01-01")),
                                as.Date(paste0(fc_years[2],"-12-31")), by = "3 month") )+
      lims(x = c(as.Date(paste0(fc_years[1],"-01-01")),
                 as.Date(paste0(fc_years[2],"-12-31"))))+
      theme_bw()
    
    p_flow <- drought_data %>% 
      ggplot()+
      geom_line(data = data_flow, aes(x = date, y = flow_mean))+
      geom_line(aes(x = date, y = discharge_pred, color = fc_method),linewidth = 1)+
      geom_hline(yintercept = Q80_y_intercept, linetype = "dotted",linewidth = 1)+
      scale_color_manual(values = c("no_seasonality" = "steelblue",
                                    "naive_season" = "darkred",
                                    "observed_flow" = "black",
                                    "naive_lag" = "orange",
                                    "Climate_Upper_BM" = "forestgreen",
                                    "Ensemble_Model" = "red2"),
                         name = "Forecasting Models") +
      scale_x_date(breaks = seq(as.Date(paste0(fc_years[1],"-01-01")),
                                as.Date(paste0(fc_years[2],"-12-31")), by = "3 month") )+
      lims(x = c(as.Date(paste0(fc_years[1],"-01-01")),
                 as.Date(paste0(fc_years[2],"-12-31"))))+
      theme_bw()
    
    
    p_def <- drought_data %>% 
      filter(def.increase_pred > 0) %>% 
      ggplot()+
      geom_line(aes(x = date, y = def.increase_pred, color = fc_method),linewidth = 1)+
      scale_color_manual(values = c("no_seasonality" = "steelblue",
                                    "naive_season" = "darkred",
                                    "observed_flow" = "black",
                                    "naive_lag" = "orange",
                                    "Climate_Upper_BM" = "forestgreen",
                                    "Ensemble_Model" = "red2"),
                         name = "Forecasting Models") +
      scale_x_date(breaks = seq(as.Date(paste0(fc_years[1],"-01-01")),
                                as.Date(paste0(fc_years[2],"-12-31")), by = "3 month") )+
      lims(x = c(as.Date(paste0(fc_years[1],"-01-01")),
                 as.Date(paste0(fc_years[2],"-12-31"))))+
      theme_bw()
   
    p_cum_def <- drought_data %>% 
      ggplot()+
      geom_line(aes(x = date, y = def.vol_pred_cum , color = fc_method),linewidth = 1)+
      scale_color_manual(values = c("no_seasonality" = "steelblue",
                                    "naive_season" = "darkred",
                                    "observed_flow" = "black",
                                    "naive_lag" = "orange",
                                    "Climate_Upper_BM" = "forestgreen",
                                    "Ensemble_Model" = "red2"),
                         name = "Forecasting Models") +
      scale_x_date(breaks = seq(as.Date(paste0(fc_years[1],"-01-01")),
                                as.Date(paste0(fc_years[2],"-12-31")), by = "3 month") )+
      lims(x = c(as.Date(paste0(fc_years[1],"-01-01")),
                 as.Date(paste0(fc_years[2],"-12-31"))))+
      theme_bw()
    
    
    png(file=paste0("results/",today,"/",model_name,"/historic_events/",fc_years_text,"/",model_name,"_historic_events_stacked_",s,"_",fc_years_text,"_",h,".png"),
        width = 1200, height = 800, units = "px")
    
    
    plot <- (p_obs + 
        theme(
          axis.title.x = element_blank(),
              axis.text.x  = element_blank(),
              axis.ticks.x = element_blank(),
              plot.margin  = margin(0,5,0,5),
              legend.position = "none")) /
    (p_flow + 
        theme(
          axis.title.x = element_blank(),
              axis.text.x  = element_blank(),
              axis.ticks.x = element_blank(),
              plot.margin  = margin(0,5,0,5),
              legend.position = "none")) /
      
      (segments +
         theme(axis.title.x = element_blank(),
               axis.text.x  = element_blank(),
               axis.ticks.x = element_blank(),
               plot.margin  = margin(0,5,0,5),
               legend.position = "none")) /
      
      (p_cum_def +
         theme(plot.margin = margin(0,5,0,5),
               legend.position = "bottom"))
    print(plot)
    dev.off()
    }
  }
}

