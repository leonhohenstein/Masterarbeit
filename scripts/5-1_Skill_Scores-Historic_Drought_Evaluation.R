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

load(file = (paste0("results/",today,"/",model_name,"/quantiles_all.RData")))
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

#DEFINE DROUGHT OBJECT FUNCTION----

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

#LOAD DATASET AND CALCULATE DROUGHT CHARACTERISTICS----
# 
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
# drought_objects <- drought_objects %>% select(-c(event.orig,pred_obs))
# save(drought_objects, file = (paste0("results/",today,"/",model_name,"/drought_object.RData")))


#EVALUATE PRED OBS DIFFERENCES ----
load(file = (paste0("results/",today,"/",model_name,"/drought_object.RData")))

test_data <- drought_objects %>% filter(fc_method %in% c("observed","catchment_memory") & horizon == 1 &
                                          fc_period == "2015_2021" &station == "Tauchenbach")


drought_characteristics_accuracy <- function(data, model, event_percentile_th){
  
  data <- test_data %>%
    setDT() %>%
    .[, `:=`(
      start    = fifelse(event.no > 0, first(date), as.Date(NA)),
      end      = fifelse(event.no > 0, last(date),  as.Date(NA)),
      duration = fifelse(event.no > 0, as.integer(last(date) - first(date) + 1), 0),
      date     = date,
      def.increase = def.increase,
      def.vol = sum(def.increase),
      qmin      = min(discharge),
      pred_obs = ifelse(fc_method == "observed","obs","pred")),
      by = .(station, fc_method, horizon, fc_period,event.no), ]
  
  wide <- data %>% select(-fc_method) %>% 
    pivot_wider(
      id_cols     = all_of(c("date","station","fc_period","horizon","threshold")),    
      names_from  = pred_obs,
      values_from =  c(event.no, discharge, def.increase,start,end,duration,qmin,def.increase,def.vol),
      names_prefix = "") 
  
  wide <- wide %>% setDT() %>% 
    .[, `:=` (start_pred = na.locf(start_pred, fromLast = TRUE, na.rm = F),
              end_pred = na.locf(end_pred, fromLast = TRUE, na.rm = F))
      ,by = .(station, horizon, fc_period), ]
  
  
  wide <- wide %>% mutate(duration_pred = end_pred - start_pred + 1)
  
  
  wide <- wide %>% setDT() %>% 
    .[, `:=` (qmin_pred = min(discharge_pred),
              def.vol_pred = ((threshold-discharge_pred)*60*60*24))
      ,by = .(station, horizon, fc_period,event.no_obs), ]

  
  compound_event_characts <- function(event_pred_id){
    event.obs_id <- event_pred_id
    event.obs_id <- 8
    
    event.pred_id <- wide %>% filter(event.no_obs == event.obs_id) %>% pull(event.no_pred) %>% unique()
    event.pred_id <- if (all(event.pred_id == 0)) 0 else event.pred_id[event.pred_id != 0]    
    
    start_obs <- if (all(event.pred_id == 0)) NA else wide %>% filter(event.no_pred %in% event.pred_id) %>% pull(start_pred) %>% unique()
    end_obs <- if (all(event.pred_id == 0)) NA else wide %>% filter(event.no_pred  %in% event.pred_id) %>% pull(end_pred) %>% unique()
    
    duration_pred <- if (all(event.pred_id == 0)) NA else sum(end_obs - start_obs+1) %>% as.numeric()
    start_obs <- min(start_obs)
    
    qmin <- if (all(event.pred_id == 0)){
      wide %>% filter(event.no_obs == event.obs_id) %>% pull(discharge_pred) %>% min()
    } else {
      wide %>% filter(event.no_pred %in% event.pred_id) %>% pull(discharge_pred) %>% min()
    }
    
    def.vol <- if (all(event.pred_id == 0)) NA else wide %>% filter(event.no_pred %in% event.pred_id) %>% pull(def.increase_pred) %>% sum()
    severity_pred <- if (all(event.pred_id == 0)) NA else def.vol/duration_pred
    results <- data.frame(event.obs_id=event.obs_id, start_obs = start_obs,def.vol_pred_comp = def.vol,
                          qmin_pred_comp =qmin, duration_pred_comp = duration_pred, severity_pred_comp = severity_pred)
    return(results)
    
  }
  ids <- setdiff(unique(wide$event.no_pred),0)
  compound_drought_obj <- do.call(rbind,lapply(ids,compound_event_characts))
  
}






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


#CALCULATE SKILL SCORES----

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














