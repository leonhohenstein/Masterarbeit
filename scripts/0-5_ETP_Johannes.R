#### Functions for creating modelling data

computing_external_radiation <- function(lat, J) # J is doy
{
  # solar declination, rad (1 rad = 57.2957795 deg)
  delta <- 0.409 * sin(0.0172 * J - 1.39)
  # relative distance Earth-Sun, []
  dr <- 1 + 0.033 * cos(0.0172 * J)
  # sunset hour angle, rad
  latr <- lat / 57.2957795
  sset <- -tan(latr) * tan(delta)
  omegas <- sset * 0
  omegas[abs(sset) <= 1] <- acos(sset[abs(sset) <= 1])
  # correction for high latitudes
  omegas[sset < (-1)] <- max(omegas)
  # Ra, MJ m-2 d-1
  Ra <- 37.6 * dr *
    (omegas * sin(latr) * sin(delta) + cos(latr) * cos(delta) * sin(omegas))
  Ra <- ifelse(Ra < 0, 0, Ra)
  Ra
}

computing_hargreaves <- function(Tmax, Tmin, lat, Prec, J)
{
  Tmean <- (Tmin + Tmax)/2
  Tr <- Tmax - Tmin
  Tr <- pmax(0,Tr)
  ET0 <- Tmin*NA
  Ra <- computing_external_radiation(lat = lat, J = J)
  ab <- Tr - 0.0123*Prec
  ET0 <- 0.0013 * 0.408 * Ra * (Tmean + 17.0) * ab^0.76
  ET0[is.nan(ab^0.76)] <- 0
  ET0 <- ifelse(ET0 < 0, 0, ET0)
  ET0
}

calc_precip_seasonality <- function(prec, temp, day)
{
  # extract day of year
  t_julian<-strptime(format(day,'%Y%m%d'),'%Y%m%d')$yday
  
  # estimate annual temperature and precipitation cycles using sine curves
  # nls (nonlinear least squares function) is used for the non-linear regression
  # a first guess is needed for the phase shift of precipiation (s_p)
  s_p_first_guess<-90-which.max(rapply(split(prec,format(day,'%m')),mean,na.rm=TRUE))*30
  s_p_first_guess<-s_p_first_guess%%360 # if necessary, convert to a value between 0 and 360
  
  fit_temp = nls(temp ~ mean(temp,na.rm=TRUE)+delta_t*sin(2*pi*(t_julian-s_t)/365.25),start=list(delta_t=5,s_t=-90))
  fit_prec = nls(prec ~ mean(prec,na.rm=TRUE)*(1+delta_p*sin(2*pi*(t_julian-s_p)/365.25)),start=list(delta_p=0.4,s_p=s_p_first_guess))
  
  s_p<-summary(fit_prec)$par['s_p','Estimate']
  delta_p<-summary(fit_prec)$par['delta_p','Estimate']
  s_t<-summary(fit_temp)$par['s_t','Estimate']
  delta_t<-summary(fit_temp)$par['delta_t','Estimate']
  
  delta_p_star<-delta_p*sign(delta_t)*cos(2*pi*(s_p-s_t)/365.25)
  
  # fraction of precipitation falling as snow - using sine curves
  # see original paper by Woods, 2009, Advances in Water Resources, 10.1016/j.advwatres.2009.06.011
  t_0<-1 # temp thershold [°C]
  t_star_bar<-(mean(temp,na.rm=TRUE)-t_0)/abs(delta_t)
  
  if (t_star_bar>1){ # above freezing all year round
    
    f_s<-0
    
  } else if (t_star_bar<(-1)){ # below freezing all year round
    
    f_s<-1
    
  } else {
    
    # there is a square in the original Woods paper (Eq. 13) lacking in the Berghuijs paper (Eq. 6a)
    f_s<-1/2-asin(t_star_bar)/pi-delta_p_star/pi*sqrt(1-t_star_bar^2)
    
  }
  
  # fraction of precipitation falling as snow - using daily temp and precip values
  if(any(temp<=0&prec>0,na.rm=TRUE)){
    
    f_s_daily<-sum(prec[temp<=0])/sum(prec)
    
  } else {
    
    f_s_daily<-0
    
  }
  tibble(p_season = delta_p_star, frac_snow = f_s_daily)
}

month2sea<-function(m){
  
  if(!is.numeric(m)){m<-as.numeric(m)}
  
  s<-m
  s[m%in%c(12,1,2)]<-'djf'
  s[m%in%3:5]<-'mam'
  s[m%in%6:8]<-'jja'
  s[m%in%9:11]<-'son'
  
  return(as.factor(s))
  
}

compute_extreme_precip_indices <- function(prec, day, rel_hp_thres, abs_lp_thres)
{
  
  # input variables:
  # rel_hp_thres: the high precipitation threshold is relative [mean daily precipitation]
  # abs_lp_thres: the low precipitation threshold is absolute [mm/day]
  
  # check data availibility
  # extract season and hydrological year
  s<-as.factor(month2sea(format(day,'%m')))
  
  # frequency and duration of high intensity precipitation events
  hp<-prec>=rel_hp_thres*mean(prec,na.rm=TRUE)
  hp[is.na(hp)]<-F # if no precip data available, consider it is not an event
  hp_length<-nchar(strsplit(paste(ifelse(hp,'H','-'),collapse=''),'-')[[1]]) # compute number of consecutive high precip days
  hp_length<-hp_length[hp_length>0]
  if(sum(hp_length)!=sum(hp)){stop('Unexpected total number of high precip days')}
  
  if(length(hp_length)>0){ # at least one high precipitation event in the provided time series
    
    hp_freq<-sum(hp)/length(hp)*365.25
    hp_dur<-mean(hp_length)
    hp_sea<-rapply(split(hp[hp],s[hp],drop=TRUE),length)
    
    if(max(rank(hp_sea)%%1!=0)){ # if tie between seasons with the most days with high precipitation, set timing to NA
      
      hp_timing<-NA
      
    } 
    else
    {
      
      hp_timing<-names(hp_sea)[which.max(hp_sea)]
      
    }
    
  }
  else { # not a single high precipitation event in the provided time series
    
    hp_freq<-0
    hp_dur<-0
    hp_timing<-NA
    
  }
  
  # frequency and duration of low intensity precipitation events
  lp<-prec<abs_lp_thres
  lp[is.na(lp)]<-F # if no precip data available, consider it is not an event
  lp_length<-nchar(strsplit(paste(ifelse(lp,'L','-'),collapse=''),'-')[[1]]) # compute number of consecutive low precip days
  lp_length<-lp_length[lp_length>0]
  
  lp_freq<-sum(lp)/length(lp)*365.25
  lp_dur<-mean(lp_length)
  lp_sea<-rapply(split(lp[lp],s[lp],drop=TRUE),length)
  
  if(max(rank(lp_sea)%%1!=0)){ # if tie between seasons with the most days with low precipitation, set timing to NA
    
    lp_timing<-NA
    
  } else{
    
    lp_timing<-names(lp_sea)[which.max(lp_sea)]
    
  }
  
  return(tibble(hi_prec_fr=hp_freq,hi_prec_du=hp_dur,hi_prec_ti=hp_timing,
                lo_prec_fr=lp_freq,lo_prec_du=lp_dur,lo_prec_ti=lp_timing))
  
  
}

get_climate_characteristics <- function(fname, lat, id = NULL, start = ymd("1991-01-01"), end = ymd("2020-12-31"))
{
  x <- read_csv(fname, show_col_types = FALSE)
  #id <- parse_number(basename(fname))
  x <- x %>% filter(between(date, start, end))
  # Transform sunshine duration to hours per day and mean temperature, day of the year
  x <- x %>% mutate(sa = sa/(60*60), tm = (tx + tn)/2, doy = yday(date))
  # Computing Hargreaves ET0
  x <- x %>% mutate(et0 = computing_hargreaves(Tmax = tx, Tmin = tn, lat = lat, Prec = rr, J = doy))
  # Computing p_mean, et0_mean, arid_1
  p_mean <- mean(x$rr)
  et0_mean <- mean(x$et0)
  arid_1 <- et0_mean/p_mean
  # p_season and fraction of snow
  tbl <- calc_precip_seasonality(prec = x$rr, temp = x$tm, day = x$date)
  # high and low precipitation indices
  extreme_indices <- compute_extreme_precip_indices(prec = x$rr, day = x$date, rel_hp_thres = 5, 
                                                    abs_lp_thres = 1)
  ### Add additional climatic variables
  cwb <- mean(p_mean - et0_mean)
  ### Mean annual snow depth
  snow <- mean(x$snow)
  tm <- mean(x$tm)
  tx <- mean(x$tx)
  tn <- mean(x$tn)
  tr <- tx - tn
  out <- tibble(id = id, p_mean = p_mean, et0_mean = et0_mean, arid_1 = arid_1, p_season = tbl$p_season, frac_snow = tbl$frac_snow, 
                cwb_mean = cwb, snow_mean = snow, tm_mean = tm, tx_mean = tx, tn_mean = tn, tr_mean = tr)
  bind_cols(out, extreme_indices)
}

