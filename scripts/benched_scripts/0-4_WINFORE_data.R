library(tidyverse)
library(ggplot2)
library(glmnet)
library(data.table)


rm(list=ls())

############################################################################

########### Data Preparation and Analysis script for the WINFORCE data set #####################

############################################################################


### model parameters ----

dataset <- "TB" #TB = Tauchenbach

### loading data ----
df <- read.csv2(paste0("data/tauchenbach/TB_Winfore_19610101_20241231.csv"), 
                header=T, skip=0,dec = ".", na.strings = c("Lücke", "NA"), sep = ",") 
                #na.strings = ("L\374cke") if there is Lücke instead of NA 

df <- fread(paste0("data/tauchenbach/TB_Winfore_19610101_20241231.csv"))

df$time <- gsub("T", " ", df$time)  # Replace "T" with a space
df$time <- gsub("\\+00:00", "", df$time)  # Remove timezone
df$time <- ymd_hm(df$time, tz = "UTC")
colnames(df) <- c("Date","ETP","SPEI30","SPEI365","SPEI90","lat","lon")

df[,year := lubridate:: year(df$Date)]
df[,month := lubridate:: month(df$Date)]
df[,day := lubridate::day(df$Date)]
df[,point := .GRP, by = lon]


### quick analysis of how heterogeneous the grid cells of the catchment are ----#

unique(df$lat)
df[year == 2000 &
     month == 3 &
     point %in% c(1,2,3)] %>% 
  
  ggplot(aes(x=Date , y= ETP, color = lat))+
  geom_line(alpha = 0.5)




df[year == 2000 &
     month == 7 &
     day  == 3, ETP]

