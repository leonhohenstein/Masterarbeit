
rm(list = ls())

install.packages("rsoi")
require(rsoi)
enso = rsoi::download_enso()
nao = rsoi::download_nao()
ao = rsoi::download_ao()
mei = rsoi::download_mei()
soi = rsoi::download_soi()

#load data from National Oceanic and Atmospheric Administration
# at: https://ftp.cpc.ncep.noaa.gov/cwlinks/
enso = rsoi::download_enso()

raw_data <- read.csv2(file = "data/climate/norm.daily.ao.cdas.z1000.19500101_current.csv", 
                      header=T, 
                      skip=0,dec = ".", 
                      na.strings = c("Lücke", "NA"), 
                      sep = ",",
                      stringsAsFactors = F)
raw_data$date <- make_date(year = raw_data$year, month = raw_data$month, day = raw_data$day)

df <- raw_data 

raw_data <- read.csv2(file = "data/climate/norm.daily.pna.cdas.z500.19500101_current.csv", 
                      header=T, 
                      skip=0,dec = ".", 
                      na.strings = c("Lücke", "NA"), 
                      sep = ",",
                      stringsAsFactors = F)
raw_data$date <- make_date(year = raw_data$year, month = raw_data$month, day = raw_data$day)

df <- raw_data %>%  select(c("date","pna_index_cdas")) %>% left_join(df, ., by = "date")

df <- enso %>% rename(date = Date) %>% select(c("ONI","SOI","NPGO","date")) %>% 
  left_join(df, ., by = "date")

df <- df %>%  setDT()


# df %>% 
#   filter(year == 2015) %>% 
#   ggplot()+
#   geom_line(aes(x = date, y = pna_index_cdas), color = "steelblue")+
#   geom_line(aes(x = date, y = ao_index_cdas), color = "forestgreen")+
#   geom_line(data = nao, aes(x = Date, y = ))
# 
# enso %>% 
#   filter(Year == 2015) %>%  
#   ggplot()+
#   geom_line(aes(x = Date, y = ONI), color = "steelblue")+
#   geom_line(aes(x = Date, y = SOI), color = "forestgreen")+
#   geom_line(aes(x = Date, y = NPGO), color = "darkred")

