library(ggplot2)
library(sf)
library(data.table)
library(rnaturalearth)
library(rnaturalearthdata)
library(jsonlite)
library(tidyverse)
library(terra)

# library(httr)
library(ncdf4)

rm(list=ls())

# Load Austria polygon
austria <- ne_countries(scale = "medium", country = "Austria", returnclass = "sf")

# catchment_shp  <- st_read("C:/Users/Leon/Desktop/Masterarbeit/GIS/results/test.shp")
# catchment_shp <- catchment_shp %>% filter(HZBFG %in% c( "Donau von der Traun bis zum Kamp (ohne Enns)"  ,            
#                                                         "Donau vom Inn bis zur Traun"   ,                            
#                                                         "Donau vom Kamp einschliesslich bis zur Leitha (ohne March)"))
# 
# catchment_shp_test_kleiner <- catchment_shp %>% filter(HZBFG %in% c( "Donau von der Traun bis zum Kamp (ohne Enns)"  ,            
#                                                         "Donau vom Inn bis zur Traun"   ))
# 
# catchment_shp_test <- st_union(catchment_shp)
# catchment_shp_test_kleiner <- st_union(catchment_shp_test_kleiner)

# st_write(catchment_shp_test_kleiner, "C:/Users/Leon/Desktop/Masterarbeit/GIS/merged_polygons_Kienstock_catchment.shp")

catchment_Kienstock_shp  <- st_read("C:/Users/Leon/Desktop/Masterarbeit/GIS/merged_polygons_kienstock_catchment.shp")

stations_metadata <- data.frame(station_name = c("Tauchenbach","Kienstock","Flattach","Uttendorf"),
                                catchment_size = c(175,95970,705,128),
                                elevation = c(247,194,687,789),
                                hzb_nr = c(210252,207357,213124,203554),
                                lat = c(47.34697, 48.38217,46.933330,47.26654),
                                lon = c(16.27815,15.46268,13.13712,12.56822))


ggplot(data = austria) +
  geom_sf(fill = "lightblue", color = "black") +
  ggtitle("Austria Map with Polygon")+
  theme_bw()


spartacus <- "D:/Masterarbeit/data/spartacus/"



#### PLOT ALL AVAILABLE STATIONS ----

coords_stations <- st_as_sf(stations_metadata, coords = c("lon", "lat"), crs = 4326)


ggplot() +
  geom_sf(data = austria, fill = "lightgrey", color = "black",linewidth = 1.5, alpha = 0.6) +  
  geom_sf(data = catchment_Kienstock_shp, color = "steelblue",fill = "steelblue")+
    geom_sf(data = coords_stations, color = "black", fill = "red", size = 2, shape = 21, stroke = 1, alpha = 0.7) +  # Points with black outline

  # geom_text(data = points_df, aes(x = lon, y = lat, label = label), vjust = -1, fontface = "bold") +  # Bold labels
  ggtitle("Measurement Stations of Water Temperature in Austria (663 Stations)") +
  theme_minimal() +  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14))




path <- "D:/Masterarbeit/data/spartacus/"
# parameter <- "RR"
year <- 1961:2021
results_rbind <- NULL
results_final <- list()
results_param <- list()
results_param <- list()
count <- 0
params <- c(
  # "RR",
  # "SR",
         # "SA",
         # "TN",
               # "TX"
"swe_tot",
  "snow_depth"
         )

for(param in  params){

#### for stations with point location ----
# 

for(s in unique(coords_stations$station_name))  {

  results_rbind <- NULL

  file <- paste0("SPARTACUS2-DAILY_",param,"_",1990)

  if(param == "swe_tot")
  {
    file <- paste0("SNOWGRID-CL_swe_tot_",1990)
  }

  if(param == "snow_depth")
  {
    file <- paste0("SNOWGRID-CL_snow_depth_",1990)
  }

  data_nc <- nc_open(paste0(path,param,"/",file,".nc"))


  lat <- ncvar_get(data_nc, "lat") %>% # Dimensions: (x, y)
    as.vector()
  lon <- ncvar_get(data_nc, "lon") %>%  # Dimensions: (x, y)
    as.vector()

  coords_spartacus <- data.frame(lat =lat, lon = lon)

  coords_spartacus <- st_as_sf(coords_spartacus, coords = c("lon", "lat"), crs = 4326)  # Start with WGS84 (EPSG:4326)


    for (n in year) {

      # file <- paste0("SPARTACUS2-DAILY_",param,"_",n)

      data_nc <- nc_open(paste0(path,param,"/",file,".nc"))


      #buffer stations with 5 km radius
      stations_buffered <- coords_stations %>% filter(station_name == s) %>%
        st_transform(., crs = 3857)  # Project to meters
      stations_buffered <- st_buffer(stations_buffered, dist = 5000)  # 5 km buffer
      stations_buffered <- st_transform(stations_buffered, crs = 4326)  # Back to WGS84

      stations_buffered <- st_join(coords_spartacus, stations_buffered, join = st_intersects, left = FALSE)

      # Get indices of selected points in original coords_spartacus
      selected_indices <- which(st_coordinates(coords_spartacus)[,1] %in% st_coordinates(stations_buffered)[,1] &
                                  st_coordinates(coords_spartacus)[,2] %in% st_coordinates(stations_buffered)[,2])

      # stations_spartacus <- coords_spartacus[indices,]
      # #
      # stations_spartacus$ID <- coords_stations$hzb_nr
      # #
      array <- ncvar_get(data_nc, param)  # Dimensions: (x, y, time)

      results <- list()

      # Loop through each time step
      for (t in 1:dim(array)[3]) {  # Loop over the time dimension
        # Extract the 2D slice for the current time step
        spatial_grid <- array[, , t]

        # Use the indices to extract the values for the grid cells of interest
        results[[t]] <- spatial_grid[selected_indices]
      }

      results <- do.call(rbind, results)

      results <- results %>% as.data.frame()

      results <- rowMeans(results)

      time <- ncvar_get(data_nc, "time")

      time_units <- ncatt_get(data_nc, "time", "units")$value

      reference_date <- as.Date(sub("days since ", "", time_units))  # Extract the reference date

      results <- data.frame(
        date = reference_date + time,
        value = results
      )

      names(results)[2] <- param
      count <- count +1
      print(paste0("finished ",count," / ",(length(year)*length(params)*length(unique(coords_stations$hzb_nr)))))

      results_rbind <- rbind(results_rbind, results)
    }

  results_param[[paste0(s)]][[paste0(param)]] <- results_rbind

}



#### repeat the same for the whole upstream catchment for Kienstock station ----

# results_rbind <- NULL
# 
#   for (n in year) {
#     # n <- 1961
#     # param <- "RR"
#     file <- paste0("SPARTACUS2-DAILY_",param,"_",n)
# 
#     if(param == "swe_tot")
#       {
#       file <- paste0("SNOWGRID-CL_swe_tot_",n)
#     }
# 
#     if(param == "snow_depth")
#     {
#       file <- paste0("SNOWGRID-CL_snow_depth_",n)
#     }
# 
#     data_nc <- nc_open(paste0(path,param,"/",file,".nc"))
# 
#     lat <- ncvar_get(data_nc, "lat") %>% # Dimensions: (x, y)
#       as.vector()
#     lon <- ncvar_get(data_nc, "lon") %>%  # Dimensions: (x, y)
#       as.vector()
# 
#     coords_spartacus <- data.frame(lat =lat, lon = lon)
# 
#     coords_spartacus <- st_as_sf(coords_spartacus, coords = c("lon", "lat"), crs = 4326)  # Start with WGS84 (EPSG:4326)
# 
# 
#     points_catchment <- st_join(coords_spartacus, catchment_Kienstock_shp, join = st_intersects, left = FALSE)
# 
#     # Get indices of selected points in original coords_spartacus
#     selected_indices <- which(st_coordinates(coords_spartacus)[,1] %in% st_coordinates(points_catchment)[,1] &
#                                 st_coordinates(coords_spartacus)[,2] %in% st_coordinates(points_catchment)[,2])
# 
#     # stations_spartacus <- coords_spartacus[indices,]
#     # #
#     # stations_spartacus$ID <- coords_stations$hzb_nr
#     # #
#     array <- ncvar_get(data_nc, param)  # Dimensions: (x, y, time)
# 
#     results <- list()
# 
#     # Loop through each time step
#     for (t in 1:dim(array)[3]) {  # Loop over the time dimension
#       # Extract the 2D slice for the current time step
#       spatial_grid <- array[, , t]
# 
#       # Use the indices to extract the values for the grid cells of interest
#       results[[t]] <- spatial_grid[selected_indices]
#     }
# 
#     results <- do.call(rbind, results)
# 
#     results <- results %>% as.data.frame()
# 
#     n_NAs <- rowSums(is.na(results))
# 
#     results <- rowMeans(results, na.rm = T)
# 
#     time <- ncvar_get(data_nc, "time")
# 
#     time_units <- ncatt_get(data_nc, "time", "units")$value
# 
#     reference_date <- as.Date(sub("days since ", "", time_units))  # Extract the reference date
# 
#     results <- data.frame(
#       date = reference_date + time,
#       value = results,
#       n_NAs = n_NAs
#     )
# 
#     names(results)[2] <- param
# 
#     print(paste0("finished ",n," / ",length(year)))
# 
#     results_rbind <- rbind(results_rbind, results)
#   }
# 
#   results_param[["catchment_Kienstock"]] <- results_rbind
# 
#   results_final[[paste0(param)]] <- results_param

}


results_snow_stations <- results_param
save(results_snow_stations, file = "data/climate/spartacus_results_snow_stations.RData")

load( file = "data/climate/spartacus_clipped_SN_TN.RData")

save <- results_final_SA_TN

####### FOR SR PARAM ONLY ----

# 
# path <- "N:/H851_STAT/Projekte/Daten_Umweltstatistik/GeoSphere/gridded_data/Spartacus/SR/SR_19610101_20221031.nc"
# r <- rast("N:/H851_STAT/Projekte/Daten_Umweltstatistik/GeoSphere/gridded_data/Spartacus/SR/SR_19610101_20221031.nc")
# r
# 
# #crop raster file to reduce size and increase efficiency ----
# 
# # Convert coords_stations to SpatVector and match CRS
# if (!inherits(coords_stations, "sf")) {
#   coords_stations <- st_as_sf(coords_stations, coords = c("lon", "lat"), crs = 4326)
# }
# 
# stations_vect <- vect(coords_stations)  # assumes coords_stations is sf or dataframe with lon/lat
# stations_vect <- project(stations_vect, crs(r))
# 
# # Buffer 5 km (optional)
# stations_buffered <- buffer(stations_vect, width = 5000)
# 
# if (!inherits(catchment_Kienstock_shp, "sf")) {
#   catchment_Kienstock_shp <- st_as_sf(catchment_Kienstock_shp)  # just convert if needed
# }
# catchment_vect <- vect(st_transform(catchment_Kienstock_shp, crs(r)))
# 
# stations_shp <- c(catchment_vect,stations_buffered)
# 
# r_crop <- crop(r, stations_shp)
# 
# extracted <- extract(r_crop, stations_shp, fun = mean, na.rm = TRUE, exact = FALSE)
# 
# extracted
# start_date <- as.Date("1961-01-01")  # adjust to match your data
# dates <- seq(start_date, by = "day", length.out = nlyr(rr))
# 
# # Reshape
# result_long <- result %>%
#   pivot_longer(-ID, names_to = "layer", values_to = "value") %>%
#   mutate(date = rep(dates, times = nrow(buffered_stations)))
# 
# 
# #### extracting SR for Kienstock-catchment
# 
# 
# 
# parameter <- "SR"
# year <- 1961:2021
# results_rbind <- NULL
# results_final <- list()
# results_param <- list()
# 
# param <- "SR"
#   
# results_param <- list()
#   
#   for(s in unique(coords_stations$station_name))  {
#     
#     results_rbind <- NULL
#     
#     data_nc <- nc_open("N:/H851_STAT/Projekte/Daten_Umweltstatistik/GeoSphere/gridded_data/Spartacus/SR/SR_19610101_20221031.nc")
#     
#     
#     lat <- ncvar_get(data_nc, "lat") %>% # Dimensions: (x, y) 
#       as.vector() 
#     lon <- ncvar_get(data_nc, "lon") %>%  # Dimensions: (x, y)
#       as.vector() 
#     
#     coords_spartacus <- data.frame(lat =lat, lon = lon)
#     
#     coords_spartacus <- st_as_sf(coords_spartacus, coords = c("lon", "lat"), crs = 4326)  # Start with WGS84 (EPSG:4326)
#     
#     
#     for (n in year) {
#       
#       
#       #buffer stations with 5 km radius
#       stations_buffered <- coords_stations %>% filter(station_name == s) %>% 
#         st_transform(., crs = 3857)  # Project to meters
#       stations_buffered <- st_buffer(stations_buffered, dist = 5000)  # 5 km buffer
#       stations_buffered <- st_transform(stations_buffered, crs = 4326)  # Back to WGS84
#       
#       stations_buffered <- st_join(coords_spartacus, stations_buffered, join = st_intersects, left = FALSE)
#       
#       # Get indices of selected points in original coords_spartacus
#       selected_indices <- which(st_coordinates(coords_spartacus)[,1] %in% st_coordinates(stations_buffered)[,1] &
#                                   st_coordinates(coords_spartacus)[,2] %in% st_coordinates(stations_buffered)[,2])
#       
#       # stations_spartacus <- coords_spartacus[indices,]
#       # # 
#       # stations_spartacus$ID <- coords_stations$hzb_nr
#       # # 
#       array <- ncvar_get(data_nc, param)  # Dimensions: (x, y, time)
#       
#       results <- list()
#       
#       # Loop through each time step
#       for (t in 1:dim(array)[3]) {  # Loop over the time dimension
#         # Extract the 2D slice for the current time step
#         spatial_grid <- array[, , t]
#         
#         # Use the indices to extract the values for the grid cells of interest
#         results[[t]] <- spatial_grid[selected_indices]
#       }
#       
#       results <- do.call(rbind, results)
#       
#       results <- results %>% as.data.frame()
#       
#       results <- rowMeans(results)
#       
#       time <- ncvar_get(data_nc, "time")
#       
#       time_units <- ncatt_get(data_nc, "time", "units")$value
#       
#       reference_date <- as.Date(sub("days since ", "", time_units))  # Extract the reference date
#       
#       results <- data.frame(
#         date = reference_date + time,
#         value = results
#       )
#       
#       names(results)[2] <- param 
#       
#       print(paste0("finished ",n," / ",length(year)))
#       
#       results_rbind <- rbind(results_rbind, results)
#     }
#     
#     results_param[[paste0(s)]] <- results_rbind
#     
#   }
#   
#   
#   
#   #### repeat the same for the whole upstream catchment for Kienstock station ----
#   
#   results_rbind <- NULL
#   
#   for (n in year) {
#     
#     file <- paste0("SPARTACUS2-DAILY_",param,"_",n)
#     
#     data_nc <- nc_open(paste0(path,param,"/",file,".nc"))
#     
#     lat <- ncvar_get(data_nc, "lat") %>% # Dimensions: (x, y) 
#       as.vector() 
#     lon <- ncvar_get(data_nc, "lon") %>%  # Dimensions: (x, y)
#       as.vector() 
#     
#     coords_spartacus <- data.frame(lat =lat, lon = lon)
#     
#     coords_spartacus <- st_as_sf(coords_spartacus, coords = c("lon", "lat"), crs = 4326)  # Start with WGS84 (EPSG:4326)
#     
#     
#     points_catchment <- st_join(coords_spartacus, catchment_Kienstock_shp, join = st_intersects, left = FALSE)
#     
#     # Get indices of selected points in original coords_spartacus
#     selected_indices <- which(st_coordinates(coords_spartacus)[,1] %in% st_coordinates(points_catchment)[,1] &
#                                 st_coordinates(coords_spartacus)[,2] %in% st_coordinates(points_catchment)[,2])
#     
#     # stations_spartacus <- coords_spartacus[indices,]
#     # # 
#     # stations_spartacus$ID <- coords_stations$hzb_nr
#     # # 
#     array <- ncvar_get(data_nc, param)  # Dimensions: (x, y, time)
#     
#     results <- list()
#     
#     # Loop through each time step
#     for (t in 1:dim(array)[3]) {  # Loop over the time dimension
#       # Extract the 2D slice for the current time step
#       spatial_grid <- array[, , t]
#       
#       # Use the indices to extract the values for the grid cells of interest
#       results[[t]] <- spatial_grid[selected_indices]
#     }
#     
#     results <- do.call(rbind, results)
#     
#     results <- results %>% as.data.frame()
#     
#     results <- rowMeans(results)
#     
#     time <- ncvar_get(data_nc, "time")
#     
#     time_units <- ncatt_get(data_nc, "time", "units")$value
#     
#     reference_date <- as.Date(sub("days since ", "", time_units))  # Extract the reference date
#     
#     results <- data.frame(
#       date = reference_date + time,
#       value = results
#     )
#     
#     names(results)[2] <- param 
#     
#     print(paste0("finished ",n," / ",length(year)))
#     
#     results_rbind <- rbind(results_rbind, results)
#   }
#   
#   results_param[["catchment_Kienstock"]] <- results_rbind
#   
#   results_final[[paste0(param)]] <- results_param
#   
# }
# 
# results_final_SA_TN <- results_final
# save(results_final_SA_TN, file = "data/climate/spartacus_clipped1ttt.RData")
# 
# load( file = "data/climate/spartacus_clipped.RData")
# 
# save <- results_final 




-----------------------------------------------------------------------
  
extract_TS_function <- function(parameter, 
                                  year, 
                                  buffer_size = 5000 #in m
                                ){
    
    results_rbind <- NULL
    
    data_nc <- nc_open(paste0("data/spartacus/SR_19610101_20221031.nc"))
    
    lat <- ncvar_get(data_nc, "lat") %>% as.vector()
    lon <- ncvar_get(data_nc, "lon") %>% as.vector()
    
    coords_spartacus <- data.frame(lat = lat, lon = lon)
    coords_spartacus <- st_as_sf(coords_spartacus, coords = c("lon", "lat"), crs = 4326)

#####----------    
    
    #buffer stations with 5 km radius
    stations_buffered <- coords_stations %>% 
      # filter(station_name == s) %>%
      st_transform(., crs = 3857)  # Project to meters
    stations_buffered <- st_buffer(stations_buffered, dist = buffer_size)  # 5 km buffer
    stations_buffered <- st_transform(stations_buffered, crs = 4326)  # Back to WGS84
    
    catchment_Kienstock_shp <- catchment_Kienstock_shp %>%
      mutate(
        station_name = "Kienstock_Catchment",
        catchment_size = NA,
        elevation = NA,
        hzb_nr = NA
      ) %>%
      select(station_name, catchment_size, elevation, hzb_nr, geometry)
    
    stations_buffered <- rbind(stations_buffered, catchment_Kienstock_shp)
    
    intersected <- st_join(coords_spartacus, stations_buffered, join = st_intersects, left = FALSE)
    intersected$index_in_spartacus <- match(st_coordinates(intersected) %>% as.data.frame() %>% do.call(paste, .),
                                            st_coordinates(coords_spartacus) %>% as.data.frame() %>% do.call(paste, .))
    index_list <- intersected %>%
      st_drop_geometry() %>%
      group_by(station_name) %>%
      summarise(indices = list(index_in_spartacus))
    
#--------
    time <- ncvar_get(data_nc, "time")
    time_units <- ncatt_get(data_nc, "time", "units")$value
    reference_date <- as.Date(sub("days since ", "", time_units))
    full_dates <- reference_date + time
    
    for (n in year) {
      
      time_indices <- which(format(full_dates, "%Y") == n)
      if (length(time_indices) == 0) next
      
      start_time <- min(time_indices)
      count_time <- length(time_indices)
      
      array <- ncvar_get(
        data_nc,
        varid = parameter,
        start = c(1, 1, start_time),
        count = c(-1, -1, count_time)  # read all x/y but only selected time range
      )
      
      results <- list()
      
      for (t in 1:dim(array)[3]) {
        spatial_grid <- array[, , t]
        
        values_per_station <- lapply(index_list$indices, function(idx) {
          mean(spatial_grid[idx], na.rm = TRUE)  # average over all grid cells per station
        })
        
        results[[t]] <- do.call(rbind, values_per_station) %>% t()
      }
      
      results <- do.call(rbind, results) %>% as.data.frame()
      colnames(results) <- index_list$station_name
      results$time <- full_dates[time_indices]
      
      print(paste0("finished ", (n-year[1]+1), " / ", length(year)))
      
      results_rbind <- rbind(results_rbind, results)
    }
    
    nc_close(data_nc)
    
    return(results_rbind)
  }

test <- extract_TS_function(parameter = "SR",
                            year = c(1961:2022),
                            buffer_size = 5000)

spartacus_SR <- test
save(spartacus_SR, file = "data/climate/spartacus_SR.RData")
save <- spartacus_SR
rm(spartacus_SR)
load( file = "data/climate/spartacus_SR.RData")





