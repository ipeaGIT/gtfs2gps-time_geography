# create data-----

rm(list=ls())
gc(reset = TRUE)
library(magrittr)
library(sf)
library(ggplot2)
library(gtfs2gps) # devtools::install_github("ipeaGIT/gtfs2gps")
library(mapview)
library(magick)
library(data.table)

# 1) Download Sao Paulo shapefile ------
spo_bound <- geobr::read_municipality(3550308,simplified = FALSE)
spo_bound <- sf::st_transform(spo_bound,4326)
readr::write_rds(spo_bound,"data/spo_bound.rds")

# 1) PREP DATA FOR SPATIAL PLOTS ------
# saving gtfs monday
sp_gtfs_raw <- gtfstools::read_gtfs("L://Proj_acess_oport/data-raw/gtfs/spo/2019/gtfs_spo_emtu_2019-10.zip")

# add shape_id info on stop_times
sp_gtfs_raw$stop_times[sp_gtfs_raw$trips,on = "trip_id",shape_id := i.shape_id]

sp_gtfs <- gtfstools::filter_by_shape_id(sp_gtfs_raw,"423032_ida")

gtfs2gps::write_gtfs(sp_gtfs,"data/gtfs_spo_emtu_2019-10_423032_ida.zip")

# saving file '51007'
tmp_shape_id <- sp_gtfs_raw %>% 
  gtfstools::filter_by_shape_id("423032_ida") %>% 
  gtfs2gps::filter_single_trip() 

tmp_shape_id$stop_times[,arrival_time := data.table::as.ITime(arrival_time)]
tmp_shape_id$stop_times[,departure_time := data.table::as.ITime(departure_time)]

tmp_gps <- tmp_shape_id %>% 
  gtfs2gps::gtfs2gps() %>% 
  gtfs2gps::adjust_speed()

readr::write_rds(x = tmp_gps,"data/423032_ida.rds")

# finding intersections---------------------------

shape_id <- gtfstools::convert_shapes_to_sf(gtfs = sp_gtfs_raw
                                            ,shape_id = c("540298_ida","502070_ida"))
stop_id <- gtfstools::convert_stops_to_sf(gtfs = sp_gtfs_tmp)
mapview(shape_id["shape_id"])+mapview(stop_id)



sp_gtfs_tmp <- gtfstools::filter_by_shape_id(sp_gtfs_raw,c("540298_ida","502070_ida"))

# sp_gtfs_tmp$stop_times[sp_gtfs_tmp$trips,on = "trip_id",shape_id := i.shape_id]
# sp_gtfs_tmp$stop_times[stop_id %in% c(551274,503088)]
# stop_tmp
# [1] "469011"           "545222"           "549234"           "537234"           "543225"          
# [6] "550215"           "540259"           "544230"           "547252"           "548256"          
# [11] "542216"           "538237"           "551274"           "545250"           "549260"          
# [16] "548289"           "539254"           "542248"           "4611079534280704"
delta = 4
hour = 8

for(i in 1:59){ # i = 41
   init_time <- sprintf("%s:%s:30",hour,ifelse(nchar(i)==1,paste0("0",i),i))
   end_time <-  sprintf("%s:%s:30",hour,ifelse(nchar(i+delta)==1,paste0("0",i+delta),i+delta))
   tmp_time <- data.table::copy(sp_gtfs_tmp$stop_times) %>% 
      .[stop_id %in% c(551274,503088)] %>% 
      .[between(as.ITime(departure_time),as.ITime(init_time),as.ITime(end_time)),] 
   init_time
   end_time
   tmp_time
   print(sprintf("from '%s' to '%s' : %s",init_time,end_time,nrow(tmp_time)))
}

sp_gtfs_tmp$stop_times[,arrival_time := data.table::as.ITime(arrival_time)]
sp_gtfs_tmp$stop_times[,departure_time := data.table::as.ITime(departure_time)]

#sp_gtfs_tmp$stop_times %>% View()

sp_gtfs_tmp <- gtfstools::filter_by_trip_id(sp_gtfs_tmp,c("502070_ida_2_6","540298_ida_0_36"))
#sp_gtfs_tmp$stop_times %>% View()

gps_tmp <- gtfs2gps::gtfs2gps(sp_gtfs_tmp)
unique(gps_tmp$trip_number)
gps_tmp <- gtfs2gps::adjust_speed(gps_tmp)

readr::write_rds(x = gps_tmp,"data/emtu_intersection_gps.rds")

