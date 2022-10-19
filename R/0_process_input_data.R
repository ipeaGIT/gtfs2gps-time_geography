# create data-----

rm(list=ls())
gc(reset = TRUE)
install.packages('easypackages')
easypackages::packages('geobr', 'magick', 'gtfs2gps', 
                       'data.table', 'sf', 'mapview',
                       'magrittr', 'dplyr', 'ggnewscale',
                       'ggplot2', 'httr',)


# 1) Download Sao Paulo shapefile ------
spo_bound <- geobr::read_municipality(3550308, simplified = FALSE)
spo_bound <- sf::st_transform(spo_bound, 4326)
readr::write_rds(spo_bound,"data/spo_bound.rds")

# 1) PREP DATA FOR SPATIAL PLOTS ------
fix_link <- "https://github.com/ipeaGIT/gtfs2gps-time_geography/releases/download/data"
emtu_link <- sprintf("%s/gtfs_spo_emtu_2019-06.zip",fix_link)
sptrans_link <- sprintf("%s/gtfs_spo_sptrans_2019-06.zip",fix_link)
dir.create("data-raw/")

download.file(url = emtu_link
              ,destfile = "data-raw/gtfs_spo_emtu_2019-06.zip")


download.file(url = sptrans_link
              ,destfile = "data-raw/gtfs_spo_sptrans_2019-06.zip")

# 2) Adjust EMTU GTFS -----
sp_gtfs_raw <- gtfstools::read_gtfs("data-raw/gtfs_spo_emtu_2019-06.zip")

# add shape_id info on stop_times
sp_gtfs_raw$stop_times[sp_gtfs_raw$trips,on = "trip_id",shape_id := i.shape_id]

sp_gtfs <- gtfstools::filter_by_shape_id(sp_gtfs_raw,"423032_ida")

gtfs2gps::write_gtfs(sp_gtfs,"data/gtfs_spo_emtu_2019-06_423032_ida.zip")

# exporting temporal intersectins ---------------------------


sp_gtfs_tmp <- gtfstools::filter_by_shape_id(sp_gtfs_raw,c("540298_ida","502070_ida"))
sp_gtfs_tmp <- gtfstools::filter_by_trip_id(sp_gtfs_tmp,c("502070_ida_2_6","540298_ida_0_36"))

gps_tmp <- gtfs2gps::gtfs2gps(sp_gtfs_tmp)
gps_tmp <- gtfs2gps::adjust_speed(gps_tmp)

readr::write_rds(x = gps_tmp,"data/emtu_intersection_gps.rds")