# 1) Load ----

rm(list=ls())
gc(reset = TRUE)
library(rayshader)
library(rayrender)
library(rayimage)
library(magrittr)
library(sf)
library(ggplot2)
library(ggmap)
library(raster)
library(gtfs2gps) # devtools::install_github("ipeaGIT/gtfs2gps")
library(mapview)
library(magick)
library(data.table)


#  2) READ gps ----



sp_gtfs <- gtfs2gps::read_gtfs("data/gtfs_spo_emtu_2019-10_423032_ida.zip")

sp_gtfs$trips[,.N,by =.(shape_id,service_id)]
sp_gtfs$calendar
sp_gtfs$calendar_dates

tmp_gtfs <- sp_gtfs %>% 
  gtfs2gps::filter_by_day("monday")

tmp_gtfs$calendar
tmp_gtfs$trips <- tmp_gtfs$trips[service_id == 0 ]

sp_gps_1 <- gtfs2gps::gtfs2gps(tmp_gtfs)


# sunday
tmp_gtfs <- sp_gtfs %>% gtfs2gps::filter_by_day("sunday")
tmp_gtfs$calendar
tmp_gtfs$trips <- tmp_gtfs$trips[service_id == 2]

sp_gps_2 <- gtfs2gps::gtfs2gps(tmp_gtfs)


time_start = "05:00:00"
time_end = "10:00:00"
myday <- "monday"

gps_dt <- rbind(sp_gps_1[,day := "monday"], sp_gps_2[,day := "sunday"]) %>% 
  .[day == myday,] %>% 
  .[!is.na(timestamp)] %>% 
  .[,timestamp := data.table::as.ITime(timestamp)] %>% 
  .[,timestart := timestamp[1],by = .(trip_number,day)] %>% 
  .[,timeend := timestamp[.N],by = .(trip_number,day)] %>% 
  .[timestart >= as.ITime(time_start) & timeend <= as.ITime(time_end),] %>% 
  .[timeend > timestart,]

gps_dt[,.N,by=day]
# gps_dt
# 
# gps_line <- sfheaders::sf_linestring(obj = gps_dt,x = "shape_pt_lon",y = "shape_pt_lat"
#                                      ,linestring_id = "trip_number",keep = FALSE) %>%
#   sf::st_set_crs(4326)
# 
# mapview(gps_line$geometry)
# 
# gps_int <- sf::st_intersects(gps_line,sparse = FALSE)
# 
# shape_inter1 <- unique(gps_line$shape_id)[gps_int[1,]]
# shape_path1 <- sprintf("bra_spo/%s.rds",shape_inter1)

#gps <- gps_dt[shape_id %in% shape_inter1[1:10]]
#readr::write_rds(gps,"sample_sp.rds")
# 
#gps <- readr::read_rds(filepath_sp[6])
#gps <- readr::read_rds("45818.rds")
# gps <- readr::read_rds("sample_sp.rds")
#gps <- readr::read_rds("bra_spo//51007.rds")
# deal with midnight trips

tmp_gps <- data.table::copy(gps_dt) %>%
  .[,time := as.numeric(timestamp)]  %>%
  .[,shape_pt_lon_end := data.table::shift(shape_pt_lon,-1,NA), by = shape_id] %>%
  .[,shape_pt_lat_end := data.table::shift(shape_pt_lat,-1,NA), by = shape_id]

tmp_stops <- data.table::copy(tmp_gps) %>%
  .[!is.na(cumtime) & !is.na(stop_id),] %>%  
  .[,time := as.numeric(timestamp)]  %>%
  .[,altitude := 100 * time/max(time)] %>% 
  data.table::setnames(.,old = c("shape_pt_lon","shape_pt_lat")
                       , new = c("X","Y"))

view_tmp_stops <- data.table::copy(tmp_stops) %>% 
  sfheaders::sf_multipoint(.,x = "X"
                           ,  y = "Y"
                           , multipoint_id = "shape_id") %>%
  sf::st_set_crs(4326) %>%
  sf::st_transform(32723) %>%
  mapview()


tmp_line <- data.table::copy(tmp_gps) %>%
  .[!is.na(cumtime) & !is.na(stop_id) & !is.na(timestamp),] %>%
  sfheaders::sf_linestring(obj = .
                           , x = "shape_pt_lon"
                           , y = "shape_pt_lat"
                           , linestring_id = "shape_id"
                           , keep = TRUE) %>%
  sf::st_set_crs(4326)


tmp_gps_bbox <- tmp_line %>%
  sf::st_transform(4326) %>%
  sf::st_transform(32723) %>%
  sf::st_buffer(x = .,dist = 8000) %>%
  sf::st_transform(4326) %>%
  sf::st_bbox() %>%
  as.numeric() %>%
  data.frame("X" = c(.[1],.[1],.[3],.[3])
             ,"Y" = c(.[2],.[4],.[4],.[2])) %>%
  sfheaders::sf_polygon(.,x = "X",y = "Y") %>%
  sf::st_set_crs(4326)



# 4) download TILE ------

osm_bbox = tmp_gps_bbox %>% raster::extent() %>%
  as.vector() %>% .[c(1,3,2,4)]


view_osm_bbox <- sf::st_bbox(tmp_gps_bbox) %>% 
  mapview()

# * read tile -----
ggmap::file_drawer()
dir(file_drawer())
base_map <- ggmap::get_stamenmap(bbox = c(left = osm_bbox[1],
                                          bottom = osm_bbox[2],
                                          right = osm_bbox[3],
                                          top = osm_bbox[4]),
                                 maptype = "terrain",
                                 crop = TRUE,
                                 zoom = 12)

ggmap::file_drawer()


view_osm_bbox+view_tmp_stops
# 3) READ topography ----

# 3.1) crop ----

elev_img <- raster::raster("data/topografia3_spo.tif")
elev_img <- raster::crop(x = elev_img,y = raster::extent(tmp_gps_bbox))
# elev_zoom_mat <- rayshader::raster_to_matrix(elev_img)

elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img)), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

elev_img <- elev_img - elev_img
# create attribute

# 4) produce gg base_map ----

my_plot_trans <- matrix(adjustcolor(base_map,
                                    alpha.f = 0.01),
                        nrow = nrow(base_map))
attributes(my_plot_trans) <-  attributes(base_map)


point_plot <- ggmap(base_map) + 
  theme_nothing() +
  labs(x = NULL,y = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(plot.margin=unit(-c(1,1,1,1), "mm"))

together_plot <- ggmap(my_plot_trans)+  
  theme_nothing() +
  labs(x = NULL,y = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(plot.margin=unit(-c(1,1,1,1), "mm"))

list_plot <- list(point_plot,together_plot)

# 5) try plotting -------------
# 

rgl::clear3d()

plot_gg(list_plot, height = nrow(base_map)/200
        , width = ncol(base_map)/200, scale = 100
        , raytrace = FALSE, windowsize = c(1200, 1200),
        fov = 70.000000, zoom = 0.525966 
        , theta = 269.911531, phi = 11.972998
        ,  max_error = 0.001, verbose = TRUE) 


scale_altitude <- 5
tmp_gps1 <- data.table::copy(tmp_gps)
tmp_gps1[, new_scale_altitude := ( time - min(time)) * scale_altitude]

# tmp_gps$new_scale_altitude <- (tmp_gps$time)

scale_color_shape_id <- viridis::viridis(n = 3)
unique_shape_id <- unique(tmp_stops$shape_id)
for(i in seq_along(unique_shape_id)){# i = unique(tmp_gps$shape_id)[1]
  
  unique_trip_id <- unique(tmp_gps1[shape_id == unique_shape_id[i]]$trip_number)
  
  for(j in unique_trip_id){ # j = unique_trip_id[1]
    
    rayshader::render_path(extent = raster::extent(elev_img)
                           , lat = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lat
                           , long = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lon
                           , altitude = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$new_scale_altitude,
                           zscale = 100, linewidth = 2
                           , clear_previous = F
                           #, color = "black"
                           , color = scale_color_shape_id[i]
    )
    
  }
}
### 2 downrender
for(i in seq_along(unique_shape_id)){# i = unique(tmp_gps1$shape_id)[1]
  
  unique_trip_id <- unique(tmp_gps1[shape_id == unique_shape_id[i]]$trip_number)
  
  for(j in unique_trip_id){ # j = unique_trip_id[1]
    
    rayshader::render_path(extent = raster::extent(elev_img)
                           , lat = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lat
                           , long = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lon
                           , altitude = 150
                           , zscale = 100
                           , linewidth = 1
                           , clear_previous = F
                           , color = "black"
                           #, color = scale_color_shape_id[i]
    )
    
  }
}
# tmp_stops[day == "monday", new_scale_altitude := ( time - min(time) ) * scale_altitude]
tmp_stops_id <- data.table::copy(tmp_stops) %>% 
  .[shape_id %in% unique(tmp_gps1$shape_id)] %>% 
  .[, new_scale_altitude := ( time - min(time)) * scale_altitude] 

tmp_stops_id[,N := .N,by = stop_id]
#  tmp_stops_id$new_scale_altitude <- (tmp_stops_id$time)

rayshader::render_points(extent = raster::extent(elev_img),
                         lat = tmp_stops_id$Y, long = tmp_stops_id$X,
                         altitude = tmp_stops_id$new_scale_altitude,
                         size = 3.5, zscale = 100,
                         clear_previous = TRUE, color = "red")


unique_shape_id <- unique(tmp_stops$shape_id)
scale_color_shape_id <- viridis::viridis(n = 3)
for(i in seq_along(unique_shape_id)){
  
  tmp_stops1 <- data.table::copy(tmp_stops) 
  tmp_stops1 <- tmp_stops1[shape_id == unique_shape_id[i],]
  tmp_stops1[, new_scale_altitude := ( time - min(time)) * scale_altitude]
  
  for(j in tail(tmp_stops1$trip_number,1)){ #  j = unique(tmp_stops1$trip_number)[1]
    
    tmp_stops2 <- data.table::copy(tmp_stops1) %>% 
      .[shape_id == unique_shape_id[i] & trip_number == j,]
    
    for(k in 1:uniqueN(tmp_stops2$stop_id)){
      rayshader::render_points(extent = raster::extent(elev_img),
                               lat  = tmp_stops2[,Y][k],
                               long = tmp_stops2[,X][k],
                               altitude = 1:(tmp_stops2[,new_scale_altitude][k]),
                               size = 0.5, zscale = 100,
                               clear_previous = F
                               #, color = scale_color_shape_id[i]
                               , color = "black"
      )
    }
  }
}




# render label


# 
# first_point <- tmp_gps[1, c("shape_pt_lat","shape_pt_lon")] %>% t() %>% as.vector()
# first_stopid <- tmp_gps[1,stop_id]

tmp_stops1 <- data.table::copy(tmp_stops) %>% 
  .[, new_scale_altitude := ( time - min(time) ) * scale_altitude] %>% 
  #.[new_trip_id %in% round(seq(1,max(new_trip_id),length.out = 7))] %>% 
  .[,text_hour := as.ITime(timestamp) %>% data.table::hour()] %>% 
  .[,text_min := as.ITime(timestamp) %>% data.table::minute()] %>% 
  .[,text_min := ifelse(nchar(text_min)==1,paste0("0",text_min),text_min)] %>% 
  .[,text_plot := sprintf('%s:%s',text_hour,text_min)]

tmp_stops1

rayshader::render_label(heightmap = elev_img
                        ,lat = tmp_stops1[,.SD[1],by = trip_number]$Y
                        , long = tmp_stops1[,.SD[1],by = trip_number]$X
                        , altitude = tmp_stops1[,.SD[1],by = trip_number]$new_scale_altitude
                        , zscale = 100
                        , textsize = 1.5
                        , linewidth = 3
                        , adjustvec = -c(3.5,0.5)
                        , extent = attr(elev_img, "extent")
                        , fonttype = "standard"
                        , text = tmp_stops1[,.SD[1],by = trip_number]$text_plot
                        , clear_previous = T)
rayshader::render_label(heightmap = elev_img
                        ,lat = tmp_stops1[,.SD[.N],by = trip_number]$Y
                        , long = tmp_stops1[,.SD[.N],by = trip_number]$X
                        , altitude = tmp_stops1[,.SD[.N],by = trip_number]$new_scale_altitude
                        , zscale = 100
                        , textsize = 1.5
                        , linewidth = 0
                        , alpha = 0
                        , adjustvec = c(4.5,0.5)
                        , extent = attr(elev_img, "extent")
                        , fonttype = "standard"
                        , text = tmp_stops1[,.SD[.N],by = trip_number]$text_plot
                        , clear_previous = F)

# 6) saving----
dir.create("snaps")
rayshader::render_snapshot(filename = "snaps/12_monday.png"
                           ,width = 2000
                           ,height = 2000)
