# Load ----

rm(list=ls())
gc(reset = TRUE)

install.packages('easypackages')
easypackages::packages('geobr'
                       , 'magick'
                       , 'gtfs2gps'
                       , 'data.table'
                       , 'sf'
                       , 'mapview'
                       , 'magrittr'
                       , 'dplyr'
                       , 'ggnewscale'
                       , 'ggplot2'
                       , 'rayshader'
                       , 'rayrender'
                       , 'rayimage'
                       , 'ggmap'
                       , 'raster'
                       , 'magick'
                       , 'httr')


#  READ gps ----

sp_gtfs <- gtfstools::read_gtfs("data/gtfs_spo_emtu_2019-06_423032_ida.zip")

# sunday
tmp_gtfs <- gtfstools::filter_by_weekday( sp_gtfs,"sunday")
sp_gps <- gtfs2gps::gtfs2gps(tmp_gtfs)

# define time windown
time_start = "05:00:00"
time_end = "10:00:00"


# check start / end time
gps_dt <- rbind(sp_gps[,day := "sunday"]) %>% 
  .[!is.na(timestamp)] %>% 
  .[,timestamp := data.table::as.ITime(timestamp)] %>% 
  .[,timestart := timestamp[1],by = .(trip_number,day)] %>% 
  .[,timeend := timestamp[.N],by = .(trip_number,day)] %>% 
  .[timestart >= as.ITime(time_start) & timeend <= as.ITime(time_end),] %>% 
  .[timeend > timestart,]

gps_dt[,.N,by=day]

# check GPS connections
tmp_gps <- data.table::copy(gps_dt) %>%
  .[,time := as.numeric(timestamp)]  %>%
  .[,shape_pt_lon_end := data.table::shift(shape_pt_lon,-1,NA), by = shape_id] %>%
  .[,shape_pt_lat_end := data.table::shift(shape_pt_lat,-1,NA), by = shape_id]

# create stops
tmp_stops <- data.table::copy(tmp_gps) %>%
  .[!is.na(cumtime) & !is.na(stop_id),] %>%  
  .[,time := as.numeric(timestamp)]  %>%
  .[,altitude := 100 * time/max(time)] %>% 
  data.table::setnames(.,old = c("shape_pt_lon","shape_pt_lat")
                       , new = c("X","Y"))

# View stops
view_tmp_stops <- data.table::copy(tmp_stops) %>% 
  sfheaders::sf_multipoint(.,x = "X"
                           ,  y = "Y"
                           , multipoint_id = "shape_id") %>%
  sf::st_set_crs(4326) %>%
  sf::st_transform(32723) %>%
  mapview()


# Create line
tmp_line <- data.table::copy(tmp_gps) %>%
  .[!is.na(cumtime) & !is.na(stop_id) & !is.na(timestamp),] %>%
  sfheaders::sf_linestring(obj = .
                           , x = "shape_pt_lon"
                           , y = "shape_pt_lat"
                           , linestring_id = "shape_id"
                           , keep = TRUE) %>%
  sf::st_set_crs(4326)

# bounding box
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

# download TILE ------

osm_bbox = tmp_gps_bbox %>% 
  raster::extent() %>%
  as.vector() %>% 
  .[c(1,3,2,4)]

view_osm_bbox <- sf::st_bbox(tmp_gps_bbox) %>% 
  mapview()

view_osm_bbox
# read tile -----
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

# produce gg base_map ----

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

# rayshader plot -------------
# 

rgl::clear3d()

plot_gg(list_plot, height = nrow(base_map)/200
        , width = ncol(base_map)/200, scale = 100
        , raytrace = FALSE, windowsize = c(1200, 1200),
        fov = 70.000000, zoom = 0.52 
        , theta = 90, phi = 6
        ,  max_error = 0.001, verbose = TRUE) 

#  rayshader::render_camera()

# create scaling factors
scale_altitude <- 5
tmp_gps1 <- data.table::copy(tmp_gps)
tmp_gps1[, new_scale_altitude := ( time - min(time)) * scale_altitude]

# create colors
scale_color_shape_id <- viridis::viridis(n = 3)
unique_shape_id <- unique(tmp_stops$shape_id)

# add multiple trips
for(i in seq_along(unique_shape_id)){# i = unique(tmp_gps$shape_id)[1]
  
  unique_trip_id <- unique(tmp_gps1[shape_id == unique_shape_id[i]]$trip_number)
  
  for(j in unique_trip_id){ # j = unique_trip_id[1]
    
    rayshader::render_path(extent = raster::extent(tmp_gps_bbox)
                           , lat = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lat
                           , long = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$shape_pt_lon
                           , altitude = tmp_gps1[trip_number == j & shape_id == unique_shape_id[i],]$new_scale_altitude
                           , zscale = 100, linewidth = 2
                           , clear_previous = F
                           #, color = "black"
                           , color = scale_color_shape_id[i]
    )
    
  }
}

### add shadow of shape in map

for(i in seq_along(unique_shape_id)){# i = unique(tmp_gps1$shape_id)[1]
  
  unique_trip_id <- unique(tmp_gps1[shape_id == unique_shape_id[i]]$trip_number)
  
  for(j in unique_trip_id){ # j = unique_trip_id[1]
    
    rayshader::render_path(extent = raster::extent(tmp_gps_bbox)
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

# add stops

tmp_stops_id <- data.table::copy(tmp_stops) %>% 
  .[shape_id %in% unique(tmp_gps1$shape_id)] %>% 
  .[, new_scale_altitude := ( time - min(time)) * scale_altitude] 

tmp_stops_id[,N := .N,by = stop_id]

rayshader::render_points(extent = raster::extent(tmp_gps_bbox),
                         lat = tmp_stops_id$Y, long = tmp_stops_id$X,
                         altitude = tmp_stops_id$new_scale_altitude,
                         size = 3.5, zscale = 100,
                         clear_previous = TRUE, color = "red")

# vertical vertical lines 
unique_shape_id <- unique(tmp_stops$shape_id)
scale_color_shape_id <- viridis::viridis(n = 3)

for(i in seq_along(unique_shape_id)){ # i  = 1
  
  tmp_stops1 <- data.table::copy(tmp_stops) 
  tmp_stops1 <- tmp_stops1[shape_id == unique_shape_id[i],]
  tmp_stops1[, new_scale_altitude := ( time - min(time)) * scale_altitude]
  
  for(j in tail(tmp_stops1$trip_number,1)){ #  j = tail(tmp_stops1$trip_number,1)
    
    tmp_stops2 <- data.table::copy(tmp_stops1) %>% 
      .[shape_id == unique_shape_id[i] & trip_number == j & 
          as.numeric(dist) != 0,]
    
    for(k in 1:uniqueN(tmp_stops2$stop_id)){ # k = 1
      
      rayshader::render_points(extent = raster::extent(tmp_gps_bbox),
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

# add labels

tmp_stops1 <- data.table::copy(tmp_stops) %>% 
  .[, new_scale_altitude := ( time - min(time) ) * scale_altitude] %>% 
  .[,text_hour := as.ITime(timestamp) %>% data.table::hour()] %>% 
  .[,text_min := as.ITime(timestamp) %>% data.table::minute()] %>% 
  .[,text_min := ifelse(nchar(text_min)==1,paste0("0",text_min),text_min)] %>% 
  .[,text_plot := sprintf('%s:%s',text_hour,text_min)]

tmp_stops1

# get artificial heightmap to add @ render_label function
elev_matrix <- raster::raster(nrows=808, ncols=964)
values(elev_matrix) <- 0
raster::extent(elev_matrix) <- raster::extent(tmp_gps_bbox)

rayshader::render_label(heightmap = elev_matrix
                        ,lat = tmp_stops1[,.SD[1],by = trip_number]$Y
                        , long = tmp_stops1[,.SD[1],by = trip_number]$X
                        , altitude = tmp_stops1[,.SD[1],by = trip_number]$new_scale_altitude
                        , zscale = 100
                        , textsize = 2.5
                        , linewidth = 3
                        , adjustvec = c(1.9,-0.00)
                        , extent = attr(elev_matrix, "extent")
                        , fonttype = "standard"
                        , text = tmp_stops1[,.SD[1],by = trip_number]$text_plot
                        , clear_previous = T)
rayshader::render_label(heightmap = elev_matrix
                        ,lat = tmp_stops1[,.SD[.N],by = trip_number]$Y
                        , long = tmp_stops1[,.SD[.N],by = trip_number]$X
                        , altitude = tmp_stops1[,.SD[.N],by = trip_number]$new_scale_altitude
                        , zscale = 100
                        , textsize = 2.5
                        , linewidth = 0
                        , alpha = 0
                        , adjustvec = -c(1.05,0.25)
                        , extent = attr(elev_matrix, "extent")
                        , fonttype = "standard"
                        , text = tmp_stops1[,.SD[.N],by = trip_number]$text_plot
                        , clear_previous = F)

# 6) saving----
dir.create("figures")
rayshader::render_snapshot(filename = "figures/12_monday.png"
                           ,width = 1000
                           ,height = 2000)
