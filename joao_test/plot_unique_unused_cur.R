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
library(mapview)
library(magick)
library(terra)
library(data.table)


#  2) READ gps ----

filepath_sp <- list.files("../proj_emis_routes/data/gps/cur/"
                          ,full.names = TRUE)
"../emission_routes/"
i = 
  
  gps_dt <- data.table::fread("../proj_emis_routes/data/gps/cur/1708.txt") %>%
  .[!is.na(departure_time),]

# gps_dt
# [1] "44331" "51954" "52085" "52135" "52145" "52149" "52251" "52253" "52254" "52256"
# [11] "52259" "52321" "52341" "52349" "52421" "52736" "52913" "52977" "53387" "54008"
# [21] "54010" "54023" "54038" "54060" "54211" "54224" "54699" "55383" "55567" "55786"
# [31] "56541" "56676" "57222" "57469" "57587" "58051" "58052" "58079" "58460" "58463"
# [41] "58466" "58487" "58634" "58642" "58993" "59000" "59336" "59395" "59454" "59474"
# [51] "59560" "59592" "59593" "59609" "59613" "59760" "59766" "59768" "59770" "59778"
# [61] "59802" "59912" "60000" "60298" "60344" "60416" "60535" "60610" "60769" "60831"
# [71] "61027" "61028" "61032" "61119" "61158" "61168" "61170" "61300" "61322" "61387"
# [81] "61515" "61668" "61692" "61797" "61801" "61802" "61953" "61954" "62000" "62074"
# [91] "62234" "62322" "62494" "62505" "62512" "62534" "62540" "62557" "62595" "62601"
# [101] "62727" "62755" "62839" "62840" "62854" "63062" "63063" "63100" "63105" "63134"
# [111] "63140" "63431" "63477" "63532" "63555" "63684" "63747" "63754" "63767" "63785"
# [121] "63834" "64000" "64060" "64061" "64103" "64293" "64351" "64550" "64723" "64886"
# [131] "65028" "65029" "65032" "65209" "65281" "65291" "65292" "68299" "68303" "68304"
# [141] "68435" "68553" "68633" "68634" "68659" "68769" "68975" "69021" "69085" "69170"
# [151] "69201" "69346" "69365" "69374" "69384" "69426" "69457" "69474" "69478" "69484"
# [161] "69509" "69634" "69647" "69832" "70076" "70078" "70079" "70153" "70155" "70159"
# [171] "70172" "70203" "70294" "70334" "70358" "70385" "70389" "70390" "70417" "70590"
# [181] "70620" "70628" "70629" "70665" "70688" "70772" "70774" "70775" "70785" "70786"
# [191] "70789" "70808" "70812" "70829" "70842"

#gps <- gps_dt[shape_id %in% shape_inter1[1:10]]
#readr::write_rds(gps,"sample_sp.rds")
# 
#gps <- readr::read_rds(filepath_sp[6])
#gps <- readr::read_rds("45818.rds")
# gps <- readr::read_rds("sample_sp.rds")
#gps <- readr::read_rds("bra_spo//51007.rds")
# deal with midnight trips
tmp_gps <- data.table::copy(gps_dt) %>%
  .[!is.na(departure_time),] %>%
  .[,time := data.table::as.ITime(departure_time) %>% as.numeric()] 

#midnight_trips <- tmp_gps[diff<0]$trip_number %>% unique()

tmp_gps <- data.table::copy(tmp_gps) %>%
  .[,shape_pt_lon_end := data.table::shift(shape_pt_lon,-1,NA), by = shape_id] %>%
  .[,shape_pt_lat_end := data.table::shift(shape_pt_lat,-1,NA), by = shape_id]

tmp_stops <- data.table::copy(tmp_gps) %>%
  .[!is.na(cumtime) & !is.na(stop_id) & !is.na(departure_time),] %>%  
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
# sf::st_coordinates() %>% 
# data.table::as.data.table() %>% 
# data.table::setnames(.,c("X","Y"),c("x","z")) %>% 
# .[,y := 5] %>% 
# .[,c("x","y","z")]

tmp_line <- data.table::copy(tmp_gps) %>%
  #.[trip_number == 1,] %>%
  .[!is.na(cumtime) & !is.na(stop_id) & !is.na(departure_time),] %>%
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

elev_img <- raster::raster("data/topografia3_cur.tif")

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
        fov = 70, zoom = 0.85, theta = 45, phi = 35
        ,  max_error = 0.001, verbose = TRUE) 


scale_altitude <- 1
tmp_gps[, new_scale_altitude := ( time - min(time) ) * scale_altitude]
# tmp_gps$new_scale_altitude <- (tmp_gps$time)

scale_color_shape_id <- viridis::viridis(n = 3)
unique_shape_id <- unique(tmp_stops$shape_id)
for(i in seq_along(unique_shape_id)){# i = unique(tmp_gps$shape_id)[1]
  
  
  
  rayshader::render_path(extent = raster::extent(elev_img)
                         , lat = tmp_gps[shape_id == unique_shape_id[i],]$shape_pt_lat
                         , long = tmp_gps[shape_id == unique_shape_id[i],]$shape_pt_lon
                         , altitude = tmp_gps[shape_id == unique_shape_id[i],]$new_scale_altitude,
                         zscale = 100, linewidth = 3
                         , clear_previous = T
                         , color = "black")
}
  #, color = scale_color_shape_id[i]
### 2 downrender
for(i in seq_along(unique_shape_id)){# i = unique(tmp_gps$shape_id)[1]
  
  
  rayshader::render_path(extent = raster::extent(elev_img)
                         , lat = tmp_gps[shape_id == unique_shape_id[i],]$shape_pt_lat
                         , long = tmp_gps[shape_id == unique_shape_id[i],]$shape_pt_lon
                         , altitude = 150,
                         zscale = 100, linewidth = 1
                         , clear_previous = F
                         , color = "black"
                         #, color = scale_color_shape_id[i]
  )
}
tmp_stops_id <- data.table::copy(tmp_stops)[shape_id %in% unique(tmp_gps$shape_id)]
tmp_stops_id[, new_scale_altitude := ( time - min(time) ) * scale_altitude]
tmp_stops_id[,N := .N,by = stop_id]
#  tmp_stops_id$new_scale_altitude <- (tmp_stops_id$time)

rayshader::render_points(extent = raster::extent(elev_img),
                         lat = tmp_stops_id$Y, long = tmp_stops_id$X,
                         altitude = tmp_stops_id$new_scale_altitude,
                         size = 6.5, zscale = 100,
                         clear_previous = TRUE, color = "red")


unique_shape_id <- unique(tmp_stops$shape_id)
scale_color_shape_id <- viridis::viridis(n = 3)
for(i in seq_along(unique_shape_id)){
  
  tmp_stops1 <- data.table::copy(tmp_stops)[, new_scale_altitude := ( time - 0 ) * scale_altitude]
  tmp_stops1 <- tmp_stops1[shape_id == unique_shape_id[i],]
  #tmp_stops1[, new_scale_altitude := ( time )]
  
  
  tmp_stops2 <- data.table::copy(tmp_stops1)[shape_id == unique_shape_id[i],]
  
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




# render label



first_point <- tmp_gps[1, c("shape_pt_lat","shape_pt_lon")] %>% t() %>% as.vector()
first_stopid <- tmp_gps[1,stop_id]

tmp_stops1 <- data.table::copy(tmp_stops) %>% 
  .[, new_scale_altitude := ( time - 0 ) * scale_altitude] %>% 
  .[stop_id == first_stopid,] %>% 
  .[,new_trip_id := .GRP,by = trip_number] %>% 
  .[new_trip_id %in% round(seq(1,max(new_trip_id),length.out = 7))] %>% 
  .[,text_hour := as.ITime(departure_time) %>% data.table::hour()] %>% 
  .[,text_min := as.ITime(departure_time) %>% data.table::minute()] %>% 
  .[,text_min := ifelse(nchar(text_min)==1,paste0("0",text_min),text_min)] %>% 
  .[,text_plot := sprintf('%s:%s',text_hour,text_min)]

tmp_stops1

rayshader::render_label(heightmap = elev_img
                        ,lat = tmp_stops1[,Y]
                        , long = tmp_stops1[,X]
                        , altitude = tmp_stops1[,new_scale_altitude]
                        , zscale = 100
                        , linewidth = 2
                        , adjustvec = c(-0.85,0.0)
                        , extent = attr(elev_img, "extent")
                        , fonttype = "standard"
                        , text = tmp_stops1[,text_plot]
                        , clear_previous = T)




dir.create("snaps")
rayshader::render_snapshot(filename = "snaps/11.png"
                           ,width = 2000
                           ,height = 2000)
