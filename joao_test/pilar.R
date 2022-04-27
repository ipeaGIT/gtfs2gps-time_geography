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

# sp_gtfs <- gtfs2gps::read_gtfs("data/gtfs_spo_emtu_2019-10.zip") %>%
#   gtfs2gps::filter_by_shape_id("423032_ida")
# gtfs2gps::write_gtfs(sp_gtfs,"data/gtfs_spo_emtu_2019-10_423032_ida.zip")

sp <- sf::read_sf("../../Downloads/Pilarzinho horizontal.kml") %>% 
  sf::st_cast('POINT') %>% sf::st_sf()

sp
sp_latlon <- sf::st_coordinates(sp)
sp$shape_pt_lon <- as.numeric(sp_latlon[,1])
sp$shape_pt_lat <- as.numeric(sp_latlon[,2])
sp$altitute <- as.numeric(sp_latlon[,3])

tmp_gps_bbox <- sp %>%
  sf::st_transform(4326) %>%
  sf::st_transform(32722) %>%
  sf::st_buffer(x = .,dist = 6000) %>%
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

# 3) READ topography ----

# 3.1) crop ----

elev_img <- raster::raster("../../Downloads/topografia_cur.tif")
elev_img <- raster::crop(x = elev_img,y = raster::extent(tmp_gps_bbox))
# elev_zoom_mat <- rayshader::raster_to_matrix(elev_img)

elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img)), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

#elev_img <- elev_img - elev_img
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
        fov = 90.22856, zoom = 0.50092 
        , theta = 35.30797, phi = 52.19693
        ,  max_error = 0.001, verbose = TRUE) 

rayshader::render_camera()

tmp_gps1 <- data.table::copy(sp)


rayshader::render_path(extent = raster::extent(elev_img)
                       , lat = tmp_gps1$shape_pt_lat
                       , long = tmp_gps1$shape_pt_lon
                       , altitude = tmp_gps1$altitute+ c(1:48)*100
                       , zscale = 100
                       , linewidth = 2
                       , clear_previous = T
                       #, color = "black"
                       , color = "black")

### 2 downrender

rayshader::render_path(extent = raster::extent(elev_img)
                       , lat = tmp_gps1$shape_pt_lat
                       , long = tmp_gps1$shape_pt_lon
                       , altitude = 150
                       , zscale = 100
                       , linewidth = 1
                       , clear_previous = F
                       , color = "black")


#tmp_stops_id[,N := .N,by = stop_id]
#  tmp_stops_id$new_scale_altitude <- (tmp_stops_id$time)

rayshader::render_points(extent = raster::extent(elev_img),
                         lat = tmp_gps1$shape_pt_lat, long = tmp_gps1$shape_pt_lon,
                         altitude = 150,
                         size = 2.5, zscale = 100,
                         clear_previous = TRUE, color = "black")


# render label

rayshader::render_label(heightmap = elev_img
                        ,lat = tmp_gps1$shape_pt_lat[1]
                        , long =  tmp_gps1$shape_pt_lon[1]
                        , altitude =  tmp_gps1$altitute[1]
                        , zscale = 100
                        , textsize = 2.5
                        , linewidth = 0
                        , adjustvec = c(1.5,-0.85)
                        , extent = attr(elev_img, "extent")
                        , fonttype = "standard"
                        , text = "Início"
                        , clear_previous = T)
rayshader::render_label(heightmap = elev_img
                        ,lat = tmp_gps1$shape_pt_lat %>% tail(1)
                        , long =  tmp_gps1$shape_pt_lon %>% tail(1)
                        , altitude =  tmp_gps1$altitute %>% tail(1)
                        , zscale = 100
                        , textsize = 2.5
                        , linewidth = 0
                        , adjustvec = c(-2,-4.9)
                        , extent = attr(elev_img, "extent")
                        , fonttype = "standard"
                        , text = "Fim"
                        , clear_previous = F)

# 6) saving----
dir.create("snaps")
rayshader::render_snapshot(filename = "snaps/plot_pilazinho.png"
                           ,width = 1000
                           ,height = 2000)
