# Load ----

rm(list=ls())
gc(reset = TRUE)

# install.packages('easypackages')
easypackages::packages('geobr'
                       , 'gtfs2gps'
                       , 'data.table'
                       , 'magrittr'
                       , 'ggplot2'
                       , 'rayshader'
                       , 'progressr'
                       , 'pbapply'
                       , 'aopdata')


# 1) GPS filter ------

# read spo boundary
spo_bound <- readr::read_rds("data/spo_bound.rds")

# emtu
emtu_path <- "./data-raw/gtfs_spo_emtu_2019-06.zip"
emtu_gtfs <- gtfstools::read_gtfs(path = emtu_path)
emtu_gtfs <- gtfstools::filter_by_weekday(emtu_gtfs,"wednesday")

# gtfs2gps 
dir.create("data/gps/")
dir.create("data/gps/emtu")

progressr::with_progress(
  gtfs2gps::gtfs2gps(gtfs_data = emtu_gtfs,
                     parallel = TRUE,
                     filepath = "data/gps/emtu/")
  )

# sptrans
sptr_path <- "./data-raw/gtfs_spo_sptrans_2019-06.zip"
sptr_gtfs <- gtfstools::read_gtfs(sptr_path)
sptr_gtfs <- gtfstools::frequencies_to_stop_times(sptr_gtfs)
sptr_gtfs <- gtfstools::filter_by_weekday(sptr_gtfs,"wednesday")

# gtfs2gps 
dir.create("data/gps/sptrans")
progressr::with_progress(
gtfs2gps::gtfs2gps(gtfs_data = sptr_gtfs,
                   parallel = TRUE,
                   filepath = "data/gps/sptrans/",
                   continue = T
                   )
  )




# 2) Rbind -----
## sptrans-----
sptrans_files <- list.files ("data/gps/sptrans/",full.names = TRUE)
sptrans_stops <- pblapply(sptrans_files,function(i){
  # i = sptrans_files[1]
  tmp <- data.table::fread(i, select = c('shape_id','trip_id','stop_id',
                                         'timestamp', 'dist',
                                         'shape_pt_lat', 'shape_pt_lon'))
  tmp <- tmp[!is.na(stop_id) & dist != 0]
  return(tmp)

  }) %>% data.table::rbindlist()

## emtu -----
emtu_files <- list.files ("data/gps/emtu/",full.names = TRUE)
emtu_stops <- pblapply(emtu_files,function(i){
  # i = emtu_files[1]
  tmp <- data.table::fread(i, select = c('shape_id','trip_id','stop_id',
                                         'timestamp', 'dist',
                                         'shape_pt_lat', 'shape_pt_lon'))
  tmp <- tmp[!is.na(stop_id) & dist != 0]
  return(tmp)
  
}) %>% data.table::rbindlist()


## Rbind GPS ----
emtu_stops[,source := "EMTU"]
sptrans_stops[,source := "SPTRANS"]

rbind_stops <- rbind(emtu_stops, sptrans_stops)
rbind_stops[, stop_id := as.character(stop_id)]

## Find bus stops inside SP -----

# total bus stop_ids 
uniqueN(rbind_stops$stop_id) 

unique_stops_sf <- sfheaders::sf_multipoint(
  obj = rbind_stops[,.SD[1],by = .(stop_id)]
  ,x = "shape_pt_lon"
  ,y = "shape_pt_lat"
  ,multipoint_id = "stop_id"
  ,keep = FALSE)

unique_stops_sf <- sf::st_set_crs(unique_stops_sf,4326)

tmp_id <- sf::st_within(x = unique_stops_sf
                        ,y = spo_bound
                        ,sparse = FALSE)

# total stops inside SP
sum(tmp_id) # 28878

unique_stops <- unique_stops_sf[which(tmp_id),]$stop_id
unique_stops <- as.character(unique_stops)

# apply filter
rbind_stops_v1 <- rbind_stops[stop_id %in% unique_stops,]

readr::write_rds(rbind_stops_v1,"data/rbind_stops_sp.rds",compress = "gz")

# 2) Freq by time -----
rm(list=ls())
gc(reset = TRUE)

rbind_stops <- readr::read_rds("data/rbind_stops_sp.rds")

# adjust time
rbind_stops[,time_to_sec := gtfstools:::cpp_time_to_seconds(timestamp)]
rbind_stops[,minu_time := round(time_to_sec/60,1)]


## add time classes ------
# 60 min
tp2 <- as.character(0:23)
tp2[nchar(tp2) == 1] <- paste0("0",tp2[nchar(tp2) == 1])

label_60min <- paste0(tp2,c(":00"))
rbind_stops[, time_60min := cut(minu_time
                                , breaks = seq(0,1440
                                               ,length.out = length(label_60min)+1)
                                , right = FALSE
                                , labels = label_60min)]
# 30 min
tp2 <- as.character(rep(0:23,each = 2))
tp2[nchar(tp2) == 1] <- paste0("0",tp2[nchar(tp2) == 1])

label_30min <- paste0(tp2,c(":00",":30"))
rbind_stops[, time_30min := cut(minu_time
                                , breaks = seq(0,1440
                                               ,length.out = length(label_30min)+1)
                                , right = FALSE
                                , labels = label_30min)]
# 15 min
tp2 <- as.character(rep(0:23,each = 4))
tp2[nchar(tp2) == 1] <- paste0("0",tp2[nchar(tp2) == 1])

label_15min <- paste0(tp2,c(":00",":15",":30",":45"))
rbind_stops[, time_15min := cut(minu_time
                                , breaks = seq(0,1440
                                               ,length.out = length(label_15min)+1)
                                , right = FALSE
                                , labels = label_15min)]

# 10 min
tp2 <- as.character(rep(0:23,each = 6))
tp2[nchar(tp2) == 1] <- paste0("0",tp2[nchar(tp2) == 1])

label_10min <- paste0(tp2,c(":00",":10",":20",":30",":40",":50"))
rbind_stops[, time_10min := cut(minu_time
                                , breaks = seq(0,1440
                                               ,length.out = length(label_10min)+1)
                                , right = FALSE
                                , labels = label_10min)]

# 5 min
tp1 <- as.character(seq(0,55,by = 5))
tp1[nchar(tp1) == 1] <- paste0("0",tp1[nchar(tp1) == 1])
tp2 <- as.character(rep(0:23,each = 12))
tp2[nchar(tp2) == 1] <- paste0("0",tp2[nchar(tp2) == 1])
label_05min <- paste0(tp2,":",tp1)

rbind_stops[, time_05min := cut(minu_time
                                , breaks = seq(0,1440
                                               ,length.out = length(label_05min)+1)
                                , right = FALSE
                                , labels = label_05min)]

rm(list = c("label_10min","label_15min","label_30min","label_60min"
            ,"label_05min","tp1","tp2"))

## calcula freq ----

rbind_stops[,N_60min := .N,by = .(stop_id,source,time_60min)]
rbind_stops[,N_30min := .N,by = .(stop_id,source,time_30min)]
rbind_stops[,N_15min := .N,by = .(stop_id,source,time_15min)]
rbind_stops[,N_10min := .N,by = .(stop_id,source,time_10min)]
rbind_stops[,N_05min := .N,by = .(stop_id,source,time_05min)]

rbind_stops <- data.table::melt.data.table(
  data = rbind_stops
  ,id.vars = c('stop_id','shape_pt_lat',  'shape_pt_lon','source')
  ,measure.vars = list(
    "time" = c('time_60min','time_30min','time_15min', 'time_10min', 'time_05min')
    ,"N" = c('N_60min','N_30min','N_15min', 'N_10min', 'N_05min')
  ))

rbind_stops[,time_interval := fcase(
  variable == 1,"60 min",
  variable == 2,"30 min",
  variable == 3,"15 min",
  variable == 4,"10 min",
  variable == 5,"05 min"  )]

#rbind_stops[,variable := NULL]

rbind_stops[1]
rbind_stops$time %>% unique() %>% sort()

# 3) stops to sf

stops_sf <- sfheaders::sf_point(obj = rbind_stops[,.SD[1],by = .(stop_id)]
                                ,x = "shape_pt_lon"
                                ,y = "shape_pt_lat"
                                ,keep = TRUE)
stops_sf <- sf::st_set_crs(stops_sf,4326)

# find hexagons
spo_hex <- aopdata::read_grid(city = "spo")

hex_id <- sf::st_within(x = stops_sf
                        ,y = spo_hex
                        ,sparse = TRUE
) %>% as.numeric()

# add to stops_sf
stops_sf$hex_id <- spo_hex$id_hex[hex_id]

setDT(stops_sf)

# combination of stop_id and hexagon

stops_sf <- stops_sf[,.SD,.SDcols = c("stop_id","hex_id")]


# 3) alocar info do hexagono ----

rbind_stops[stops_sf, on = "stop_id",hex_id := i.hex_id]

# soma por hexagono
rbind_stops <- rbind_stops[,list(N = sum(N)),by = .(hex_id,time,time_interval)]


# 4) aloca info de renda -----
spo_renda <- aopdata::read_landuse(city = "spo")


rbind_stops[spo_renda,on = c("hex_id" = "id_hex")
            ,":="(
              total_pop = P001
              ,avg_inc = R001
              ,quintil_inc = R002
              ,decil_ind = R003
            )]


# 5) aloca geometria

rbind_stops_sf <- data.table::merge.data.table(
  x = setDT(spo_hex)
  ,y = rbind_stops
  ,by.x = "id_hex"
  ,by.y = "hex_id"
)
rbind_stops_sf <- sf::st_as_sf(rbind_stops_sf)
rbind_stops_sf <- sf::st_set_crs(rbind_stops_sf,4326)

readr::write_rds(x = rbind_stops_sf
                 ,file = "data/hex_freq_sf.rds"
                 ,compress = "gz")




# 4.1) values for article -----------------------------

rm(list=ls())
gc(reset = TRUE)

rbind_stops <- readr::read_rds("data/hex_freq_sf.rds")
data.table::setDT(rbind_stops)

# check hexagons in morning peak by income group
tmp <- rbind_stops[time_interval == "60 min",] %>% 
  .[time %in% c("06:00","07:00","08:00"),peak := "Morning"] %>% 
  .[!is.na(peak),] %>% 
  .[,decil_class := fcase(decil_ind %in% 9:10,"20p_richest"
                          , decil_ind %in% 1:5,"50p_poorest")] %>% 
  .[!is.na(decil_class),] %>% 
  .[,list("N_vehicles" = sum(N), "N_hex" = .N, "Minutes" = 180),by = .(peak,decil_class)]

tmp[,vehicles_by_hex_by_minute := N_vehicles / (N_hex * Minutes)]
tmp[,vehicles_by_hex := N_vehicles / (N_hex )]
tmp[,prop_vehicles_by_hex := round(100 * vehicles_by_hex / min(vehicles_by_hex))]
tmp[]

tmp[decil_class == '20p_richest']$vehicles_by_hex / tmp[decil_class == '50p_poorest']$vehicles_by_hex 
#> 4839.267 / 3225.112 = 1.500496

# check mean in morning peak by income group
tmp <- rbind_stops[time_interval == "05 min",] %>% 
  .[time %in% c("06:00","07:00","08:00"),peak := "Morning"] %>% 
  .[!is.na(peak),] %>% 
  .[,hour :=  stringr::str_split(time,":",n = 2,simplify = TRUE)[1],by = .(id_hex,time)] %>% 
  .[,minute :=  stringr::str_split(time,":",n = 2,simplify = TRUE)[2],by = .(id_hex,time)] %>% 
  .[,time_minute := as.numeric(hour) * 60 + as.numeric(minute)] %>% 
  .[,decil_class := fcase(decil_ind %in% 9:10,"20p_richest"
                          , decil_ind %in% 1:5,"50p_poorest")] %>% 
  .[!is.na(decil_class),]

tmp <- tmp[,weighted.mean(x = time_minute,w = N),by = .(decil_class)]

tmp[,hour := V1%/%60]
tmp[,minute := (V1 - hour*60)]
tmp[,time := paste0(hour,":",round(minute,0))]
tmp[]

tmp[decil_class == '20p_richest']$vehicles_by_hex / tmp[decil_class == '50p_poorest']$vehicles_by_hex 


# 5) Plots ----
rm(list=ls())
gc(reset = TRUE)

rbind_stops <- readr::read_rds("data/hex_freq_sf.rds")
data.table::setDT(rbind_stops)


# save tmp data
vec <- unique(rbind_stops$time_interval)

list_plots <- lapply(seq_along(vec),function(i){ # i = 4
  
  tmp <- rbind_stops[total_pop > 0 &
                       time_interval == vec[i] & 
                       !is.na(time ),] %>% 
    .[,weighted.mean(N,total_pop),by = .(time,decil_ind )]   # depois
  
  fixed_time <- c("00:00","04:00","08:00","12:00","16:00","20:00","23:00")
  
  
  plot <- ggplot(tmp)+
    geom_tile(aes(x = time,y= as.factor(decil_ind),fill = V1))+
    scale_x_discrete(breaks = fixed_time,labels = fixed_time)+
    labs(title = vec[i]
         ,x = NULL
         ,y = "Decil de renda"
         , fill = "Frequency of \nbus stops")+
    viridis::scale_fill_viridis()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
  return(plot)
})


library(patchwork)

(list_plots[[1]] | list_plots[[2]])/
  (list_plots[[3]] | list_plots[[4]])

list_plots[[4]]

# rayshader ----
future::plan("multisession", workers = 19)

tmp <- rbind_stops[total_pop > 0 &
                     time_interval == "10 min" & 
                     !is.na(time ),] %>% 
  .[,weighted.mean(N,total_pop),by = .(time,decil_ind )]   # depois

fixed_time <- c("00:00","04:00","08:00","12:00","16:00","20:00","23:00")


plot <- ggplot(tmp)+
  geom_tile(aes(x = time,y= as.factor(decil_ind),fill = V1))+
  scale_x_discrete(breaks = fixed_time,labels = fixed_time)+
  coord_cartesian(expand = FALSE)+
  labs(title = NULL
       ,x = NULL
       ,y = "Income decile")+
  scale_fill_continuous(type = "viridis",direction = +1)+
  theme(axis.text.x = element_text(angle = 0))+
  guides(fill = guide_colourbar(
    title = "Mean frequency\nof vehicles at\npublic transport\nstops by every\n10 min."
    ,title.position = "bottom"
    ,label.position = "left"))
plot

ggplot2::ggsave(plot
                ,filename = "figures/10min_freq_2d.png"
                ,width = 10
                ,height = 8
                ,dpi = 300
                ,scale = 0.65)

rayshader::plot_gg(ggobj = plot
                   , multicore = TRUE
                   , width = 5
                   , height = 5
                   , scale = 250
                   , windowsize = c(1400,866)
                   , zoom = 0.5391094   
                   , phi = 30.4472961      
                   , theta = -23.2254651    )

# find angle view
# rayshader::render_camera(theta = NULL,phi = NULL,zoom = NULL,fov = NULL)
rayshader::render_snapshot(filename = "figures/10min_freq_3d_rayshader.png"
                           ,width = 1000
                           ,height = 1000
                           )

