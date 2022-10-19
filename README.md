### Exploring the time geography of public transport networks with the gtfs2gps package


## Reference
- Pereira, R. H. M., Andrade, P. R., & Bazzo Vieira, J. P. (2022). Exploring the time geography of public transport networks with the gtfs2gps package. SocArXiv Preprint [https://doi.org/10.31235/osf.io/qydr6](https://doi.org/10.31235/osf.io/qydr6)


This repository contains the R scripts used to produce the figures and other results of the study. Each figure and analysis are described below:

#### - `R/0_process_input_data.R`
 This script aims to download and pre-process the GTFS data used in the case study. We used the GTFS data of São Paulo from EMTU and SPTRANS transportation authorities. In order to generate the figures of the study, users should first run this code.

#### - `R/1.0_plot_basic_route`

Space-time path of a single public transport trip in São Paulo, Brazil
<img align="left" src="https://github.com/ipeaGIT/gtfs2gps-time_geography/blob/master/figures/emtu_first_plot.png?raw=true" alt="logo" width="180"> 

#### - `R/1.2.1_plot_emtu_monday.R` 

(A) Space-time paths of multiple trips of a public transport route between 5 am and 10 am in São Paulo, Brazil.
<img align="left" src="https://github.com/ipeaGIT/gtfs2gps-time_geography/blob/master/figures/12_monday.png?raw=true" alt="logo" width="180"> 

#### - `R/1.2.2_plot_emtu_sunday.R`

(B) Space-time paths of multiple trips of a public transport route between 5 am and 10 am in São Paulo, Brazil.
<img align="left" src="https://github.com/ipeaGIT/gtfs2gps-time_geography/blob/master/figures/12_sunday.png?raw=true" alt="logo" width="180"> 

#### - `R/1.1_plot_trips_intersection.R`

Bundling of space-time paths of routes/trips that arrive at a selected bus stop within a 10-minute window. São Paulo, Brazil.
<img align="left" src="https://github.com/ipeaGIT/gtfs2gps-time_geography/blob/master/figures/intersection_emtu.png?raw=true" alt="logo" width="180"> 


#### - `R/1.3_plot_vehicle_frequency.R`

2D and 3D representation of the average frequency of public transport services in regions of different income levels across different times of the day, São Paulo, 2019.

<img align="left" src="https://github.com/ipeaGIT/gtfs2gps-time_geography/blob/master/figures/10min_freq_2d.png?raw=true" alt="logo" width="180"> 

<img align="left" src="https://github.com/ipeaGIT/gtfs2gps-time_geography/blob/master/figures/10min_freq_3d_rayshader.png?raw=true" alt="logo" width="180"> 


* Corresponding author.
E-mail address: rafael [dot] pereira [at] ipea [dot] gov [dot] br
