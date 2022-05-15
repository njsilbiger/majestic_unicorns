#################################################
# title: short look at temp + tide data
# purpose: look at water + air temps experienced by whelks over time
# author: lp
# created: 12/16/21
# last edited: 5/15/21
##################################################

##### load packages #####
library(lubridate) # dates/times
library(janitor)   # clean up
library(tidyverse) # life

##### read in data + notes to consider #####

# tide data pulled from SD bay station jan 2016- dec 2021
tide <- read_csv("data/abiotic/sd_bay_tides_assembled.csv")

# pier data pulled from SIO pier station
sio <- read_csv("data/abiotic/sio_pier_assembled.csv", 
                col_types = cols(sio_air_temp = col_double()))

# cabr tidbit data july 2020 - dec 2021
cabr <- read_csv("data/abiotic/cabr_z1_tidbit.csv")

  # get mean temp per hour (to match up with tide + sio datasets)
  cabr <- cabr %>%
    mutate(dtime = round_date(dtime, unit = 'hour')) %>%
    group_by(dtime) %>%
    summarize(tidbit_temp_c = mean(tidbit_temp_c, na.omit = TRUE))
  

# remove tide data only (joined with other 2)
  remove(tide)

##### cabr tidbit figs (jul 2020 - dec 2021) ####

# not de-tided (time series of "raw" data) plot
ggplot(data = cabr,
       mapping = aes(x = dtime, y = tidbit_temp_c, color = tidbit_temp_c)) + 
    geom_point() + 
    xlab('Year-month') + 
    ylab('Intertidal temp. (°C)') +
    ggtitle('CABR Zone 1 TidbiT')

###### sio pier sst figs (2016-2021) #####
  
# we'll prob want a few versions of this plot, so here's the general function
sst_plot_fn <- function(dataset) {
  ggplot(data = dataset,
         mapping = aes(x = dtime, y = sio_sst, color = sio_sst)) + 
    geom_point() +
    xlab('Year') + 
    ylab('SIO SST (°C)') +
    ggtitle('SIO Sea Surface Temp.') + 
    scale_color_viridis() 
}

# save full plot
  ggsave(filename = './figures/sio_sst_2016_2021.png',
         plot = sst_plot_fn(sio) + dark_theme + theme(legend.position = 'none'),
         width = 7, height = 5)
         
# do version for 2020-2021
  ggsave(filename = './figures/sio_sst_2020_2021.png',
         plot = sst_plot_fn(filter(sio, year(dtime) > 2019)) + dark_theme + 
         theme(legend.position = 'none'),
         width = 7, height = 5)
  
##### look at air temp and water temp distribution #####
  
sio2 <- sio %>%
    # make columns for each tide ht of whether the area is in the air/water
    mutate(year = year(dtime),
           `1` = if_else(tide_predict_m >= 1, 'water', 'air'),
           `1.25` = if_else(tide_predict_m >= 1.25, 'water', 'air'),
           `1.5` = if_else(tide_predict_m >= 1.5, 'water', 'air'),
           `1.75` = if_else(tide_predict_m >= 1.75, 'water', 'air'),
           `2` = if_else(tide_predict_m >= 2, 'water', 'air')) %>%
    # get 2016 onwards (last 5 yrs)
    filter(year > 2015) %>%
    # pivot longer
    pivot_longer(cols = c(`1`:`2`), 
                 names_to = 'tide_ht', values_to = 'air_water') %>%
    mutate(temp = if_else(air_water == 'air', sio_air_temp, sio_sst))
    

# violin plots w/ box overlay for each year, faceted by air and water

ggplot(data = sio2,
       mapping = aes(x = year, y = temp, group = year, fill = year)) +
  geom_violin() + 
  scale_fill_viridis() +
  xlab('Year') + 
  ylab('Temperature (°C)') + 
  ggtitle('SIO Pier Station Air & SST Temps') + 
  facet_grid(tide_ht ~ air_water) + 
  dark_theme + 
  theme(legend.position = 'none')

ggsave(filename = './figures/sio_air_water.png',
       width = 5, height = 7)
       

              