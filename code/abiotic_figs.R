#################################################
# title: short look at temp + tide data
# purpose: look at water + air temps experienced by whelks over time
# author: lp
# created: 12/16/21
# last edited: 12/16/21
##################################################

##### load packages #####
library(gt)        # nice tables
library(viridis)   # color palette
library(ggdark)    # dark field versions of ggplot2 themes
library(lubridate) # dates/times
library(tidyverse) # life

##### themes + presets #####

# not in operator
`%notin%` <- Negate(`%in%`)

# theme
light_theme <- theme_bw() + theme(text = element_text(size = 12),
                                    # add more space between panels
                                    panel.spacing = unit(1, 'lines'),
                                    # no background to wrap panels
                                    strip.background = element_blank(),
                                    strip.text = element_text(size = 12, 
                                                              hjust = 0),
                                    # panel labels outside x axis labels
                                    strip.placement = 'outside',
                                    # adjust x axis labels
                                    axis.text.y = element_text(size = 12),
                                    axis.text.x = element_text(size = 12, 
                                                               angle = 45, 
                                                               hjust = 1))

dark_theme <- dark_theme_bw() + theme(text = element_text(size = 12),
                                        # add more space between panels
                                        panel.spacing = unit(1, 'lines'),
                                        # no background to wrap panels
                                        strip.background = element_blank(),
                                        strip.text = element_text(size = 12, 
                                                                  hjust = 0),
                                        # panel labels outside x axis labels
                                        strip.placement = 'outside',
                                        # adjust x axis labels
                                        axis.text.y = element_text(size = 12),
                                        axis.text.x = element_text(size = 12, 
                                                                   angle = 45, 
                                                                   hjust = 1))

##### read in data + notes to consider #####

# tide data pulled from SD bay station jan 2016- dec 2021
tide <- read_csv("data/abiotic/sd_bay_tides_assembled.csv")

# pier data pulled from SIO pier station
sio <- read_csv("data/abiotic/sio_pier_assembled.csv", 
                col_types = cols(sio_air_temp = col_double()))

  # join pier and tide data 
  sio <- left_join(tide, sio, by = 'dtime')

# cabr tidbit data july 2020 - dec 2021
cabr <- read_csv("data/abiotic/cabr_z1_tidbit.csv")

  # get mean temp per hour (to match up with tide + sio datasets)
  cabr <- cabr %>%
    mutate(dtime = round_date(dtime, unit = 'hour')) %>%
    group_by(dtime) %>%
    summarize(tidbit_temp_c = mean(tidbit_temp_c, na.omit = TRUE))
  
  # join tidbit data with tide data
  cabr <- left_join(cabr, tide, by = 'dtime')

# remove tide data only (joined with other 2)
  remove(tide)

##### cabr tidbit figs (jul 2020 - dec 2021) ####

# not de-tided (time series of "raw" data) plot
intertidal_plot <- ggplot(data = cabr,
       mapping = aes(x = dtime, y = tidbit_temp_c, color = tidbit_temp_c)) + 
    geom_point() + 
    xlab('Year-month') + 
    ylab('Intertidal temp. (°C)') +
    ggtitle('CABR Zone 1 TidbiT') + 
    scale_color_viridis() 
  
  # save plot (theme in save so it's easier to save light version if desired)
  ggsave(filename = './figures/intertidal_temp.png',
       plot = intertidal_plot + dark_theme + theme(legend.position = 'none'),
       width = 7, height = 5)

# I'm not sure about tide height, but I'll figure that out today so I can de-tide
  
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
  



              