#################################################
# title: look at presence/absence of A.S. and M.L. over time
# author: lp
# created: 12/19/21
# last edited: 12/19/21
##################################################

##### load packages #####
library(gt)        # nice tables
library(readxl)    # read excel files
library(viridis)   # color palette
library(lubridate) # deal w dates and times
library(ggdark)    # dark field versions of ggplot2 themes
library(tidyverse) # life

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

##### load data #####

point_contact <- read_xlsx('./data/survey/CABR_Coastal_Biodiversity_Survey.xlsx', sheet = 5)
mobile_quad <- read_xlsx('./data/survey/CABR_Coastal_Biodiversity_Survey.xlsx', sheet = 6)
survey_dates <- read_csv("data/survey/cbs_survey_dates.csv")

sio_pier <- read_csv("data/abiotic/SIO_TEMP.csv", 
                     skip = 26)

##### tidy data #####
# Notes
  # Acanthinucella spirata can be categoried as ACASPI or ACASPP (Acanthinucella spp.)
  # Mexicanthina lugubris is MEXLUG
  # make spp list
  sppnames <- c('Mexacanthina lugubris', 'Acanthinucella spp')

# tidy point_contact 
point_contact <- point_contact %>%
  select(intertidal_sitename, year, species_lump, number_of_hits, percent_cover) %>%
  filter(species_lump %in% sppnames)

# there were no spp of interest observed in the point_contact data
remove(point_contact)

# tidy mobile quadrat data 
mobile_quad <- mobile_quad %>%
  select(intertidal_sitename, year, species_lump, density_per_m2) %>%
  filter(species_lump %in% sppnames) %>%
  # join with survey dates
  left_join(., survey_dates, by = c('intertidal_sitename', 'year')) %>%
  mutate(site = if_else(intertidal_sitename == 'Cabrillo I', 
                                       '(A) Zone I', '(B) Zone III'))

# SIO pier data
sio_pier2 <- sio_pier %>%
  clean_names() %>%
  


##### visualize mobile quadrat data #####

ggplot(data = mobile_quad,
       mapping = aes(x = year, y = density_per_m2, group = species_lump,
                     fill = species_lump)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.7) + 
  xlab('Year surveyed') + 
  ylab('Density (whelks/m²)') + 
  facet_wrap(~site, scales = 'free_x') +
  dark_theme + 
  theme(legend.position = 'bottom')

##### mean annual 12 mo prior to collection - avg of month of collection in yr vs density #####
 
temp_table <- select(mobile_quad, date) %>%
  mutate(start_date = mdy(date) - years(1),
         end_date = mdy(date)) %>%
  distinct() %>%
  na.omit()
  

  
  


  
  
