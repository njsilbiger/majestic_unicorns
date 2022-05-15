## Thermal niche analyses for snails
## Edited by Nyssa, Piper, Lauren, and Laura

### load libraries
library(tidyverse)
library(here)
library(janitor)
library(performance)

# read in the snail data
snails<-read_csv(here("data","FieldTemperatures","SnailTemps.csv")) %>%
  clean_names() # to clean the names because of spaces
  
# Analysis
snail_calc<-snails %>%
  mutate(difftemp = body_temp - surface_temp, # temp difference 
         difftemp_mm = (body_temp - surface_temp)) # nonrmalized temp difference)

# summary stats         
snail_summary<-snails_calc %>%
  group_by(species)%>%
  summarize(counts = n(),
            meandiff = mean(difftemp),
            se = meandiff/sqrt(n()))

# plot of size normalized temperature per species
snail_calc %>%
  ggplot(aes(x = species, y = difftemp_mm)) +
  geom_boxplot()+
  geom_jitter(width = 0.2)
  
# plot os snail temp by species
snail_calc %>%
  ggplot(aes(x = species, y = difftemp)) +
  geom_boxplot()+
  geom_jitter(width = 0.2)

## diff temperature between internal and external  
mod<-lm(difftemp~species, data = snails_calc)
check_model(mod)
anova(mod)

### 
# thermal niche
snails_calc %>%
ggplot(aes(x = species, y = surface_temp, color = air_temp)) +
  geom_boxplot()+
  geom_jitter(width = 0.2)

## transects were 40-45m each

# Tide height ocean times
# 1/28/22  = -0.155 m 
# 1/29 = -0.243 m

# Need to convert all tide height data to meters from feet
