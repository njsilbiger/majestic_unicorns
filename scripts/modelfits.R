### Model Selection of different TPC models
## Created on 8/13/2022
## Created by Nyssa

###################

# load libraries
library(here)
library(tidyverse)
library(rTPC)
library(nls.multstart)
library(broom)
library(boot)
library(car)

## read in the data

# RespoData<-read_csv(here("data","RespoFiles","Respo.RNormalized_clean.csv"))
RespoData<-read_csv(here("data","RespoFiles","Respo.RNormalized.csv"))%>%
   filter(Temp.Block != 30 | Species != 'Acanthinucella spirata') # this value is really off... 
     #Temp.Block != 30)
     #SampleID != 'AS_18')

  #        Temp.Block < 35,
  #        Temp.Block != 28) 
  # 
d<- RespoData %>%
  select(Species, temp = Temp.Block, umol.gram.hr_uncorr, umol.gram.hr) %>%
  group_by(Species, temp) %>%
  summarise(rate = mean(umol.gram.hr, na.rm = TRUE),
            sd = sd(umol.gram.hr, na.rm = TRUE))%>%
  ungroup()
  

# when scaling up our code to fit hundreds of models, its nice to have a progress bar
# edit nls_multstart to allow for a progress bar
nls_multstart_progress <- function(formula, data = parent.frame(), iter, start_lower, 
                                   start_upper, supp_errors = c("Y", "N"), convergence_count = 100, 
                                   control, modelweights, ...){
  if(!is.null(pb)){
    pb$tick()
  }
  nls_multstart(formula = formula, data = data, iter = iter, start_lower = start_lower, 
                start_upper = start_upper, supp_errors = supp_errors, convergence_count = convergence_count, 
                control = control, modelweights = modelweights, ...)
}

# start progress bar and estimate time it will take
number_of_models <- 3
number_of_curves <- length(unique(d$Species))

# setup progress bar
pb <- progress::progress_bar$new(total = number_of_curves*number_of_models,
                                 clear = FALSE,
                                 format ="[:bar] :percent :elapsedfull")

## run the model fits on a weighted regression
# fit two chosen model formulation in rTPC
d_fits <- nest(d, data = c(temp, rate, sd)) %>%
  mutate(sharpeschoolhigh = map(data, ~nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 20),                                     data = .x,
                                                     #        iter = c(4,4,4,4), 
                                                    iter = c(3,3,3,3),
                                                     
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') - .1,         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') + 1,
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981')+2,
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE,
                                                     # include weights here!
                                                     modelweights = 1/sd)),
         gaussian = map(data, ~nls_multstart(rate ~ gaussian_1987(temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(3,3,3),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 1,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 1,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE,
                                             modelweights = 1/sd)),
         
         quadratic = map(data, ~nls_multstart(rate~quadratic_2008(temp = temp, a, b, c),
                                              data = .x,
                                              iter = c(4,4,4),
                                              start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') - 0.5,
                                              start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') + 0.5,
                                              lower = get_lower_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              upper = get_upper_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE,
                                              modelweights = 1/sd)))


# create new list column of for high resolution data
d_preds <- mutate(d_fits, new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100)))) %>%
  # get rid of original data column
  select(., -data) %>%
  # stack models into a single column, with an id column for model_name
  pivot_longer(., names_to = 'model_name', values_to = 'fit', c(sharpeschoolhigh:quadratic)) %>%
  # create new list column containing the predictions
  # this uses both fit and new_data list columns
  mutate(preds = map2(fit, new_data, ~augment(.x, newdata = .y))) %>%
  # select only the columns we want to keep
  select(Species,model_name, preds) %>%
  # unlist the preds list column
  unnest(preds)

ggplot(d_preds) +
  geom_line(aes(temp, .fitted, col = model_name)) +
  geom_point(aes(temp, rate), d) +
  facet_wrap(~Species, scales = 'free_y', ncol = 6) +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_color_brewer(type = 'qual', palette = 2) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'All fitted thermal performance curves',
      # subtitle = 'gaussian in green; sharpeschoolhigh in orange'
       )

# calculate the params
d_params <- pivot_longer(d_fits, names_to = 'model_name', values_to = 'fit', c(sharpeschoolhigh: quadratic)) %>%
  mutate(params = map(fit, calc_params)) %>%
  select(Species, model_name, params) %>%
  unnest(params)

