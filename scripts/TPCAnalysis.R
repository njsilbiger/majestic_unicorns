### Run TPC curves and calculate metrics

# load libraries
library(here)
library(tidyverse)
library(rTPC)
library(nls.multstart)
library(broom)

## read in the data

RespoData<-read_csv(here("data","RespoFiles","Respo.RNormalized_clean.csv"))

## analysis
# all the tpc model names
get_model_names()

# We will start with "sharpeschoolhigh_1981"

RespoData_clean <- RespoData %>%
  select(ID = SampleID, Species, rate = mmol.gram.hr, temp = Temp.C) %>% # select just what we need
  mutate(rate = ifelse(rate<0,0,rate), # make the - values 0
    rate = log(rate+1))%>%
  drop_na()

# make it possible


Respofits<-RespoData_clean%>%
  filter(!ID %in% c("AS_12","AS_13"))%>%
#  filter(Species == "Acanthinucella spirata")%>%
  nest(data = c(temp, rate)) %>%
  mutate(sharpeschoolhigh = map(data, ~nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 20),                                     data = .x,
                    #        iter = c(4,4,4,4), 
                    iter = 1,
                                                     
start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') - .1,         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') + 1,
        lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981')+2,
        upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
            supp_errors = 'Y',
  convergence_count = FALSE)))

Respofits

# stack models
d_stack <- select(Respofits, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', sharpeschoolhigh)

# get parameters using tidy
params <- d_stack %>%
  mutate(., est = map(fit, tidy)) %>%
  select(-fit) %>%
  unnest(est)

# get predictions using augment
newdata <- tibble(temp = seq(min(d$temp), max(d$temp), length.out = 100))
d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  select(-fit) %>%
  unnest(preds)

d_params <- d_stack %>%
  mutate(params = map(fit, calc_params)) %>%
  unnest(params)


d_params %>%
  ggplot(aes(x = Species, y = topt))+
  geom_boxplot()+
  geom_jitter(width = 0.2)



# plot
ggplot(d_preds, aes(temp, rate)) +
  geom_point(aes(temp, rate), RespoData_clean) +
  geom_line(aes(temp, .fitted), col = 'blue') +
  facet_wrap(~ID, scales = 'free') +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        strip.background = element_blank()) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Fits of every model available in rTPC') +
  geom_hline(aes(yintercept = 0), linetype = 2)


### get params

params <-d_stack$fit %>%
  map(.,tidy) %>%
  unnest()

### test things
# keep just a single curve
d <- filter(RespoData_clean, ID == "ML_6")

# show the data
ggplot(d, aes(temp, rate)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Respiration across temperatures')
#choose model
mod = 'sharpschoolhigh_1981'

# get start vals
start_vals <- get_start_vals(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')


# get limits
low_lims <- get_lower_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')

fit <- nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 18),
                     data = d,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

# calculate additional traits
calc_params(fit) %>%
  # round for easy viewing
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(d$temp), max(d$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
ggplot(d, aes(temp, rate)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Respiration across temperatures')
