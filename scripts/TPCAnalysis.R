### Run TPC curves and calculate metrics

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
    filter(Temp.Block != 30 | Species != 'Acanthinucella spirata') 
   # filter(SampleID != 'AS_18',
   #        Temp.Block < 33,
   #        Temp.Block != 28)

## analysis
# all the tpc model names
get_model_names()

## Inidividuals

RespoData %>%
  filter(Temp.Block<32)%>%
  ggplot(aes(x = Temp.C, y = umol.gram.hr_uncorr, group = SampleID))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~SampleID, scales = "free_y")


# get means and se for rates at each temperature
RespoMeans<-RespoData  %>% # I think the weight is wrong
  group_by(Species, Temp.Block) %>%
  summarise(rate_mean = mean(umol.gram.hr, na.rm = TRUE),
            rate_se = rate_mean/sqrt(n()),
            rate_sd = sd(umol.gram.hr, na.rm = TRUE),
            lower.ci = rate_mean - qt(1 - (0.05 / 2), n() - 1) * rate_se,
            upper.ci = rate_mean + qt(1 - (0.05 / 2), n() - 1) * rate_se) %>% ungroup()


# plot the means of the uncorrected data 
RespoMeans %>%
  filter(Temp.Block<34)%>%
  ggplot(aes(x = Temp.Block, y = rate_mean, color = Species))+ geom_point(size = 3)+
  geom_smooth()+
  geom_point(data = RespoData %>%
               filter(Temp.Block<28), aes(x = Temp.Block, y = umol.gram.hr, color = Species), alpha = 0.1)+
  geom_errorbar(aes(ymin = lower.ci,
                    ymax = upper.ci),width = 0.2)


## Let's fit a weighted TPC (by SD) to account for variance within a temperature and then use bootstrapping to calculate error



# We will start with "sharpeschoolhigh_1981"


# make it possible
RespoMeans2<-RespoMeans %>%
  select(Species, temp = Temp.Block, rate = rate_mean, sd = rate_sd)


## Mexicanthina
dfit<-nest(RespoMeans2 %>% filter(Species  == "Mexicanthina lugubris"), data = c(temp, rate ,sd)) %>%
  mutate(sharpeschoolhigh = map(data, ~nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 20),                                     data = .x,
                                                     #        iter = c(4,4,4,4), 
                                                     iter = 1,
                                                     
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') - .1,         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') + 1,
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981')+2,
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE,
# include weights here!
modelweights = 1/sd)))

dfit$sharpeschoolhigh

# get predictions
newdata <- tibble(temp = seq(min(RespoMeans2$temp), max(RespoMeans2$temp), length.out = 100))

d_preds <- dfit %>%
  mutate(., preds = map(sharpeschoolhigh, augment, newdata = newdata)) %>%
  select(-sharpeschoolhigh) %>%
  unnest(preds)

## Acanthinucella
dfit_Ac<-nest(RespoMeans2 %>% filter(Species  == "Acanthinucella spirata"), data = c(temp, rate ,sd)) %>%
  mutate(sharpeschoolhigh = map(data, ~nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 20),                                     data = .x,
                                                     #        iter = c(4,4,4,4), 
                                                     iter = 1,
                                                     
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') - .1,         start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') + 1,
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981')+2,
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE,
                                                     # include weights here!
                                                     modelweights = 1/sd)))

# get predictions
d_preds_Ac <- dfit_Ac %>%
  mutate(., preds = map(sharpeschoolhigh, augment, newdata = newdata)) %>%
  select(-sharpeschoolhigh) %>%
  unnest(preds)

# Make a plot
# plot
RespoMeans2_Mex<- 
  RespoMeans2 %>% 
filter(Species == 'Mexicanthina lugubris')

RespoMeans2_Ac<- 
  RespoMeans2 %>% 
  filter(Species == 'Acanthinucella spirata')

ggplot() +
  geom_line(aes(temp, .fitted), d_preds) +
  geom_linerange(aes(x = temp, ymin = rate - sd, ymax = rate + sd), RespoMeans2_Mex) +
  geom_point(aes(temp, rate), RespoMeans2_Mex, size = 2, shape = 21, fill = 'green4') +
  
  geom_line(aes(temp, .fitted), d_preds_Ac, color = 'grey') +
  geom_linerange(aes(x = temp, ymin = rate - sd, ymax = rate + sd), RespoMeans2_Ac) +
  geom_point(aes(temp, rate), RespoMeans2_Ac, size = 2, shape = 21, fill = 'red4') +
  
  theme_bw(base_size = 12) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        strip.background = element_blank()) +
  labs(x ='Temperature (ºC)',
       y = 'Metabolic rate',
       title = 'Respiration rates across temperatures') +
  geom_hline(aes(yintercept = 0), linetype = 2) 
  
  #ylim(c(-0.25, 3.5))

# Mex
# refit model using nlsLM
fit_nlsLM <- minpack.lm::nlsLM(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 20),
                               data = RespoMeans2_Mex,
                               start = coef(dfit$sharpeschoolhigh[[1]]),
                               lower = get_lower_lims(RespoMeans2_Mex$temp, RespoMeans2_Mex$rate, model_name = 'sharpeschoolhigh_1981'),
                               upper = get_upper_lims(RespoMeans2_Mex$temp, RespoMeans2_Mex$rate, model_name = 'sharpeschoolhigh_1981'),
                               weights = 1/sd)

# perform case bootstrap
boot1 <- Boot(fit_nlsLM, method = 'case')
#> 

# predict over new data
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(RespoMeans2_Mex$temp), max(RespoMeans2_Mex$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 20))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975),
            mean = mean(pred, na.rm = TRUE),
            .groups = 'drop')

# calculate all params
allparams<-calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')


ci_allparams <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_allparams <- left_join(ci_allparams, allparams) %>%
  mutate(Species = "Mexicanthina")

# AC
# refit model using nlsLM
fit_nlsLM_Ac <- minpack.lm::nlsLM(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 20),
                               data = RespoMeans2_Ac,
                               start = coef(dfit$sharpeschoolhigh[[1]]),
                               lower = get_lower_lims(RespoMeans2_Ac$temp, RespoMeans2_Ac$rate, model_name = 'sharpeschoolhigh_1981'),
                               upper = get_upper_lims(RespoMeans2_Ac$temp, RespoMeans2_Ac$rate, model_name = 'sharpeschoolhigh_1981'),
                               weights = 1/sd)

# perform case bootstrap
boot1_Ac <- Boot(fit_nlsLM_Ac, method = 'case')
#> 

# predict over new data
boot1_preds_Ac <- boot1_Ac$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(RespoMeans2_Ac$temp), max(RespoMeans2_Ac$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 20))

# calculate bootstrapped confidence intervals
boot1_conf_preds_Ac <- group_by(boot1_preds_Ac, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975),
            mean = mean(pred, na.rm = TRUE),
            .groups = 'drop')


# calculate all params
allparams_Ac<-calc_params(fit_nlsLM_Ac) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')


ci_allparams_Ac <- Boot(fit_nlsLM_Ac, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_allparams_Ac <- left_join(ci_allparams_Ac, allparams_Ac) %>%
  mutate(Species = "Acanthanucella")

allparams_both<-bind_rows(ci_allparams, ci_allparams_Ac)

Topt<-allparams_both %>%
  filter(param == "topt")

### plot bootsstrapped predictions
ggplot() +
  #geom_line(aes(temp, .fitted), d_preds, col = 'black') +
  geom_line(aes(temp, pred, group = iter), boot1_preds, alpha = 0.007, color = "purple") +
  geom_line(aes(temp, pred, group = iter), boot1_preds_Ac, alpha = 0.007, color = "green") +
  geom_line(data = boot1_conf_preds, aes(x = temp, y = mean), color = "purple", lwd = 3)+
  geom_line(data = boot1_conf_preds_Ac, aes(x = temp, y = mean), color = "green", lwd = 3)+
  geom_vline(aes(xintercept = Topt$estimate[1]), color = "purple", lty = 2, lwd = 2)+
  geom_vline(aes(xintercept = Topt$estimate[2]), color = "green", lty = 2, lwd = 2)+
  geom_text(aes(x = 20.5, y = 0.02, label = "Mexicanthina"), size = 10)+
  geom_text(aes(x = 30, y = 0.065, label = "Acanthanucella"), size = 10)+
  #geom_linerange(aes(x = temp, ymin = rate - sd, ymax = rate + sd), RespoMeans2_Mex) +
  #geom_point(aes(temp, rate), RespoMeans2_Mex, size = 2, shape = 21, fill = 'green4') +
  theme_bw(base_size = 10) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        strip.background = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 22),
        axis.title = element_text(size = 22)) +
  labs(x ='Temperature (ºC)',
       y = 'Respiration rate (mmol g-1 hr-1)') 

ggsave(here("output", "TPCFig.png"))

 # geom_hline(aes(yintercept = 0), linetype = 2) 


### plot the params
ggplot(allparams_both, aes(Species, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') 

## Calculate parameters
param <- broom::tidy(fit_nlsLM) %>%
  select(param = term, estimate)

# CIs from case resampling
ci3 <- confint(boot1) %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

############# other
Respofits<-RespoData_clean%>%
#  filter(ID %in% c("ML_19","ML_20"))%>%
  filter(!ID %in% c("AS_15","AS_16", "AS_7", "AS_8","AS_9", "ML_4","ML_9"))%>%
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
