
# Skew and Kurtosis of Simulated data -------------------------------------

library(moments)
library(rhemtulla2012)
library(tidyverse)

sim_scenarios2 <- rhemtulla2012:::getScenarios(its = 1,
                                              N = 1000000,
                                              cat = c(2, 3, 4, 5, 6, 7),
                                              sym = c("sym", "moderate", "extreme", "moderate-alt", "extreme-alt" ),
                                              models = c("model1"),
                                              dist = c("normal", "non-normal"),
                                              conditions = NULL)

sim_scenarios2 %>% 
  rowwise() %>%
  mutate(cat_data = list(getCatData(models = models,
                                    N = N, 
                                    cat = cat, 
                                    sym = sym, 
                                    dist = dist)[, 1])) -> simDat


simDat$skew <- simDat$cat_data %>%
  map_dbl(skewness) %>% round(digits = 2)# good 
simDat$kurt <- simDat$cat_data %>%
  map_dbl(kurtosis) # bad
