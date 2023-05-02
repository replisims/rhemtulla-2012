
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


saveRDS(simDat, "./results/simDat.rds")

simDat$skew <- simDat$cat_data %>%
  map_dbl(skewness) %>% round(digits = 2)# good 
simDat$kurt <- simDat$cat_data %>%
  map_dbl(kurtosis) %>% round(digits = 2)# bad



# Table -------------------------------------------------------------------

library(kableExtra)

tab1_dat <- simDat %>% 
  dplyr::select(dist, cat, sym, skew, kurt) %>% 
  arrange(sym, dist, cat) %>% 
  mutate(kurt = kurt - 3)
#%>% 
 # mutate(skew = skew, kurt = kurt)

tab1df_wide <- tab1_dat %>% 
  pivot_wider(names_from = c(sym),
              values_from = c(skew, kurt))


kbl(tab1df_wide %>% 
      dplyr::select(all_of(c("dist", "cat", "skew_sym", "kurt_sym", "skew_moderate", "kurt_moderate",
                      "skew_moderate-alt", "kurt_moderate-alt", "skew_extreme","kurt_extreme", 
                      "kurt_extreme-alt", "skew_extreme-alt"))),
    col.names = c("Underlying distribution", "Categories", rep(c("S", "K"), 5))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 2, 
                     "Symmetry" = 2,
                     "Mod. Asym" = 2,
                     "Mod. Asym-Alt" = 2,
                     "Ext. Asym-Alt" = 2,
                     "Ext. Asym-Alt" = 2)) %>% 
  footnote(general = "Values in this table were obtained by generating samples of size N = 1,000,000 for and recording the skew and kurtosis of the observed distributions. Mod. Asym= Moderate Asymmetry; Mod.Asym-Alt = Moderate Asymmetry-Alternating; Ext.Asym = Extreme Asymmetry; Ext. Asym-Alt = Extreme Asymmetry-Alternating: S = skew; K = kurtosis")