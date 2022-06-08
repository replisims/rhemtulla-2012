
# Tables supplements ------------------------------------------------------

sim_fit_cov <- readRDS("sim_fit_cov_no_out.rds")

sim_fit_all_unnest <- readRDS("sim_fit_all_unnest_no_out.rds")



lambda_03 <- c('b1', 'b6', 'b11', 'b16') 
lambda_04 <- c('b2', 'b7', 'b12', 'b17')
lambda_05 <- c('b3', 'b8', 'b13', 'b18')
lambda_06 <- c('b4', 'b9', "b14", "b19")
lambda_07 <- c("b5", "b10", "b15", "b20")

sim_fit_all_unnest <- sim_fit_all_unnest %>% 
  mutate(par_type = case_when(label %in% lambda_03 ~ "l03",
                              label %in% lambda_04 ~ "l04",
                              label %in% lambda_05 ~ "l05",
                              label %in% lambda_06 ~ "l06",
                              label %in% lambda_07 ~ "l07",
                              label == "s1" ~ "s1"))

# A2/A3 Number of nonconverged cases per 1000 replications -------------------
library(kableExtra)
#!!!!!!!! ATTENTION NORMAL AND NON-NORMAL ARE SWITCHED AROUND
fig_a1_dat <- simfitALL %>% 
  group_by(dist, models, sym, N, estimator, cat) %>% 
  summarize(non_convergence = sum(!converged))


fig_a1_dat_wide <- fig_a1_dat %>% 
  pivot_wider(names_from = c(sym, N, estimator),
              values_from = c(non_convergence)) %>% 
  mutate(dist = factor(dist, levels = c("normal", "non-normal")))


#fig_a1_dat_wide$dist <-  c("normal", rep(" ", 11), "non-normal", rep(" ", 11))
fig_a1_dat_wide$models <-  c("1", rep(" ", 5), "2", rep(" ", 5),"1", rep(" ", 5), "2", rep(" ", 5))

kbl(fig_a1_dat_wide %>% ungroup(),
    col.names = c("Distribution", "Model", "cats", rep(c("ML", "ULS"), 20))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 3, 
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2)) %>% 
  add_header_above(c(" " = 3, 
                     "Ext. Asym." = 8,
                     "Ext. Asym.-Alt" = 8,
                     "Mod. Asym." = 8,
                     "Mod. Asym.-Alt" = 8,
                     "Symmetric" = 8)) %>%
  footnote(general = "Mod.Asym = Moderate Asymmetry ;
           Mod.Asym-Alt = Moderate Asymmetry-Alternating;
           Ext. Asym = Extreme Asymmetry;
           Ext. Asym-Alt = Extreme Asymetry-Alternating;
           ML = robust normal-theory maximum likelihood;
           ULS = robust categorical least squares.")# %>% landscape()


# A4/A5 Number of improper solutions per 1000 replications ----------------

fig_a4_dat <- simfitALL %>% 
  group_by(dist, models, sym, N, estimator, cat) %>% 
  summarize(improper = sum(!post_check))


fig_a4_dat_wide <- fig_a4_dat %>% 
  pivot_wider(names_from = c(sym, N, estimator),
              values_from = c(improper)) %>% 
  mutate(dist = factor(dist, levels = c("normal", "non-normal")))


#fig_a4_dat_wide$dist <-  c("normal", rep(" ", 11), "non-normal", rep(" ", 11))
fig_a4_dat_wide$models <-  c("1", rep(" ", 5), "2", rep(" ", 5),"1", rep(" ", 5), "2", rep(" ", 5))

kbl(fig_a4_dat_wide %>% ungroup(),
    col.names = c("Distribution", "Model", "cats", rep(c("ML", "ULS"), 20))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 3, 
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2)) %>% 
  add_header_above(c(" " = 3, 
                     "Ext. Asym." = 8,
                     "Ext. Asym.-Alt" = 8,
                     "Mod. Asym." = 8,
                     "Mod. Asym.-Alt" = 8,
                     "Symmetric" = 8)) %>%
  footnote(general = "Mod.Asym = Moderate Asymmetry ;
           Mod.Asym-Alt = Moderate Asymmetry-Alternating;
           Ext. Asym = Extreme Asymmetry;
           Ext. Asym-Alt = Extreme Asymetry-Alternating;
           ML = robust normal-theory maximum likelihood;
           ULS = robust categorical least squares.")# %>% landscape()


# Parameter bias ----------------------------------------------------------


# A6 Parameterbias Model 1 Underlying Distribution = Normal ---------------

fig_a6_dat <- sim_fit_cov %>% 
  dplyr::filter(models == "model1") %>% 
  dplyr::filter(dist == "normal") %>% 
  group_by(estimator, rep, scenario_id, label) %>% 
  mutate(bias = mean(est, na.rm = TRUE) - true_par) %>% 
  group_by(par_type, sym, N, estimator, cat) %>% 
  summarize(bias = mean(bias, na.rm = TRUE) %>% round(digits = 3))
  

fig_a6_dat_wide <- fig_a6_dat %>% 
  pivot_wider(names_from = c(sym, N, estimator),
              values_from = c(bias)) 

fig_a6_dat_wide$par_type <-  c("lambda = .3", rep(" ", 5), 
                               "lambda = .4", rep(" ", 5),
                               "lambda = .5", rep(" ", 5), 
                               "lambda = .6", rep(" ", 5), 
                               "lambda = .7", rep(" ", 5), 
                               "phi = .3", rep(" ", 5))


kbl(fig_a6_dat_wide %>% ungroup(),
    col.names = c("param.", "cats", rep(c("ML", "ULS"), 20))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 2, 
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2)) %>% 
  add_header_above(c(" " = 2, 
                     "Ext. Asym." = 8,
                     "Ext. Asym.-Alt" = 8,
                     "Mod. Asym." = 8,
                     "Mod. Asym.-Alt" = 8,
                     "Symmetric" = 8)) %>%
  footnote(general = "Mod.Asym = Moderate Asymmetry ;
           Mod.Asym-Alt = Moderate Asymmetry-Alternating;
           Ext. Asym = Extreme Asymmetry;
           Ext. Asym-Alt = Extreme Asymetry-Alternating;
           ML = robust normal-theory maximum likelihood;
           ULS = robust categorical least squares.")# %>% landscape() 

# A7 Parameterbias Model 1 Underlying Distribution = non-Normal ---------------

fig_a7_dat <- sim_fit_cov %>% 
  dplyr::filter(models == "model1") %>% 
  dplyr::filter(dist == "non-normal") %>% 
  group_by(estimator, rep, scenario_id, label) %>% 
  mutate(bias = mean(est, na.rm = TRUE) - true_par) %>% 
  group_by(par_type, sym, N, estimator, cat) %>% 
  summarize(bias = mean(bias, na.rm = TRUE) %>% round(digits = 3))


fig_a7_dat_wide <- fig_a7_dat %>% 
  pivot_wider(names_from = c(sym, N, estimator),
              values_from = c(bias))

fig_a7_dat_wide$par_type <-  c("lambda = .3", rep(" ", 5), 
                               "lambda = .4", rep(" ", 5),
                               "lambda = .5", rep(" ", 5), 
                               "lambda = .6", rep(" ", 5), 
                               "lambda = .7", rep(" ", 5), 
                               "phi = .3", rep(" ", 5))


kbl(fig_a7_dat_wide %>% ungroup(),
    col.names = c("param.", "cats", rep(c("ML", "ULS"), 20))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 2, 
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2)) %>% 
  add_header_above(c(" " = 2, 
                     "Ext. Asym." = 8,
                     "Ext. Asym.-Alt" = 8,
                     "Mod. Asym." = 8,
                     "Mod. Asym.-Alt" = 8,
                     "Symmetric" = 8)) %>%
  footnote(general = "Mod.Asym = Moderate Asymmetry ;
           Mod.Asym-Alt = Moderate Asymmetry-Alternating;
           Ext. Asym = Extreme Asymmetry;
           Ext. Asym-Alt = Extreme Asymetry-Alternating;
           ML = robust normal-theory maximum likelihood;
           ULS = robust categorical least squares.")# %>% landscape() 

# A8 Parameterbias Model 2 Underlying Distribution = Normal ---------------

fig_a8_dat <- sim_fit_cov %>% 
  dplyr::filter(models == "model2") %>% 
  dplyr::filter(dist == "normal") %>% 
  group_by(estimator, rep, scenario_id, label) %>% 
  mutate(bias = mean(est, na.rm = TRUE) - true_par) %>% 
  group_by(par_type, sym, N, estimator, cat) %>% 
  summarize(bias = mean(bias, na.rm = TRUE) %>% round(digits = 3))


fig_a8_dat_wide <- fig_a8_dat %>% 
  pivot_wider(names_from = c(sym, N, estimator),
              values_from = c(bias))

fig_a8_dat_wide$par_type <-  c("lambda = .3", rep(" ", 5), 
                               "lambda = .4", rep(" ", 5),
                               "lambda = .5", rep(" ", 5), 
                               "lambda = .6", rep(" ", 5), 
                               "lambda = .7", rep(" ", 5), 
                               "phi = .3", rep(" ", 5))


kbl(fig_a8_dat_wide %>% ungroup(),
    col.names = c("param.", "cats", rep(c("ML", "ULS"), 20))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 2, 
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2)) %>% 
  add_header_above(c(" " = 2, 
                     "Ext. Asym." = 8,
                     "Ext. Asym.-Alt" = 8,
                     "Mod. Asym." = 8,
                     "Mod. Asym.-Alt" = 8,
                     "Symmetric" = 8)) %>%
  footnote(general = "Mod.Asym = Moderate Asymmetry ;
           Mod.Asym-Alt = Moderate Asymmetry-Alternating;
           Ext. Asym = Extreme Asymmetry;
           Ext. Asym-Alt = Extreme Asymetry-Alternating;
           ML = robust normal-theory maximum likelihood;
           ULS = robust categorical least squares.")# %>% landscape() 

# A9 Parameterbias Model 2 Underlying Distribution = non-Normal ---------------

fig_a9_dat <- sim_fit_cov %>% 
  dplyr::filter(models == "model2") %>% 
  group_by(estimator, rep, scenario_id, label) %>% 
  mutate(bias = mean(est, na.rm = TRUE) - true_par) %>% 
  group_by(par_type, sym, N, estimator, cat) %>% 
  summarize(bias = mean(bias, na.rm = TRUE) %>% round(digits = 3))


fig_a9_dat_wide <- fig_a9_dat %>% 
  pivot_wider(names_from = c(sym, N, estimator),
              values_from = c(bias))

fig_a9_dat_wide$par_type <-  c("lambda = .3", rep(" ", 5), 
                               "lambda = .4", rep(" ", 5),
                               "lambda = .5", rep(" ", 5), 
                               "lambda = .6", rep(" ", 5), 
                               "lambda = .7", rep(" ", 5), 
                               "phi = .3", rep(" ", 5))


kbl(fig_a9_dat_wide %>% ungroup(),
    col.names = c("param.", "cats", rep(c("ML", "ULS"), 20))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 2, 
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2)) %>% 
  add_header_above(c(" " = 2, 
                     "Ext. Asym." = 8,
                     "Ext. Asym.-Alt" = 8,
                     "Mod. Asym." = 8,
                     "Mod. Asym.-Alt" = 8,
                     "Symmetric" = 8)) %>%
  footnote(general = "Mod.Asym = Moderate Asymmetry ;
           Mod.Asym-Alt = Moderate Asymmetry-Alternating;
           Ext. Asym = Extreme Asymmetry;
           Ext. Asym-Alt = Extreme Asymetry-Alternating;
           ML = robust normal-theory maximum likelihood;
           ULS = robust categorical least squares.")# %>% landscape() 


# A10 Efficiency, Model 1, Underlying Distribution = Normal ---------------
fig_a10_dat <- sim_fit_all_unnest %>% 
  dplyr::filter(models == "model1") %>% 
  dplyr::filter(dist == "normal") %>% 
  group_by(estimator, scenario_id, label) %>% 
  mutate(square_diff = (est - mean(est, na.rm = TRUE))^2) %>% 
  mutate(efficiency = sqrt(sum(square_diff, na.rm = TRUE)/(n()-1))) %>% 
  group_by(par_type, sym, N, estimator, cat) %>% 
  summarize(efficiency = mean(efficiency, na.rm = TRUE) %>% round(digits = 3))


fig_a10_dat_wide <- fig_a10_dat %>% 
  pivot_wider(names_from = c(sym, N, estimator),
              values_from = c(efficiency))

fig_a10_dat_wide$par_type <-  c("lambda = .3", rep(" ", 5), 
                                "lambda = .4", rep(" ", 5),
                                "lambda = .5", rep(" ", 5), 
                                "lambda = .6", rep(" ", 5), 
                                "lambda = .7", rep(" ", 5), 
                                "phi = .3", rep(" ", 5))


kbl(fig_a10_dat_wide %>% ungroup(),
    col.names = c("param.", "cats", rep(c("ML", "ULS"), 20))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 2, 
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2)) %>% 
  add_header_above(c(" " = 2, 
                     "Ext. Asym." = 8,
                     "Ext. Asym.-Alt" = 8,
                     "Mod. Asym." = 8,
                     "Mod. Asym.-Alt" = 8,
                     "Symmetric" = 8)) %>%
  footnote(general = "Mod.Asym = Moderate Asymmetry ;
           Mod.Asym-Alt = Moderate Asymmetry-Alternating;
           Ext. Asym = Extreme Asymmetry;
           Ext. Asym-Alt = Extreme Asymetry-Alternating;
           ML = robust normal-theory maximum likelihood;
           ULS = robust categorical least squares.")# %>% landscape() 

# A11 Efficiency, Model 1, Underlying Distribution = non-Normal ---------------

fig_a11_dat <- sim_fit_all_unnest %>% 
  dplyr::filter(models == "model1") %>% 
  dplyr::filter(dist == "non-normal") %>% 
  group_by(estimator, scenario_id, label) %>% 
  mutate(square_diff = (est - mean(est, na.rm = TRUE))^2) %>% 
  mutate(efficiency = sqrt(sum(square_diff, na.rm = TRUE)/(n()-1))) %>% 
  group_by(par_type, sym, N, estimator, cat) %>% 
  summarize(efficiency = mean(efficiency, na.rm = TRUE) %>% round(digits = 3))


fig_a11_dat_wide <- fig_a11_dat %>% 
  pivot_wider(names_from = c(sym, N, estimator),
              values_from = c(efficiency))

fig_a11_dat_wide$par_type <-  c("lambda = .3", rep(" ", 5), 
                                "lambda = .4", rep(" ", 5),
                                "lambda = .5", rep(" ", 5), 
                                "lambda = .6", rep(" ", 5), 
                                "lambda = .7", rep(" ", 5), 
                                "phi = .3", rep(" ", 5))


kbl(fig_a11_dat_wide %>% ungroup(),
    col.names = c("param.", "cats", rep(c("ML", "ULS"), 20))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 2, 
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2)) %>% 
  add_header_above(c(" " = 2, 
                     "Ext. Asym." = 8,
                     "Ext. Asym.-Alt" = 8,
                     "Mod. Asym." = 8,
                     "Mod. Asym.-Alt" = 8,
                     "Symmetric" = 8)) %>%
  footnote(general = "Mod.Asym = Moderate Asymmetry ;
           Mod.Asym-Alt = Moderate Asymmetry-Alternating;
           Ext. Asym = Extreme Asymmetry;
           Ext. Asym-Alt = Extreme Asymetry-Alternating;
           ML = robust normal-theory maximum likelihood;
           ULS = robust categorical least squares.")


# A12 Efficiency, Model 2, Underlying Distribution = Normal ---------------
fig_a12_dat <- sim_fit_all_unnest %>% 
  dplyr::filter(models == "model2") %>% 
  dplyr::filter(dist == "normal") %>% 
  group_by(estimator, scenario_id, label) %>% 
  mutate(square_diff = (est - mean(est, na.rm = TRUE))^2) %>% 
  mutate(efficiency = sqrt(sum(square_diff, na.rm = TRUE)/(n()-1))) %>% 
  group_by(par_type, sym, N, estimator, cat) %>% 
  summarize(efficiency = mean(efficiency, na.rm = TRUE) %>% round(digits = 3))


fig_a12_dat_wide <- fig_a12_dat %>% 
  pivot_wider(names_from = c(sym, N, estimator),
              values_from = c(efficiency))

fig_a12_dat_wide$par_type <-  c("lambda = .3", rep(" ", 5), 
                                "lambda = .4", rep(" ", 5),
                                "lambda = .5", rep(" ", 5), 
                                "lambda = .6", rep(" ", 5), 
                                "lambda = .7", rep(" ", 5), 
                                "phi = .3", rep(" ", 5))


kbl(fig_a12_dat_wide %>% ungroup(),
    col.names = c("param.", "cats", rep(c("ML", "ULS"), 20))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 2, 
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2)) %>% 
  add_header_above(c(" " = 2, 
                     "Ext. Asym." = 8,
                     "Ext. Asym.-Alt" = 8,
                     "Mod. Asym." = 8,
                     "Mod. Asym.-Alt" = 8,
                     "Symmetric" = 8)) %>%
  footnote(general = "Mod.Asym = Moderate Asymmetry ;
           Mod.Asym-Alt = Moderate Asymmetry-Alternating;
           Ext. Asym = Extreme Asymmetry;
           Ext. Asym-Alt = Extreme Asymetry-Alternating;
           ML = robust normal-theory maximum likelihood;
           ULS = robust categorical least squares.")# %>% landscape() 



# A13 Efficiency, Model 2, Underlying Distribution = non-Normal ---------------
fig_a13_dat <- sim_fit_all_unnest %>% 
  dplyr::filter(models == "model2") %>% 
  dplyr::filter(dist == "non-normal") %>% 
  group_by(estimator, scenario_id, label) %>% 
  mutate(square_diff = (est - mean(est, na.rm = TRUE))^2) %>% 
  mutate(efficiency = sqrt(sum(square_diff, na.rm = TRUE)/(n()-1))) %>% 
  group_by(par_type, sym, N, estimator, cat) %>% 
  summarize(efficiency = mean(efficiency, na.rm = TRUE) %>% round(digits = 3))


fig_a13_dat_wide <- fig_a13_dat %>% 
  pivot_wider(names_from = c(sym, N, estimator),
              values_from = c(efficiency))

fig_a13_dat_wide$par_type <-  c("lambda = .3", rep(" ", 5), 
                                "lambda = .4", rep(" ", 5),
                                "lambda = .5", rep(" ", 5), 
                                "lambda = .6", rep(" ", 5), 
                                "lambda = .7", rep(" ", 5), 
                                "phi = .3", rep(" ", 5))


kbl(fig_a13_dat_wide %>% ungroup(),
    col.names = c("param.", "cats", rep(c("ML", "ULS"), 20))) %>%
  kable_classic() %>% 
  add_header_above(c(" " = 2, 
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2,
                     "N = 100" = 2,
                     "N = 150" = 2,
                     "N = 350" = 2,
                     "N = 600" = 2)) %>% 
  add_header_above(c(" " = 2, 
                     "Ext. Asym." = 8,
                     "Ext. Asym.-Alt" = 8,
                     "Mod. Asym." = 8,
                     "Mod. Asym.-Alt" = 8,
                     "Symmetric" = 8)) %>%
  footnote(general = "Mod.Asym = Moderate Asymmetry ;
           Mod.Asym-Alt = Moderate Asymmetry-Alternating;
           Ext. Asym = Extreme Asymmetry;
           Ext. Asym-Alt = Extreme Asymetry-Alternating;
           ML = robust normal-theory maximum likelihood;
           ULS = robust categorical least squares.")