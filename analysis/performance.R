sim_scenarios_id <- readRDS("sim_scenarios_id.rds")


# Collect outcomes --------------------------------------------------------

simfitML <- simfitML %>% 
  left_join(sim_scenarios_id, 
            by = c("scenario_id" = "id"))

simfitML <- tibble(estimator = "ML", simfitML)

sim_fitULS50 <- sim_fitULS50 %>% 
  left_join(sim_scenarios_id, 
            by = c("scenario_id" = "id"))

sim_fitULS50 <- tibble(estimator = "ULS", sim_fitULS50)

simfitALL <- bind_rows(simfitML, sim_fitULS50)

# sim_fit_all_joined <- sim_fit_all %>% 
#   ungroup %>% 
#   left_join(sim_scenarios_id, 
#             by = c("scenario_id" = "id"))

# saveRDS(sim_fit_all_joined, "sim_fit_all_joined.rds")

# Parameter estimates -----------------------------------------------------
lambda_03 <- c('b1', 'b6', 'b11', 'b16') 
lambda_04 <- c('b2', 'b7', 'b12', 'b17')
lambda_05 <- c('b3', 'b8', 'b13', 'b18')
lambda_06 <- c('b4', 'b9', "b14", "b19")
lambda_07 <- c("b5", "b10", "b15", "b20")


sim_fit_all_unnest <- simfitALL %>% 
  dplyr::filter(converged == TRUE) %>% 
  dplyr::filter(post_check == TRUE) %>% 
  dplyr::select(-c(test, converged, post_check)) %>%  
  group_by(estimator, rep, scenario_id) %>% 
  unnest(cols = parameter_est)

saveRDS(sim_fit_all_unnest, "sim_fit_all_unnest.rds")


sim_fit_all_unnest_alt <- sim_fit_all_joined %>% 
  ungroup() %>% 
  group_by(scenario_id, rep) %>% 
  mutate(exclude = (any(!converged) | any(!post_check))) %>% 
  filter(exclude == FALSE) %>% 
  dplyr::select(-c(test, converged, post_check, exclude)) %>%  
  group_by(estimator, rep, scenario_id) %>% 
  unnest(cols = parameter_est)
  
  
saveRDS(sim_fit_all_unnest_alt, "sim_fit_all_unnest_alt.rds")


sim_fit_all_unnest2 <- sim_fit_all_unnest %>% 
  mutate(par_type = case_when(label %in% lambda_03 ~ "l03",
                              label %in% lambda_04 ~ "l04",
                              label %in% lambda_05 ~ "l05",
                              label %in% lambda_06 ~ "l06",
                              label %in% lambda_07 ~ "l07",
                              label == "s1" ~ "s1"))

sim_fit_all_unnest2_alt <- sim_fit_all_unnest_alt %>% 
  mutate(par_type = case_when(label %in% lambda_03 ~ "l03",
                              label %in% lambda_04 ~ "l04",
                              label %in% lambda_05 ~ "l05",
                              label %in% lambda_06 ~ "l06",
                              label %in% lambda_07 ~ "l07",
                              label == "s1" ~ "s1"))


# Labels ------------------------------------------------------------------

sym_labels <- c("sym" = "Symmetry", 
                "extreme" = "Extreme Asymmetry",
                "extreme-alt" = "Extreme Asymmetry Alternating")

# dist_labels <- c()

par_labels <- c("l03" = "lambda = .3",
                "l07" = "lambda = .7")

# Figure 3 -----------------------------------------------------------------

fig_3_dat <- sim_fit_all_unnest2 %>% 
  dplyr::filter(dist == "normal") %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type %in% c("l03", "l07")) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(sym, cat, estimator, N, par_type) %>% 
  summarize(est = mean(est, na.rm = TRUE))

fig_3 <- ggplot(fig_3_dat) +
  aes(x = cat, 
      y = est, 
      color = factor(estimator), 
      linetype = factor(N), 
      shape = factor(par_type)) +
  geom_point(size = 1.5) +
  geom_line() +
  scale_x_continuous(name = "Number of Categories") +
  scale_y_continuous(name = "Average Estimated Parameter Value",
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) +
  scale_color_manual(values = c("grey70", "black")) +
  scale_shape_manual(values = c(16, 17),
                     labels = c("lambda = 0.3", "lambda = 0.7")) +
  scale_linetype_manual(values = c("dashed", "solid"),
                     labels = c("N = 100", "N = 600")) +
  facet_wrap(vars(sym),
             labeller = labeller(sym = sym_labels)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "black",
                                          size = 0.5),
        strip.background = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_blank())



# Figure 4 ----------------------------------------------------------------

fig_4_dat <- sim_fit_all_unnest2 %>% 
  dplyr::filter(dist == "non-normal") %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type %in% c("l03", "l07")) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(sym, cat, estimator, N, par_type) %>% 
  summarize(est = mean(est, na.rm = TRUE))

fig_4 <- ggplot(fig_4_dat) +
  aes(x = cat, 
      y = est, 
      color = factor(estimator), 
      linetype = factor(N), 
      shape = factor(par_type)) +
  geom_point(size = 1.5) +
  geom_line() +
  scale_x_continuous(name = "Number of Categories") +
  scale_y_continuous(name = "Average Estimated Parameter Value",
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) +
  scale_color_manual(values = c("grey70", "black")) +
  scale_shape_manual(values = c(16, 17),
                     labels = c("lambda = 0.3", "lambda = 0.7")) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("N = 100", "N = 600")) +
  facet_wrap(vars(sym),
             labeller = labeller(sym = sym_labels)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "black",
                                          size = 0.5),
        strip.background = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_blank())



# Figure 5 ----------------------------------------------------------------

fig_5_dat <- sim_fit_all_unnest2 %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type == "s1") %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(dist, sym, cat, estimator, N) %>% 
  summarize(est = mean(est, na.rm = TRUE))

fig_5 <- ggplot(fig_5_dat) +
  aes(x = cat, 
      y = est, 
      color = factor(estimator), 
      linetype = factor(N), 
      shape = factor(estimator)) +
  geom_point(size = 1.5) +
  geom_line() +
  scale_x_continuous(name = "Number of Categories") +
  scale_y_continuous(name = "Average Estimated Parameter Value",
                     breaks = c(0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55)) +
  scale_color_manual(values = c("grey70", "black")) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("N = 100", "N = 600")) +
  facet_wrap(dist ~ sym,
             labeller = labeller(sym = sym_labels)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "black",
                                          size = 0.5),
        strip.background = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_blank())

# Coverage ----------------------------------------------------------------

sim_fit_cov <- sim_fit_all_unnest2 %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(true_par = case_when(par_type == "l03" ~ 0.3,
                              par_type == "l04" ~ 0.3,
                              par_type == "l05" ~ 0.5,
                              par_type == "l06" ~ 0.6,
                              par_type == "l07" ~ 0.7,
                              par_type == "s1" ~ 0.3)) %>% 
  mutate(covered = (true_par > (est - 1.96 * se)) & (true_par < (est + 1.96 * se)))



# Figure 6 ----------------------------------------------------------------


fig_6_dat <- sim_fit_cov %>% 
  dplyr::filter(dist == "normal") %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type %in% c("l03", "l07")) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(sym, cat, estimator, N, par_type) %>% 
  summarize(coverage = mean(covered, na.rm = TRUE))

fig_6 <- ggplot(fig_6_dat) +
  aes(x = cat, 
      y = coverage, 
      color = factor(estimator), 
      linetype = factor(N), 
      shape = factor(par_type)) +
  geom_point(size = 1.5) +
  geom_line() +
  scale_x_continuous(name = "Number of Categories") +
  scale_y_continuous(name = "95% Confidence Interval Coverage",
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  scale_color_manual(values = c("grey70", "black")) +
  scale_shape_manual(values = c(16, 17),
                     labels = c("lambda = 0.3", "lambda = 0.7")) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("N = 100", "N = 600")) +
  facet_wrap(par_type ~ sym,
             labeller = labeller(sym = sym_labels)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "black",
                                          size = 0.5),
        strip.background = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_blank())




# Figure 7 ----------------------------------------------------------------

fig_7_dat <- sim_fit_cov %>% 
  dplyr::filter(dist == "non-normal") %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type %in% c("l03", "l07")) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(sym, cat, estimator, N, par_type) %>% 
  summarize(coverage = mean(covered, na.rm = TRUE))

fig_7 <- ggplot(fig_7_dat) +
  aes(x = cat, 
      y = coverage, 
      color = factor(estimator), 
      linetype = factor(N), 
      shape = factor(par_type)) +
  geom_point(size = 1.5) +
  geom_line() +
  scale_x_continuous(name = "Number of Categories") +
  scale_y_continuous(name = "95% Confidence Interval Coverage",
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  scale_color_manual(values = c("grey70", "black")) +
  scale_shape_manual(values = c(16, 17),
                     labels = c("lambda = 0.3", "lambda = 0.7")) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("N = 100", "N = 600")) +
  facet_wrap(par_type ~ sym,
             labeller = labeller(sym = sym_labels)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "black",
                                          size = 0.5),
        strip.background = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_blank())



# Figure 8 ----------------------------------------------------------------

fig_8_dat <- sim_fit_cov %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type == "s1") %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(dist, sym, cat, estimator, N) %>% 
  summarize(coverage = mean(covered, na.rm = TRUE))

fig_8 <- ggplot(fig_8_dat) +
  aes(x = cat, 
      y = coverage, 
      color = factor(estimator), 
      linetype = factor(N), 
      shape = factor(estimator)) +
  geom_point(size = 1.5) +
  geom_line() +
  scale_x_continuous(name = "Number of Categories") +
  scale_y_continuous(name = "95% Confidence Interval Coverage",
                     breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  scale_color_manual(values = c("grey70", "black")) +
  scale_shape_manual(values = c(16, 17),
                     labels = c("lambda = 0.3", "lambda = 0.7")) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("N = 100", "N = 600")) +
  facet_wrap(dist ~ sym,
             labeller = labeller(sym = sym_labels)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "black",
                                          size = 0.5),
        strip.background = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_blank())

# Type I Error ------------------------------------------------------------

# Figure 9 ----------------------------------------------------------------
sim_fit_all_test <- sim_fit_all_joined %>% 
  ungroup %>%
  dplyr::filter(converged) %>% 
  dplyr::filter(post_check) %>% 
  dplyr::select(-c(parameter_est, converged, post_check)) %>% 
  mutate(robust_p = map_dbl(test, ~{.x$scaled.shifted$pvalue})) %>% 
  mutate(sig = robust_p < 0.05) %>% 
  dplyr::select(-test) #%>% 
 # left_join(sim_scenarios_id, by = c("scenario_id" = "id"))


fig_9_dat <- sim_fit_all_test %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(dist, sym, cat, estimator, N) %>% 
  summarize(type_I = mean(sig, na.rm = TRUE))


fig_9 <- ggplot(fig_9_dat) +
  aes(x = cat, 
      y = type_I, 
      color = factor(estimator), 
      linetype = factor(N), 
      shape = factor(estimator)) +
  geom_point(size = 1.5) +
  geom_line() +
  scale_y_continuous(name = "Type I Error Rate of Robust Test Statistic",
                     breaks = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40)) +
  scale_color_manual(values = c("grey70", "black")) +
  scale_shape_manual(values = c(16, 17)) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("N = 100", "N = 600")) +
  theme_classic() +
  facet_wrap(dist ~ sym,
             labeller = labeller(sym = sym_labels)) +
  theme(panel.grid.major.y = element_line(color = "black",
                                          size = 0.5),
        strip.background = element_blank(),
        axis.title = element_text(size = 10),
        legend.title = element_blank())
  





