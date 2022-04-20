
# Performance alternative -------------------------------------------------

fig_3_dat_alt <- sim_fit_all_unnest2_alt %>% 
  dplyr::filter(dist == "normal") %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type %in% c("l03", "l07")) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(sym, cat, estimator, N, par_type) %>% 
  summarize(est = mean(est, na.rm = TRUE))

fig_3_alt <- ggplot(fig_3_dat_alt) +
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

fig_4_dat_alt <- sim_fit_all_unnest2_alt %>% 
  dplyr::filter(dist == "non-normal") %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type %in% c("l03", "l07")) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(sym, cat, estimator, N, par_type) %>% 
  summarize(est = mean(est, na.rm = TRUE))

fig_4_alt <- ggplot(fig_4_dat_alt) +
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

fig_5_dat_alt <- sim_fit_all_unnest2_alt %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type == "s1") %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(dist, sym, cat, estimator, N) %>% 
  summarize(est = mean(est, na.rm = TRUE))

fig_5_alt <- ggplot(fig_5_dat_alt) +
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

sim_fit_cov_alt <- sim_fit_all_unnest2_alt %>% 
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


fig_6_dat_alt <- sim_fit_cov_alt %>% 
  dplyr::filter(dist == "normal") %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type %in% c("l03", "l07")) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(sym, cat, estimator, N, par_type) %>% 
  summarize(coverage = mean(covered, na.rm = TRUE))

fig_6_alt <- ggplot(fig_6_dat_alt) +
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

fig_7_dat_alt <- sim_fit_cov_alt %>% 
  dplyr::filter(dist == "non-normal") %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type %in% c("l03", "l07")) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(sym, cat, estimator, N, par_type) %>% 
  summarize(coverage = mean(covered, na.rm = TRUE))

fig_7_alt <- ggplot(fig_7_dat_alt) +
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

fig_8_dat_alt <- sim_fit_cov_alt %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  dplyr::filter(par_type == "s1") %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(dist, sym, cat, estimator, N) %>% 
  summarize(coverage = mean(covered, na.rm = TRUE))

fig_8_alt <- ggplot(fig_8_dat_alt) +
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
sim_fit_all_test_alt <- sim_fit_all_joined %>% 
  ungroup %>%
  ungroup() %>% 
  group_by(scenario_id, rep) %>% 
  mutate(exclude = (any(!converged) | any(!post_check))) %>% 
  filter(exclude == FALSE) %>% 
  dplyr::select(-c(parameter_est, converged, post_check, exclude)) %>% 
  mutate(robust_p = map_dbl(test, ~{.x$scaled.shifted$pvalue})) %>% 
  mutate(sig = robust_p < 0.05) %>% 
  dplyr::select(-test) #%>% 
# left_join(sim_scenarios_id, by = c("scenario_id" = "id"))


fig_9_dat_alt <- sim_fit_all_test_alt %>% 
  dplyr::filter(sym %in% c("sym", "extreme", "extreme-alt")) %>% 
  dplyr::filter(N %in% c(100, 600)) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "extreme", 
                                      "extreme-alt"))) %>% 
  group_by(dist, sym, cat, estimator, N) %>% 
  summarize(type_I = mean(sig, na.rm = TRUE))


fig_9_alt <- ggplot(fig_9_dat_alt) +
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


