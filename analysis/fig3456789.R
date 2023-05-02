
# Bias --------------------------------------------------------------------

ggplot(sim_fit_all_unnest %>% dplyr::filter(dist == "normal")) +
  aes(x = cat, y = avg_bias, color = factor(N)) +
  geom_point(shape = "circle", size = 1.5) +
  geom_line() +
  theme_minimal() +
  facet_wrap(vars(sym))


# Parameter estimate underlying distribution normal -----------------------

ggplot(sim_reps_agg %>% filter(dist == "normal") %>% filter(N %in% c(100,600))) +
  aes(x = cat, y = avg_parameter, color = factor(N)) +
  geom_point(shape = "circle", size = 1.5) +
  geom_line() +
  theme_minimal() +
  facet_wrap(vars(sym))