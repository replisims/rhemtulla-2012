
# Figure no outliers ------------------------------------------------------

sim_fit_all_unnest_no_out <- sim_fit_all_unnest_no_out %>% 
  mutate(par_type = case_when(label %in% lambda_03 ~ "l03",
                              label %in% lambda_04 ~ "l04",
                              label %in% lambda_05 ~ "l05",
                              label %in% lambda_06 ~ "l06",
                              label %in% lambda_07 ~ "l07",
                              label == "s1" ~ "s1"))



# Coverage ----------------------------------------------------------------

sim_fit_cov <- sim_fit_all_unnest_no_out %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(true_par = case_when(par_type == "l03" ~ 0.3,
                              par_type == "l04" ~ 0.4,
                              par_type == "l05" ~ 0.5,
                              par_type == "l06" ~ 0.6,
                              par_type == "l07" ~ 0.7,
                              par_type == "s1" ~ 0.3)) %>% 
  mutate(covered = (true_par > (est - 1.96 * se)) & (true_par < (est + 1.96 * se)))

saveRDS(sim_fit_cov, "sim_fit_cov_no_out.rds")
# Figure 3 -----------------------------------------------------------------

fig_3_dat <- sim_fit_all_unnest_no_out %>% 
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

# Save plot
ggsave("./analysis/Replication Report Rhemthulla et al 2012/figures/fig_3_no_out.png",
       plot = fig_3,
       device = "png",
       scale = 1,
       dpi = 300,
       limitsize = TRUE)

