
library(tidyverse)
library(simsem)
source("R/getSims.R")
source("R/getModel.R")
source("R/gen_cat_data.R")
source("R/lavaan_model.R")
source("R/run_rep.R")
source("R/catData.R")

# Compile Scenarios for Fig 1 & 2--------------------------------------------

scenarios_fig12 <- getSims(its = 6,
                           N = c(1000000),
                           cat = c(2, 3, 4, 5, 6, 7),
                           sym = c("sym", "moderate", "extreme", "moderate-alt", "extreme-alt" ),
                           models = c("model1", "model2"),
                           dist = c("normal", "non-normal"),
                           conditions = NULL)
scenarios_fig12_id <- cbind(id = 1:nrow(scenarios_fig12), scenarios_fig12)

# Figure data ----------------------------------------------------------------

fig12_dat <- run_rep(run_id = 1, 
                     sim_scenarios = scenarios_fig12_id)

saveRDS(object = fig12_dat,
        file = "fig12_dat.rds")

# fig12_dat <- readRDS("fig12_dat.rds")

dump_data1 <- fig12_dat$sim_data %>% 
  dplyr::filter(dist == "normal") %>% 
  dplyr::filter(sym %in% c("sym", "moderate", "extreme")) %>%
  dplyr::select(sym, cat, models, cat_data) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "moderate", 
                                      "extreme"))) %>% 
  group_by(sym, cat) %>% 
  unnest(cols = c(cat_data))


dump_data1_count <- list("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9", "y10") %>% 
  map_dfc(~{
    col <-  rlang::parse_expr(.x)
    dump_data1 %>% 
      count(!!col)})

dump_data1sum <- dump_data1_count %>% rowwise() %>% 
  mutate(n_all = sum(c_across(starts_with("n..."))))


# Date prep for figure 2 --------------------------------------------------

dump_data2mod1 <- fig12_dat$sim_data %>% 
  dplyr::filter(dist == "non-normal") %>% 
  dplyr::filter(models == "model1") %>% 
  dplyr::select(sym, cat, models, cat_data) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "moderate",
                                      "moderate-alt",
                                      "extreme",
                                      "extreme-alt"))) %>% 
  group_by(sym, cat) %>% 
  unnest(cols = c(cat_data))

dump_data2_countmod1 <- list("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9", "y10") %>% 
  map_dfc(~{
    col <-  rlang::parse_expr(.x)
    dump_data2mod1 %>% 
      count(!!col)})

dump_data2summod1 <- dump_data2_countmod1 %>% rowwise() %>% 
  mutate(n_all = sum(c_across(starts_with("n..."))))



dump_data2mod2 <- fig12_dat$sim_data %>% 
  dplyr::filter(dist == "non-normal") %>% 
  dplyr::filter(models == "model2") %>% 
  dplyr::select(sym, cat, models, cat_data) %>% 
  mutate(sym = factor(sym, levels = c("sym", 
                                      "moderate",
                                      "moderate-alt",
                                      "extreme",
                                      "extreme-alt"))) %>% 
  group_by(sym, cat) %>% 
  unnest(cols = c(cat_data))

dump_data2_countmod2 <- list("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9", "y10") %>% 
  map_dfc(~{
    col <-  rlang::parse_expr(.x)
    dump_data2mod2 %>% 
      count(!!col)})

dump_data2summod2 <- dump_data2_countmod2 %>% rowwise() %>% 
  mutate(n_all = sum(c_across(starts_with("n..."))))

data_fig2 <- cbind(dump_data2summod1$sym...1, dump_data2summod1$cat...2, dump_data2summod1 )

# Figure 1 ----------------------------------------------------------------

brks <- c(0, 0.20, 0.40, 0.60, 0.80, 1)

fig1_dat <- dump_data1sum %>% 
  group_by(sym...1, cat...2) %>%
  mutate(perc = n_all/sum(n_all))

fig1 <- fig1_dat %>% 
  ggplot(aes(x = factor(y1), y = perc)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  facet_grid(sym...1 ~ cat...2) +
  theme_classic()

# Figure 2 ----------------------------------------------------------------

fig2_dat <- dump_data %>% 
  ungroup %>% 
  dplyr::filter(dist == "non-normal") %>% 
  group_by(sym, cat, x) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))


fig2 <- fig2_dat %>% 
  ggplot(aes(x = factor(x), y = perc)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  facet_grid(sym ~ cat) +
  theme_minimal()
