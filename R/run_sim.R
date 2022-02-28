
# Questions for Arjan -----------------------------------------------------
# - What is the N in getModel for?


# Define model 1 ----------------------------------------------------------
# library(tidyverse)
# library(simsem)
#library(devtools)


loadings <- matrix(0, 10, 2)
loadings[1:5, 1] <- NA
loadings[6:10, 2] <- NA
pop <- matrix(0, 10, 2)
pop[1:5, 1] <- c(.3, .4, .5, .6, .7)
pop[6:10, 2] <- c(.3, .4, .5, .6, .7)

sim_model1 <- getModel(N = 100, 
                       loadings = loadings, 
                       population = pop, 
                       latent.cor = 0.3)

# Compile Simulation Scenarios --------------------------------------------

sim_scenarios <- getSims(its = 6,
                         N = c(100, 150, 350, 600),
                         cat = c(2, 3, 4, 5, 6, 7),
                         sym = c("sym", "moderate", "extreme", "moderate-alt", "extreme-alt" ),
                         models = c(sim_model1),
                         dist = c("normal", "non-normal"),
                         conditions = NULL)
sim_scenarios_id <- cbind(id = 1:nrow(sim_scenarios), sim_scenarios)


# Generate categorical data for all scenarios ------------------------------
run_rep <- function(sim_scenarios, sim_model1){
sim_raw <- sim_scenarios_id %>% 
  purrr::pmap(gen_cat_data) %>% set_names(sim_scenarios_id$id)
                                          


sim_raw_3 <- sim_scenarios_id %>% 
  rowwise() %>% 
  mutate(cat_data = list(gen_cat_data(models = models, 
                                      N = N, 
                                      cat = cat, 
                                      sym = sym, 
                                      dist = dist,
                                      seed = id)))

# Estimate model on simulated data

sim_raw_res <- simsem::sim(model = sim_model1, rawData = sim_raw_2$cat_data)


# Extract model coefficients from estimated models
sim_coef <- coef(sim_raw_res)


# Add id variable based on row names --------------------------------------

sim_coef_id <- sim_coef %>% tibble::rownames_to_column(var = "id") %>% mutate(id = as.numeric(id))

# Join scenarios and estimated coefficients data frame --------------------

sim_coef_df <- dplyr::left_join(sim_scenarios_id, sim_coef_id)

vars_01 <- c("f1=~y1", "f2=~y6")
vars_02 <- c("f1=~y5", "f2=~y10")

aggregate_coef <- sim_coef_df %>% 
  dplyr::select(`f1=~y1`,`f2=~y6`, `f1=~y5`, `f2=~y10`, N, sym, cat, dist) %>% rowwise() %>% 
  dplyr::mutate(lambda_03 = mean(c(`f1=~y1`, `f2=~y6`), na.rm = TRUE)) %>%   
  dplyr::group_by(N, cat, sym, dist) %>% mutate(lambda_03_bar = mean(lambda_03, na.rm = TRUE)) %>% 
  mutate(bias_03 = lambda_03_bar - 0.3) %>% ungroup()

aggregate_coef
}


sim_reps <- 1:6 %>% map(~{run_rep(sim_scenarios, sim_model1)})

sim_reps_agg <- sim_reps %>% dplyr::group_by(N, cat, sym, dist) %>% summarize(avg_bias = mean(bias_03, na.rm = TRUE))

# sim_plot <- ggplot2::ggplot(data = sim_coef_df_filtered) +
#   ggplot2::geom_point(ggplot2::aes(x = cat, y = lambda_03 ))

View(sim_reps_agg)
