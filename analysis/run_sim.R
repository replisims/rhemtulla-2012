library(tidyverse)
library(simsem)
#library(devtools)
source("R/getModel.R")
source("R/lavaan_model.R")
source("R/getSims.R")
source("R/gen_cat_data.R")
source("R/catData.R")
source("R/cfa_ML.R")
source("R/run_rep.R")
source("R/extract_par_est.R")


# Compile Simulation Scenarios --------------------------------------------

sim_scenarios <- getSims(its = 6,
                         N = c(100, 150, 350, 600),
                         cat = c(2, 3, 4, 5, 6, 7),
                         sym = c("sym", "moderate", "extreme", "moderate-alt", "extreme-alt" ),
                         models = c("model1", "model2"),
                         dist = c("normal", "non-normal"),
                         conditions = NULL)
sim_scenarios_id <- cbind(id = 1:nrow(sim_scenarios), sim_scenarios)

saveRDS(object = sim_scenarios_id,
        file = "sim_scenarios_id.rds")


# Generate data -----------------------------------------------------------

sim_reps50 <- 1:50 %>% map(~{run_rep(run_id = .x, 
                                       sim_scenarios = sim_scenarios_id)})
saveRDS(object = sim_reps50,
        file = "sim_reps50.rds")

sim_reps100 <- 51:100 %>% map(~{run_rep(run_id = .x, 
                                     sim_scenarios = sim_scenarios_id)})
saveRDS(object = sim_reps100,
        file = "sim_reps100.rds")

sim_reps150 <- 101:150 %>% map(~{run_rep(run_id = .x, 
                                        sim_scenarios = sim_scenarios_id)})
saveRDS(object = sim_reps150,
        file = "sim_reps150.rds")

sim_reps200 <- 151:200 %>% map(~{run_rep(run_id = .x, 
                                         sim_scenarios = sim_scenarios_id)})
saveRDS(object = sim_reps200,
        file = "sim_reps200.rds")


sim_reps250 <- 201:250 %>% map(~{run_rep(run_id = .x, 
                                         sim_scenarios = sim_scenarios_id)})
saveRDS(object = sim_reps250,
        file = "sim_reps250.rds")

sim_reps300 <- 251:300 %>% map(~{run_rep(run_id = .x, 
                                         sim_scenarios = sim_scenarios_id)})
saveRDS(object = sim_reps300,
        file = "sim_reps300.rds")

