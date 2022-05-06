library(rhemtulla2012)

# Compile Simulation Scenarios --------------------------------------------

sim_scenarios <- getScenarios(its = 6,
                         N = c(100, 150, 350, 600),
                         cat = c(2, 3, 4, 5, 6, 7),
                         sym = c("sym", "moderate", "extreme", "moderate-alt", "extreme-alt" ),
                         models = c("model1", "model2"),
                         dist = c("normal", "non-normal"),
                         conditions = NULL)
sim_scenarios_id <- cbind(id = 1:nrow(sim_scenarios), sim_scenarios)


# Generate data -----------------------------------------------------------
set.seed(8361)
simreps <- 1:1000 %>% map(~{rhemtulla2012:::getSimData(run_id = .x, 
                                                       sim_scenarios = sim_scenarios_id)})

# Fit models
fitML <- function(datalist){datalist %>% 
                map_df(~{.x$sim_data %>% 
                                dplyr::select(id, models, cat_data) %>%  
                                pmap_dfr(.f = function(id, models, cat_data){posscfa_ML(run_id = .x$run_id, 
                                                                                        id = id, 
                                                                                        models = models, 
                                                                                        cat_data = cat_data)})})
}

set.seed(5731)
simfitML <- fitML(simreps)


sim_reps50 <- 1:50 %>% map(~{rhemtulla2012:::getSimData(run_id = .x, 
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

