# Read in the data --------------------------------------------------------


# sim_reps50 <- readRDS("sim_reps50.rds")
# sim_reps100 <- readRDS("sim_reps100.rds")
# sim_reps150 <- readRDS("sim_reps150.rds")
 sim_reps200 <- readRDS("sim_reps200.rds")
# sim_reps300 <- readRDS("sim_reps300.rds")

# Robust ML ---------------------------------------------------------------


fitML <- function(datalist){datalist %>% 
    map_df(~{.x$sim_data %>% 
        dplyr::select(id, models, cat_data) %>%  
        pmap_dfr(.f = function(id, models, cat_data){posscfa_ML(run_id = .x$run_id, 
                                                               id = id, 
                                                               models = models, 
                                                               cat_data = cat_data)})})
  }

sim_fitML50 <- fitML(sim_reps50)
saveRDS(object = sim_fitML50,
        file = "sim_fitML50.rds")

sim_fitML100 <- fitML(sim_reps100)
saveRDS(object = sim_fitML100,
        file = "sim_fitML100.rds")

sim_fitML150 <- fitML(sim_reps150)
saveRDS(object = sim_fitML150,
        file = "sim_fitML150.rds")

sim_fitML200 <- fitML(sim_reps200)
saveRDS(object = sim_fitML200,
        file = "sim_fitML200.rds")

sim_fitML250 <- fitML(sim_reps250)
saveRDS(object = sim_fitML250,
        file = "sim_fitML250.rds")


sim_fitML300 <- fitML(sim_reps300)
saveRDS(object = sim_fitML300,
        file = "sim_fitML300.rds")
# Robust ULS ------------------------------------------------------------

fitULS <- function(datalist){
  datalist %>% 
    map_df(~{.x$sim_data %>% 
        dplyr::select(id, models, cat_data) %>%  
        pmap_df(.f = function(id, models, cat_data){posscfa_ULS(run_id = .x$run_id, 
                                                                  id = id, 
                                                                  models = models, 
                                                                  cat_data = cat_data)})})
}

sim_fitULS50 <- fitULS(sim_reps50)
sim_fitULS100 <- fitULS(sim_reps100)
sim_fitULS150 <- fitULS(sim_reps150)
sim_fitULS200 <- fitULS(sim_reps200)
sim_fitULS250 <- fitULS(sim_reps250)
sim_fitULS300 <- fitULS(sim_reps300)


 saveRDS(object = sim_fitULS50,
         file = "sim_fitULS50.rds")

saveRDS(object = sim_fitULS100,
         file = "sim_fitULS100.rds")

saveRDS(object = sim_fitULS150,
        file = "sim_fitULS150.rds")

saveRDS(object = sim_fitULS200,
        file = "sim_fitULS200.rds")

saveRDS(object = sim_fitULS250,
        file = "sim_fitULS250.rds")

saveRDS(object = sim_fitULS300,
        file = "sim_fitULS300.rds")
# Not yet run -------------------------------------------------------------
