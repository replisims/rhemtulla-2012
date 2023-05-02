# Generate categorical data for all scenarios ------------------------------
getSimData <- function(run_id, sim_scenarios){
  
  sim_data <- sim_scenarios %>% 
    rowwise() %>% 
    mutate(cat_data = list(getCatData(models = models,
                                        N = N, 
                                        cat = cat, 
                                        sym = sym, 
                                        dist = dist)))
  print(run_id)
  
  list(sim_data = sim_data,
       run_id = run_id)
}