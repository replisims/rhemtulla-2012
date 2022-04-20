# Generate categorical data for all scenarios ------------------------------
run_rep <- function(run_id, sim_scenarios){
  
  sim_data <- sim_scenarios %>% 
    rowwise() %>% 
    mutate(cat_data = list(gen_cat_data(models = models,
                                        N = N, 
                                        cat = cat, 
                                        sym = sym, 
                                        dist = dist,
                                        seed = run_id)))
  print(run_id)
  
  list(sim_data = sim_data,
       run_id = run_id)
}