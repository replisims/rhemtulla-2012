cfa_ML <- function(run_id, id, models, cat_data){
  lav_fit <- lavaan::cfa(model = get(models),
                         data = cat_data,
                         std.lv = TRUE, 
                         orthogonal = FALSE,
                         estimator = "MLMV")
  #se ="robust",
  #test = c("Satorra.Bentler", "Yuan.Bentler", "Yuan.Bentler.Mplus", "Satterthwaite", "scaled.shifted"))
  
  print(c(run_id, id))
  
  parameter_est <- extract_par_est(lav_fit)
  
  
  tibble(rep = run_id,
         scenario_id = id,
         converged = lavInspect(object = lav_fit, 
                                what = "converged"),
         post_check = lavInspect(object = lav_fit, 
                                 what = "post.check"),
         parameter_est = list(parameter_est),
         test = list(lavInspect(object = lav_fit, 
                                what = "test")))

}

posscfa_ML <- possibly(.f = cfa_ML,
                       otherwise = NULL)


cfa_ULS <- function(run_id, id, models, cat_data){
  lav_fit <- lavaan::cfa(model = get(models),
                         data = cat_data,
                         std.lv = TRUE, 
                         orthogonal = FALSE,
                         estimator = "ULSMV",
                         #se = "robust",
                         #test = c("scaled.shifted"),
                         ordered = TRUE)
  
  print(c(run_id, id))
  
  
  parameter_est <- extract_par_est(lav_fit)
  
  tibble(rep = run_id,
         scenario_id = id,
         converged = lavInspect(object = lav_fit, 
                                what = "converged"),
         post_check = lavInspect(object = lav_fit, 
                                 what = "post.check"),
         parameter_est = list(parameter_est),
         test = list(lavInspect(object = lav_fit, 
                                what = "test")))
}

posscfa_ULS <- possibly(.f = cfa_ULS,
                        otherwise = NULL)