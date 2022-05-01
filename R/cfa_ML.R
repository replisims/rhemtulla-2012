#' Run the ML models
#' 
#' Runs models using Maximum Likelihood (ML) parameter estimation.
#' 
#' @param run_id unique identifier for each run
#' @param id unique identifier for each specified model
#' @param models model file
#' @param cat_data categorical data set
cfa_ML <- function(run_id, id, models, cat_data){
  # Fit model with ML estimator
  lav_fit <- lavaan::cfa(model = get(models),
                         data = cat_data,
                         std.lv = TRUE, 
                         orthogonal = FALSE,
                         estimator = "MLMV")

  # This is for printing updates
  print(c(run_id, id))
  
  # Extract parameter estimates from fitted model
  parameter_est <- getEstimates(lav_fit)
  
  tibble(rep = run_id,
         scenario_id = id,
         converged = lavInspect(object = lav_fit, 
                                what = "converged"),
         post_check = lavInspect(object = lav_fit, 
                                 what = "post.check"),
         parameter_est = list(parameter_est),
         test = list(lavInspect(object = lav_fit, 
                                what = "test")))
} # end

# Wrapper
posscfa_ML <- possibly(.f = cfa_ML,
                       otherwise = NULL)


#' Run the ULS models
#' 
#' Runs models using Unweighted Least Squares (ULS) parameter estimation.
#' 
#' @param run_id unique identifier for each run
#' @param id unique identifier for each specified model
#' @param models model file
#' @param cat_data categorical data set
cfa_ULS <- function(run_id, id, models, cat_data){
  # Fit model with ULS estimator
  lav_fit <- lavaan::cfa(model = get(models),
                         data = cat_data,
                         std.lv = TRUE, 
                         orthogonal = FALSE,
                         estimator = "ULSMV",
                         #se = "robust",
                         #test = c("scaled.shifted"),
                         ordered = TRUE)
  
  # This is for printing updates
  print(c(run_id, id))
  
  # Extract parameter estimates from fitted model
  parameter_est <- getEstimates(lav_fit)
  
  tibble(rep = run_id,
         scenario_id = id,
         converged = lavInspect(object = lav_fit, 
                                what = "converged"),
         post_check = lavInspect(object = lav_fit, 
                                 what = "post.check"),
         parameter_est = list(parameter_est),
         test = list(lavInspect(object = lav_fit, 
                                what = "test")))
} # end

# Wrapper
posscfa_ULS <- possibly(.f = cfa_ULS,
                        otherwise = NULL)