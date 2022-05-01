#' Generate categorical data
#'
#' @param models 
#' @param N 
#' @param cat 
#' @param sym 
#' @param dist 
#' @param seed 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
getCatData <- function(models, N, cat, sym, dist, seed = 42, ...){
  variance <- 0
  pos_def <- TRUE
  
  while(variance == 0 | pos_def){
  set.seed(seed)
  gen_data <- simsem::generate(model = switch(models,
                                              "model1" = sim_model1,
                                              "model2" = sim_model2), 
                               n = N,
                               indDist = switch(dist,
                                                "normal" = NULL,
                                                "non-normal" = simsem::bindDist(skewness = 2, kurtosis = 7))
  )
  cat_data <- catData(gen_data, 
                      k = cat, 
                      sym = sym, 
                      standardize = FALSE)
  
  
  pos_def <- class(try(inspectSampleCov(model = get(models), 
                                data = cat_data), 
               silent = T)) == "try-error"
  
  variance <- cat_data %>% 
    summarize_all(.funs = var) %>% 
    t %>% min
  
  seed <- seed + 1000
  }
  
  cat_data
  
}