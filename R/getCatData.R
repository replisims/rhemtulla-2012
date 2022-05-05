#' Generate categorical data
#'
#' @param models model file
#' @param N Amount of cases to generate
#' @param cat Number of categories
#' @param sym One of \code{c("sym", "moderate", "extreme", "moderate-alt",
#'   "extreme-alt")}. By default (\code{sym}), the data is divided into equally
#'   distanced cut-off points between -2.5 and 2.5. In \code{moderate}
#'   assymmetry, cut-offs are chosen so that the peak of a normal distribution
#'   falls on the left-hand side. \code{extreme} assymmetry results in a
#'   distribution where the first category has the highest number of cases.
#'   'alt' versions of the distributions invert the cut-off points.
#' @param dist Distribution of the data. One of \code{c("normal", "non-normal")}.
#' @param ... For passing along additional parameters
#'
#' @return
#' @export
#'
#' @examples getCatData()
getCatData <- function(models, N, cat, sym, dist, ...){
  variance <- 0
  pos_def <- TRUE
  
  while(variance == 0 | pos_def){
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
  
  }
  
  cat_data
  
}