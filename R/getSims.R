#' Simulate model outcomes under various conditions
#'
#' This function simulates the outcomes of each condition under each supplied
#' model.
#'
#' @param its           Number of iterations to simulate for.
#' @param N             Vector of integers representing the number of cases for
#'   which data is simulated.
#' @param cat           A vector of integers between 2 and 7. Number of
#'   categories to divide the data into.
#' @param sym           Character vector containing one or more of
#'   \code{c("sym", "moderate", "extreme", "moderate-alt", "extreme-alt")}.
#'   Symmetry of the categorical divisions.
#' @param models        Vector containing \code{SimSem} model files. Can be
#'   generated with the \code{getModel} wrapper function.
#' @param dist          Vector of \code{SimDataDist} objects. Specifies the
#'   distributions used to draw the data from. Note that the categorization
#'   always assumes a normal distribution.
#' @param conditions    Tibble file containing all conditions for which a user
#'   wants to simulate. Bypasses the parameters \code{N}, \code{cat},
#'   \code{sym}, \code{models} and \code{dist}.
#' @export
getSims <- function(its = 100, 
                    N = 100,
                    cat = 5,
                    sym = "sym",
                    models,
                    dist = NULL,
                    conditions = NULL) {
  
  if (is.null(conditions)){
    conditions <- expand_grid(N = N, 
                              cat = cat, 
                              sym = sym, 
                              models = models,
                              dist = dist)
  }
}