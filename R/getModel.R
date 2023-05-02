#' Generate a CFA model
#'
#' A wrapper function around \code{model} from the package \code{simsem}.
#' Creates data from a CFA model with normal or non-normal data.
#'
#' @param loadings      Matrix with number of rows equal to the number of
#'   observed variables, and number of columns equal to the number of latent
#'   factors. Values specified are fixed factor loadings in the model. \code{NA}
#'   values indicate freed parameters, and will be estimated.
#' @param population    Matrix with identical dimensions to the loadings matrix.
#'   Indicates population values for data generation.
#' @param latent.cor    Vector of latent factor correlations in the population
#'   (estimated by default).
#' @examples
#' # generate a matrix from a 2 factor CFA model with 10 observed variables.
#' loadings <- matrix(0, 10, 2)
#' loadings[1:5, 1] <- NA
#' loadings[6:10, 2] <- NA
#' pop <- matrix(0, 10, 2)
#' pop[1:5, 1] <- c(.3, .4, .5, .6, .7)
#' pop[6:10, 2] <- c(.3, .4, .5, .6, .7)
#'
#' data <- getModel(loadings = loadings, population = pop, latent.cor = 0.3)
#' @export
getModel <- function(loadings = NULL,
                    population = NULL,
                    latent.cor = NULL){

  if (is.null(loadings) | is.null(population) | is.null(latent.cor))
    stop("Specify the loadings, population and latent.cor, or supply a SimSem model file")
  if (nrow(loadings) != nrow(population) | ncol(loadings) != ncol(population))
    stop ("Dimenions for loadings and population not equal")
  if (ncol(loadings)-1 != length(latent.cor))
    stop("Number of latent correlations needs to be equal to the number of columns of Lambda")

  # Factor loadings
  Lambda <- bind(loadings, population)

  # Factor correlation
  Psi <- matrix(NA, ncol(loadings), ncol(loadings))
  diag(Psi) <- 1
  RPS <- binds(Psi, latent.cor)

  # Error correlation
  error.cor <- matrix(0, nrow(loadings), nrow(loadings))
  diag(error.cor) <- 1
  RTE <- binds(error.cor)

  model <- model(LY = Lambda, RPS = RPS, RTE = RTE, modelType = "CFA")
  return(model)
}
