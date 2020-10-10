#' Categorize continuous data
#'
#' Transforms continuous data into categorical data, based on Z-score cut-off
#' points.
#'
#' @param data    A numerical data.frame, matrix or vector to be categorized.
#' @param k       The number of categories to divide the data into. An integer
#'   between 2 and 7.
#' @param asym    One of \code{c("sym", "moderate", "extreme", "moderate-alt",
#'   "extreme-alt")}. By default (\code{sym}), the data is divided into equally
#'   distanced cut-off points between -2.5 and 2.5. In \code{moderate}
#'   asymmetry, cut-offs are chosen so that the peak of a normal distribution
#'   falls on the left-hand side. \code{extreme} asymmetry results in a
#'   distribution where the first category has the highest number of cases.
#'   'alt' versions of the distributions invert the cut-off points.
#' @param standardize   Boolean. Should parameters be standardized before
#'   transformation?
#' @export
catData <- function(data, k = 4, asym = "sym", standardize = FALSE){
  # error checks
  if (k < 2 | k > 7) stop("k needs to be an integer between 2 and 7")
  if (!asym %in% c("sym", "moderate", "moderate-alt", "extreme", "extreme-alt"))
    stop("sym parameter needs to be one of: sym, moderate, moderate-alt, extreme, extreme-alt")

  # standardize if needed
  if (standardize) data <- data/apply(data, 2, function(x) sd(x, na.rm = TRUE))

  # get the cut-off points
  if (asym == "sym"){
    sds <- seq(-2.5, 2.5, by = 5/k)
    sds[1] <- -Inf
    sds[k+1] <- Inf
  } else if (grepl("moderate", asym)) {
    sds <- switch(k-1,
                  '2' = c(-Inf, 0.36, Inf),
                  '3' = c(-Inf, -0.50, 0.76, Inf),
                  '4' = c(-Inf, -0.31, 0.79, 1.66, Inf),
                  '5' = c(-Inf, -0.70, 0.39, 1.16, 2.05, Inf),
                  '6' = c(-Inf, -1.05, 0.08, 0.81, 1.44, 2.33, Inf),
                  '7' = c(-Inf, -1.43, -0.43, 0.38, 0.94, 1.44, 2.54, Inf))
  } else if (grepl("extreme", asym)) {
    sds <- switch(k-1,
                  '2' = c(-Inf, 1.04, Inf),
                  '3' = c(-Inf, 0.58, 1.13, Inf),
                  '4' = c(-Inf, 0.28, 0.71, 1.23, Inf),
                  '5' = c(-Inf, 0.05, 0.44, 0.84, 1.34, Inf),
                  '6' = c(-Inf, -0.13, 0.25, 0.61, 0.99, 1.48, Inf),
                  '7' = c(-Inf, -0.25, 0.13, 0.47, 0.81, 1.18, 1.64, Inf))
  }
  # if alt, reverse all values
  if(grepl("alt", asym)) sds <- sort(sds*-1)

  # apply cut-offs
  as.data.frame(lapply(data, function(x) cut(x, breaks = sds, labels = FALSE)))
}
