#' Simple Linear Regression Using Rcpp
#'
#' This function performs a simple linear regression analysis on numeric vectors x and y using an RcppArmadillo backend.
#' It validates the input, and then calls the C++ function `SimpLinCpp` to perform the computation.
#' 
#' @param x A numeric vector of predictor values.
#' @param y A numeric vector of response values.
#' 
#' @return A list containing regression coefficients, standard errors, confidence intervals, residuals, and fitted values.
#'
#' @examples
#' x <- 1:10
#' y <- 2 + 3 * x + rnorm(10)
#' result <- SimpLinR(x, y)
#' print(result)
#'
#' @export
#' @useDynLib SimpLin
#' @importFrom Rcpp sourceCpp
SimpLinR <- function(x, y) {
  # Input validation
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric vectors.")
  }
  
  if (length(x) != length(y)) {
    stop("x and y must be of the same length.")
  }
  
  # Call the C++ function
  result <- SimpLinCpp(x,y)
  
  return(result)
}
