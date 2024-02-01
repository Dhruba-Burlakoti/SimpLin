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
