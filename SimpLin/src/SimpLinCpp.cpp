#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
List SimpLinCpp(NumericVector x, NumericVector y) {
  int n = x.size();
  mat X(n, 2);
  X.col(0).fill(1); // Fill the first column with 1s for the intercept
  X.col(1) = as<vec>(x);
  
  vec Y = as<vec>(y);
  vec beta = solve(X, Y); // Solves X*beta = Y
  
  vec residuals = Y - X * beta;
  vec predicted = X * beta;
  
  // Correct variance calculation for standard errors
  double residuals_variance = sum(square(residuals)) / (n - X.n_cols);
  mat covMat = inv(X.t() * X) * residuals_variance;
  vec se = sqrt(covMat.diag());
  
  // Confidence intervals
  vec lower = beta - 1.96 * se;
  vec upper = beta + 1.96 * se;
  
  return List::create(Named("coefficients") = beta,
                      Named("stderr") = se,
                      Named("confint_lower") = lower,
                      Named("confint_upper") = upper,
                      Named("residuals") = residuals,
                      Named("fitted_values") = predicted);
}
