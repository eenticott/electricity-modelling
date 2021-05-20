// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]

#include <omp.h>
#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
  
// [[Rcpp::export]]
SEXP fit_rr(arma::mat X, arma::mat y, double lambda){
  double n = X.n_cols;
  arma::mat XtX = X.t() * X;
  arma::mat I; I.eye(n, n);
  arma::vec beta = inv(XtX + (lambda * I)) * X.t() * y;
  
  return Rcpp::wrap(beta);
}

// [[Rcpp::export]]
arma::vec diag_get(const arma::mat& X)   // note the 'const' and '&'
{
  return X.diag();
}

// [[Rcpp::export]]
double get_ocv(arma::mat X, arma::mat y, double lambda){
  double n = X.n_cols;
  arma::mat XtX = X.t() * X;
  arma::mat I; I.eye(n, n);
  arma::mat Ad = arma::diagvec(X * inv(XtX + (lambda * I)) * X.t());
  arma::vec mu_hat =  X * inv(XtX + (lambda * I)) * X.t() * y;
  double out = sum(pow(y - mu_hat, 2)/pow(1 - Ad, 2));
  return out;
}

// [[Rcpp::export]]
SEXP optim_rr(arma::mat X, arma::mat y){
  double n = X.n_cols;
  int i = 0;
  arma::vec lams = arma::logspace(-5, 5, 10);
  arma::vec out;
  for (double lambda : lams) {
    out[i] = get_ocv(X, y, lambda);
  }
  return Rcpp::wrap(out);
}




