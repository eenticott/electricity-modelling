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
SEXP optim_rr(arma::mat X, arma::mat y, arma::vec lams){
  //arma::vec lams = arma::logspace(-5, 5, 20);
  double n = lams.n_elem; 
  arma::vec out(n);
  for (int i = 0; i < n; i++) {
    double lambda = lams[i];
    out[i] = get_ocv(X, y, lambda);
  }
  return Rcpp::wrap(out);
}




