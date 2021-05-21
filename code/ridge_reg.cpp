// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]

#include <omp.h>
#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
using namespace arma;
  
// [[Rcpp::export]]
vec fit_rr(mat X, mat y, double lambda){
  double n = X.n_cols;
  mat XtX = X.t() * X;
  mat I; I.eye(n, n);
  vec beta = inv(XtX + (lambda * I)) * X.t() * y;
  
  return beta;
}

// [[Rcpp::export]]
double get_ocv(mat X, mat y, double lambda){
  double n = X.n_cols;
  mat XtX = X.t() * X;
  mat I; I.eye(n, n);
  mat Ad = diagvec(X * inv(XtX + (lambda * I)) * X.t());
  vec mu_hat =  X * inv(XtX + (lambda * I)) * X.t() * y;
  double out = sum(pow(y - mu_hat, 2)/pow(1 - Ad, 2));
  return out;
}

// [[Rcpp::export]]
vec optim_rr(mat X, mat y, vec lams){
  double n = lams.n_elem; 
  vec out(n);
  for (int i = 0; i < n; i++) {
    double lambda = lams[i];
    out[i] = get_ocv(X, y, lambda);
  }
  return out;
}



// defining an Rcpp function that uses openMP to parallelise over an index vector of groups 
// data is split, optim_rr run on each group and a matrix of betas returned (column per group)

// [[Rcpp::export]]
mat par_reg(mat X, mat y, vec lams, vec idx)
{
  // Initialise empty betas matrix
  vec groups = unique(idx);
  int ncol = groups.n_elem;
  int nrow = X.n_cols ;
  mat betas(nrow, ncol);
    
  #pragma omp parallel for
  for(int i=0; i<ncol; i++)
  {
  
    // subset the data
    int tod = groups[i];
    uvec rows = find(idx == tod);
    mat X_sub = X.rows(rows);
    mat y_sub = y.rows(rows);

    //use optim_rr to find the optimal lambda
    vec ocvs = optim_rr(X_sub, y_sub, lams);
    uword lam_id = ocvs.index_min(); 
    double opt_lam = lams(lam_id);
    
    //use fit_rr to get the corresponding betas
    vec beta = fit_rr(X_sub, y_sub, opt_lam);
    
    #pragma omp critical
    {
      betas.col(i) = beta;
    }
  }

  return betas;
}



