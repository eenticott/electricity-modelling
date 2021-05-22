// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(sitmo)]]

#define BOOST_DISABLE_ASSERTS true

#include <boost/random/normal_distribution.hpp>
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <sitmo.h>
#ifdef _OPENMP
#include <omp.h>
#endif

using namespace arma;

// [[Rcpp::export]]
SEXP rmvn_omp(unsigned int n,
                                      vec mu,
                                      mat sigma,
                                      Rcpp::NumericVector seeds) {

  unsigned int d = sigma.n_cols;
  unsigned int ncores = seeds.size();
  
  mat cholDec = trimatu( chol(sigma));
  mat A(n, d);
  mat out(n, d);
  #ifdef _OPENMP
  #pragma omp parallel num_threads(ncores)
  {
  #endif
  
   uint32_t coreseed = static_cast<uint32_t>(seeds[0]);
   
   #ifdef _OPENMP
    coreseed = static_cast<uint32_t>(seeds[omp_get_thread_num()]);
   #endif
   
   sitmo::prng_engine engine( coreseed );
   boost::normal_distribution<> normal(0.0, 1.0);
   
   #ifdef _OPENMP
    #pragma omp for schedule(static)
   #endif
   for (unsigned int irow = 0; irow < n; irow++) 
     for (unsigned int icol = 0; icol < d; icol++) 
         A(irow, icol) = normal(engine);


  #ifdef _OPENMP
  }
  #endif
  
  out = repmat(mu, 1, n).t() + (A * cholDec);


  return Rcpp::wrap(out);
}
