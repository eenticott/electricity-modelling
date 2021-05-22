library(sitmo)
library(Rcpp)
library(RcppArmadillo)
library(microbenchmark)
library(mvnfast)
sourceCpp("code/rmvn_omp.cpp")


plot(rmvn(1000, c(1, 2), sigma = matrix(c(2,2,2,4), nrow = 2)))
plot(rmvn_omp(1000, c(1, 2), sigma = matrix(c(2,2,2,4), nrow = 2), seeds = c(1,2)))

microbenchmark(rmvnomp1 = rmvn_omp(1000000, c(1, 2), sigma = matrix(c(2,2,2,4), nrow = 2), seeds = c(1)),
               rmvnfast1 = rmvn(1000000, c(1,2), sigma = matrix(c(2,2,2,4), nrow = 2), ncores = 1),
               rmvnomp2 = rmvn_omp(1000000, c(1, 2), sigma = matrix(c(2,2,2,4), nrow = 2), seeds = c(1,2)),
               rmvnfast2 = rmvn(1000000, c(1,2), sigma = matrix(c(2,2,2,4), nrow = 2), ncores = 2),
               unit = "relative",
               times = 100)
