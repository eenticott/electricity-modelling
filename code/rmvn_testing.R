library(sitmo)
library(Rcpp)
library(RcppArmadillo)
library(microbenchmark)
library(mvnfast)
library(MASS)
sourceCpp("code/rmvn_omp.cpp")


plot(rmvn(1000, c(1, 2), sigma = matrix(c(2,2,2,4), nrow = 2)))
plot(rmvn_omp(1000, c(1, 2), sigma = matrix(c(2,2,2,4), nrow = 2), seeds = c(1,2)))

microbenchmark(MASSmvrnorm = mvrnorm(1000000, c(1, 2), Sigma = matrix(c(2,2,2,4), nrow = 2)),
               rmvnomp1 = rmvn_omp(1000000, c(1, 2), sigma = matrix(c(2,2,2,4), nrow = 2), seeds = c(1)),
               rmvnfast1 = rmvn(1000000, c(1,2), sigma = matrix(c(2,2,2,4), nrow = 2), ncores = 1),
               rmvnomp2 = rmvn_omp(1000000, c(1, 2), sigma = matrix(c(2,2,2,4), nrow = 2), seeds = c(1,2)),
               rmvnfast2 = rmvn(1000000, c(1,2), sigma = matrix(c(2,2,2,4), nrow = 2), ncores = 2),
               rmvnomp4 = rmvn_omp(1000000, c(1, 2), sigma = matrix(c(2,2,2,4), nrow = 2), seeds = c(1,2,3,4)),
               rmvnfast4 = rmvn(1000000, c(1,2), sigma = matrix(c(2,2,2,4), nrow = 2), ncores = 4),
               rmvnomp8 = rmvn_omp(1000000, c(1, 2), sigma = matrix(c(2,2,2,4), nrow = 2), seeds = c(1,2,3,4,5,6,7,8)),
               rmvnfast8 = rmvn(1000000, c(1,2), sigma = matrix(c(2,2,2,4), nrow = 2), ncores = 8),
               unit = "relative",
               times = 50)

