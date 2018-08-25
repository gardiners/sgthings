# Functions for working with multivariate normal distributions.

#' Generate random vectors from a multivariate normal distribution.
#'
#' @param n Number of random vectors to return.
#' @param mu The mean vector.
#' @param sigma The variance-covariance matrix.
#' @return A matrix of \code{n} rows, each drawn from the MVN distribution with parameters \code{mu} and \code{sigma}.
#'
rmvnorm <- function(n, mu, sigma){
  # @TODO: dimension checking.
  p <- length(mu)
  E <- eigen(sigma)
  A <- E$vectors %*% diag(sqrt(E$values))

  t(A %*% matrix(rnorm(n * p), ncol = n) + mu)
}

#' The multivariate normal density function.
#'
#' @param x A p-dimension vector.
#' @param mu A p-dimensional mean vector.
#' @param sigma A p \times p variance-covariance matrix.
#' @return A scalar density at \code{x} for the distribution defined by \code{mu} and \code{sigma}.
dmvnorm <- function(x, mu, sigma){
  # @TODO: dimension checking.
  p <- length(x)
  c <- 1 / (2 * pi)^(1/p) * sqrt(det(sigma))
  c * exp((-t(x - mu) %*% solve(sigma) %*% (x - mu)) / 2)
}
