# Functions for working with multivariate normal distributions.

#' rmvnorm
#' Simulate from a multivariate normal distribution.
rmvnorm <- function(n, mu, sigma){
  p <- length(mu)
  E <- eigen(sigma)
  A <- E$vectors %*% sqrt(diag(E$values))

  matrix(rnorm(n * p), ncol = p) %*% A + mu
}
