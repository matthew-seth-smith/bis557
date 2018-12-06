# Matthew Smith, 12/2/18
# In this file, we create function for IRWLS GLM with a ridge penalty


#' Create a Generalized Linear Model Using Iteratively Re-Weighted Least Squares with a Ridge Penalty
#' 
#' @description This function is an adaptation of casl_glm_irwls from A Computational Approach to Statistical Learning to have a ridge penalty.
#' @param X A numeric data matrix
#' @param y Response vector
#' @param family Instance of an R "family" object (for the family of Generalized Linear Model)
#' @param lambda Ridge parameter for the L-2 penalty
#' @param maxit Integer maximum number of iterations. Default is 25.
#' @param tol Numeric tolerance parameter. Default is 1e-10.
#' @return Regression vector beta of length ncol(X).
#' @examples
#' # Based on the example in the textbook on Page 130
#' n <- 1000
#' p <- 3
#' X <- cbind(1, matrix(rnorm(n * (p-1)), ncol = p-1))
#' mu <- 1 - pcauchy(X %*% beta)
#' y <- as.numeric(runif(n) > mu)
#' beta <- glm_irwls_ridge(X, y, family=binomial(link="cauchit"), lambda=10)
#' beta
#' @export
glm_irwls_ridge <- function(X, y, family, lambda, maxit=25, tol=1e-10){
  beta <- rep(0, ncol(X))
  for(j in seq_len(maxit)){
    b_old <- beta
    eta <- X %*% beta
    mu <- family$linkinv(eta)
    mu_p <- family$mu.eta(eta)
    z <- eta + (y - mu) / mu_p
    W <- as.numeric(mu_p^2 / family$variance(mu))
    XtX <- crossprod(X, diag(W) %*% X) + diag(rep(lambda, ncol(XtX))) #This is my addition to casl_glm_irwls
    Xtz <- crossprod(X, W * z)
    beta <- solve(XtX, Xtz)
    if(sqrt(crossprod(beta - b_old)) < tol) break
  }
  beta
}