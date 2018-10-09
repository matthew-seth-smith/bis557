#' Fit a Ridge Regression Linear Model
#'
#' @description This function computes a ridge regression linear model. The implementation uses SVD.
#' @param formula a formula
#' @param data a data.frame
#' @param lambda a non-negative lambda value for the ridge regression (0 just gives ordinary least squares regression)
#' @return A ridge_reg object, which is a list containing a vector of named coefficients, the lambda value, and the formula
#' @import stats
#' @import MASS
#' @examples
#' fit_reg <- ridge_reg(Sepal.Length ~., iris, 1)
#' fit_reg
#' @export
ridge_reg <- function(formula, data, lambda){
  # Based on what we did in class and the linear_model function in this package
  rownames(data) <- NULL #In case data is a subset of a larger data.frame, this will mean the indexing won't get messed up
  y_index <- which(colnames(data) == as.character(formula)[2]) #This is where in the data we can find the response variable
  y <- data[,y_index] #Get the vector of the response data
  m <- stats::model.matrix(formula, data) #This is our model matrix of the data for the model
  
  svd_object <- svd(m) #Do singular value decomposition
  U <- svd_object$u #One of the orthogonal rotation/reflection matrices from SVD
  V <- svd_object$v #The other orthogonal matrix from SVD
  svals <- svd_object$d #The singular values from SVD
  D <- diag(svals / (svals^2 + lambda)) #The diagonal matrix used in ridge regression, using the proper vectorization from R
  
  beta <- V %*% D %*% t(U) %*% y #Calculating the ridge regression coefficients
  rownames(beta) <- colnames(m) #Naming the coefficients
  
  ret <- list(coefficients=beta, lambda=lambda, formula=formula) #Collecting the coefficients, lambda value, and formula in a list
  class(ret) <- "ridge_reg" #Making the returned object the ridge_reg class
  ret #Returning the ridge_reg object
}
