#' Fit a linear model
#'
#' @description This function is my own implementation of the "lm" function in R. The implementation is based on the SVD method from Chapter 2 of A Computational Approach to Statistical Learning.
#' @description This method only uses the covariates that are linearly independent, using the "alias" function from the "stats" package to perform this test.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object, but only the coefficients (not the standard errors, t-values, or two-sided probabilities), so don't use summary() on this!
#' @import stats 
#' @import MASS
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' fit
#' @export
linear_model <- function(formula, data) {
  X <- stats::model.matrix(formula, data) #This is our model matrix of the data for the model
  coef_out <- data.frame(rep(NA, ncol(X))) #Initialize the output
  rownames(coef_out) <- colnames(X) #Naming the coefficients
  y_index <- which(colnames(data) == as.character(formula)[2]) #This is where in the data we can find the response variable
  y <- data[,y_index] #Get the vector of the response data
  
  # Using the alias command to remove dependencies, as suggested by Cathy Xue
  omitted <- rownames(stats::alias(formula, data)$Complete) #Alias removes the dependencies. This returns the columns to be omitted
  Xred <- X[,setdiff(colnames(X), omitted)] #Remove the linearly dependent columns, to get a reduced data set
  
  # Using Chapter 2, Section 5 of A Computational Approach to Statistical Learning:
  svd_list <- svd(Xred)
  sv <- svd_list[["d"]] #The singular values of X
  sigma_inverse <- diag(1/sv) #The sigma^-1 matrix, where sigma is a matrix of singular values in decreasing order
  U <- svd_list[["u"]] #U matrix of SVD
  V <- svd_list[["v"]] #V matrix of SVD
  beta_hat <- V %*% sigma_inverse %*% t(U) %*% y
  coef_out[colnames(Xred),1] <- beta_hat
  colnames(coef_out) <- ""
  coef_out <- t(coef_out) #Make horizontal for output
  coef_out <- as.list(coef_out) #Make a list, as it would be in an lm object
  names(coef_out) <- colnames(X) #Putting the names back
  
  # Returning the lm object output
  ret <- list() #Initialize the return object
  class(ret) <- "lm" #Make the return object an lm object
  #Assign the formula and data to the call field of the lm object. I used as.name to remove the quotes:
  ret$call <- as.name(paste(c("linear_model(formula = ", formula, ", data = ", deparse(substitute(data)), ")"), sep="", collapse=""))
  ret$coefficients <- unlist(coef_out) #Assign the coefficients of this lm object to be the ones we determined earlier
  return(ret)
}
