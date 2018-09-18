#' Fit a linear model
#'
#' @description This function is my own implementation of the "lm" function in R. The implementation is based on the SVD method from Chapter 2 of A Computational Approach to Statistical Learning.
#' @description This method only uses the singular values that define an inverse condtion number greater than the tolerance of double precision floating point arithmetic, which we assume is 1e-16.
#' @param formula a formula
#' @param data a data.frame
#' @param tol the tolerance of double precision floating point arithmetic, which is by default 1e-16 (according to the book)
#' @return An lm object, but only the coefficients (not the standard errors, t-values, or two-sided probabilities), so don't use summary() on this!
#' @import stats 
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' fit
#' @export
linear_model <- function(formula, data, tol=1e-16) {
  X <- stats::model.matrix(formula, data) #This is our model matrix of the data for the model
  coef_out <- as.list(rep(NA, ncol(X))) #Initialize the output
  names(coef_out) <- colnames(X) #Naming the coefficients
  y_index <- which(colnames(data) == as.character(formula)[2]) #This is where in the data we can find the response variable
  y <- data[,y_index] #Get the vector of the response data
  
  # Cathy's thing for removing dependencies!
  omitted <- rownames(stats::alias(formula, data)$Complete) #Alias removes the dependencies. This returns the columns to be omitted
  X <- X[,setdiff(colnames(X), omitted)] #Remove the linearly dependent columns
  
  # Using Chapter 2, Section 5 of A Computational Approach to Statistical Learning:
  
  svd_list <- svd(X)
  sv <- svd_list[["d"]] #The singular values of X
  sigma_inverse <- diag(1/sv) #The sigma^-1 matrix, where sigma is a matrix of singular values in decreasing order
  U <- svd_list[["u"]] #U matrix of SVD
  V <- svd_list[["v"]] #V matrix of SVD
  browser()
  beta_hat <- V %*% sigma_inverse %*% t(U) %*% y
  
  
  # We now look for the subset of the singular values where the inverse condition number is larger than the tolerance
  #cond_inv <- sv / sv[1] #This gives the ratio of each singular value to the largest singular value
  #smallest <- sum(cond_inv > tol) #Since the singular values are decreasing, so are the entries for cond
  # Then this gives us the index of the smallest singular value where the condition number is still greater than the tolerance
  
  # Ask how to get the correct subset of the predictors from this...
  
  
  # Using Homework 3 of BIS 623 from Fall, 2017 as a guide:
  #beta_hat <- data.frame(solve(t(X) %*% X) %*% t(X) %*% y) #A data.frame with one column of the estimated coefficients
  # By using a data.frame instead of a vector, we can add row names
  #rownames(beta_hat) <- colnames(X) #Change this later to be just the subset used
  
  for(i in 1:length(coef_out)){ #Temporary fix, until have the subsetted data thing
    coef_out[[i]] <- beta_hat[i,]
  }
  
  ret <- list() #Initialize the return object
  class(ret) <- "lm" #Make the return object an lm object
  #Assign the formula and data to the call field of the lm object. I used as.name to remove the quotes:
  ret$call <- as.name(paste(c("linear_model(formula = ", formula, ", data = ", deparse(substitute(data)), ")"), sep="", collapse=""))
  ret$coefficients <- unlist(coef_out) #Assign the coefficients of this lm object to be the ones we determined earlier
  return(ret)
}
