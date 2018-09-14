#' Fit a linear model
#'
#' @description This function is my own implementation of the "lm" function in R. It tests for and removes colinear variables from the model. 
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object, but only the coefficients (not the standard errors, t-values, or two-sided probabilities)
#' @import stats 
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' #summary(fit) #Commented this out so that the lm object without all the fields filled in can work
#' @export
linear_model <- function(formula, data) {
  X <- stats::model.matrix(formula, data) #This is our model matrix of the data for the model
  coef_out <- as.list(rep(NA, ncol(X))) #Initialize the output
  names(coef_out) <- colnames(X) #Naming the coefficients
  
  # Implement finding collinearity later. Not sure how to do...
  # Also ask if the vector of 1's should be included
  
  y_index <- which(colnames(data) == as.character(formula)[2]) #This is where in the data we can find the response variable
  y <- data[,y_index] #Get the vector of the response data
  # Using Homework 3 of BIS 623 from Fall, 2017 as a guide:
  beta_hat <- data.frame(solve(t(X) %*% X) %*% t(X) %*% y) #A data.frame with one column of the estimated coefficients
  # By using a data.frame instead of a vector, we can add row names
  rownames(beta_hat) <- colnames(X) #Change this later to be just the subset used
  
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
