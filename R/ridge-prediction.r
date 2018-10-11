#' Predict the Response to a Ridge Regression
#'
#' @description This method predicts the response to a ridge regression object.
#' @param object a ridge_reg object
#' @param ... all the other possible parameters for the predict method
#' @return A vector of the predicted values for the given data and ridge regression coefficients
#' @import stats
#' @import MASS
#' @examples
#' fit_reg <- ridge_reg(Sepal.Length ~., 1, iris)
#' predict(fit_reg, iris)
#' @export
predict.ridge_reg <- function(object, ...){
  # Based on what we did in class
  newdata <- list(...)[[1]] #This takes the first extra parameter as the data to use for the prediction
  m <- stats::model.matrix(object$formula, newdata) #Reconstructing the data matrix using the formula stored in object
  m %*% object$coefficients #Returning a vector of the predicted values
}