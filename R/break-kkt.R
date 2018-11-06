#' Test where the KKT Conditions are Broken
#' 
#' @description This function returns a logical vector of the same length as the coefficient vector that says where the Karush-Kuhn-Tucker (KKT) Conditions are broken.
#' @param b the coefficient vector to check
#' @param X the data matrix for the LASSO regression, assumed to be standardized (mean 0 and variance 1 for each column) prior. We also assume there is no intercept column
#' @param y the vector of responses, which we assume has also had the mean subtracted
#' @param lambda the LASSO penalty constant (we assume there is no ridge regression component in this elastic net)
#' @return A logical vector of the same length as `b` that says whether each coefficient breaks the KKT Conditions
#' @import stats
#' @import MASS
#' @examples
#' X <- scale(model.matrix(Sepal.Length ~ . -1, iris))
#' y <- iris$Sepal.Length - mean(iris$Sepal.Length)
#' fit <- glmnet::cv.glmnet(X, y, standardize=FALSE, intercept = FALSE)
#' plot(fit) #Get an estimate for the optimal lambda
#' # We will use the largest lambda value that is within one standard deviation of the lambda that gives the minimum MSE
#' lambda <- fit$lambda.1se
#' b <- fit$glmnet.fit$beta[,fit$lambda == lambda]
#' break_kkt(b, X, y, lambda)
#' @export
break_kkt <- function(b, X, y, lambda){
  if(length(b) != ncol(X) | length(y) != nrow(X)){ #First check to make sure the dimensions of everything agree
    print("Error: incorrect number of dimensions somewhere.")
  }else{
    ret <- rep(FALSE, length(b)) #Initialize the return object to be a vector of FALSE
    for(j in 1:length(ret)){ #For each coefficient
      test_var <- sum(X[,j] * (y - X %*% b)) /nrow(X) #Using EQ 7.37 on page 188, using element-wise multiplication before we sum over the rows
      s <- round(test_var / lambda) #This is the s-value in the equation to test the KKT Conditions
      # We unfortunately have to round this to the nearest integer because glmnet does not produce very accurate coefficients,
      # even when we provide it a vector of lambda values
      if(b[j] > 0){
        ret[j] <- !(s == 1) #We are testing to see if the KKT Conditions are BROKEN, not satisfied
      }else if(b[j] == 0){
        ret[j] <- !(-1 <= s & s <= 1)
      }else{ #If negative
        ret[j] <- !(s == -1)
      }
    }
    ret #Returning the logical vector
  }
}