#' Test where the KKT Conditions are Broken
#' 
#' @description This function returns a logical vector of the same length as the coefficient vector that says where the Karush-Kuhn-Tucker (KKT) Conditions are broken.
#' @param b the coefficient vector to check
#' @param X_stand the data matrix for the LASSO regression, assumed to be standardized (mean 0 and variance 1 for each column) prior
#' @param y the vector of responses
#' @param lambda the LASSO penalty constant (we assume there is no ridge regression component in this elastic net)
#' @return A logical vector of the same length as `b` that says whether each coefficient breaks the KKT Conditions
#' @import stats
#' @import MASS
#' @examples
#' X <- model.matrix(Sepal.Length ~ ., iris)
#' plot(cv.glmnet(X, iris$Sepal.Length, standardize=TRUE)) #Get an estimate for the optimal lambda
#' lambda <- exp(-4.5) #Based on the above plot, after exponentiating
#' b <- glmnet(X, iris$Sepal.Length, lambda=lambda, standardize=TRUE)$beta
#' X_stand <- scale(X) #Standardize the data matrix X before plugging it into this function
#' break_kkt(b, X_stand, iris$Sepal.Length, lambda)
#' @export
break_kkt <- function(b, X_stand, y, lambda){
  if(length(b) != ncol(X_stand) | length(y) != nrow(X_stand)){ #First check to make sure the dimensions of everything agree
    print("Error: incorrect number of dimensions somewhere.")
  }else{
    ret <- rep(FALSE, length(b)) #Initialize the return object to be a vector of FALSE
    resids <- y - X_stand %*% b
    mys <- apply(X_stand, 2, function(xj) crossprod(xj, resids)) / lambda / nrow(X_stand)
    # May need to round this
    #browser()
    for(j in 1:length(ret)){ #For each coefficient
#      browser()
      test_var <- sum(X_stand[,j] * (y - X_stand %*% b)) /nrow(X_stand) #Using EQ 7.37 on page 188, using element-wise multiplication before we sum over the rows
      s <- test_var / lambda #This is the s-value in the equation to test the KKT Conditions
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