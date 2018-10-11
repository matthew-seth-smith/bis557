## ------------------------------------------------------------------------
library(bis557)
data("ridge_train")
data("ridge_test")
form <- y ~ .

## ------------------------------------------------------------------------
library(stats) #For model.matrix
library(ggplot2) #For ggplot
X <- model.matrix(form, ridge_train) #We use the ridge_train data set to determine the range
y <- ridge_train$y

svd_object <- svd(X) #Do singular value decomposition
U <- svd_object$u
V <- svd_object$v
svals <- svd_object$d #The singular values from SVD
D_2 <- function(lam){ #The diagonal matrix in the derivative vector
  diag(svals / (svals^2 + lam)^2)
}

norm_sq_deriv <- function(lam){ #A function of the lambda value that gives the L2-norm squared of the derivative of beta_hat
  deriv <- -V %*% D_2(lam) %*% t(U) %*% y
  return(sum(deriv^2))
}

nsd <- function(lam){ #This function vectorizes norm_sq_deriv so we can use it in ggplot
  unlist(lapply(lam, norm_sq_deriv))
}

eps <- 0.001 #Our cut-off value epsilon
ggplot(data.frame(x=0), aes(x=x)) + stat_function(fun=nsd, color="blue") + xlim(0,30) +
  ylim(0,0.015) + geom_hline(yintercept=0, color="black") + geom_hline(yintercept=eps, color="red") +
  geom_vline(xintercept=20.5, color="purple")
nsd(20.5)

## ------------------------------------------------------------------------
y_test <- ridge_test$y #The response data
mse <- function(lam){ #Calculates the MSE for ridge_test for a given lambda value lam
  y_hat <- predict(ridge_reg(form, lam, ridge_test), ridge_test) #The predicted values
  mean((y_test-y_hat)^2)
}

