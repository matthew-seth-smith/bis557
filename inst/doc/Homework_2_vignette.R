## ------------------------------------------------------------------------
library(bis557)
data("ridge_train")
data("ridge_test")
form <- y ~ .

## ------------------------------------------------------------------------
library(stats) #For model.matrix
library(ggplot2) #For ggplot
X_train <- model.matrix(form, ridge_train) #We use the ridge_train data set to determine the range
y_train <- ridge_train$y

svd_object_train <- svd(X_train) #Do singular value decomposition
U_train <- svd_object_train$u
V_train <- svd_object_train$v
svals_train <- svd_object_train$d #The singular values from SVD
D_2 <- function(lam){ #The diagonal matrix in the derivative vector
  diag(svals_train / (svals_train^2 + lam)^2)
}

norm_sq_deriv <- function(lam){ #A function of the lambda value that gives the L2-norm squared of the derivative of beta_hat
  deriv <- -V_train %*% D_2(lam) %*% t(U_train) %*% y_train
  return(sum(deriv^2))
}

nsd <- function(lam){ #This function vectorizes norm_sq_deriv so we can use it in ggplot
  unlist(lapply(lam, norm_sq_deriv))
}

eps <- 5e-4 #Our cut-off value epsilon
ggplot(data.frame(x=0), aes(x=x)) + stat_function(fun=nsd, color="blue") + xlim(0,50) +
  ylim(0,0.015) + geom_hline(yintercept=0, color="black") + geom_hline(yintercept=eps, color="red") +
  geom_vline(xintercept=30, color="purple") + labs(x=expression(lambda), y=expression(paste("||", scriptstyle(frac(partialdiff, paste(partialdiff, lambda))), " ", hat(beta)["Ridge"], "||")[2]^2))
nsd(30)

## ------------------------------------------------------------------------
y_test <- ridge_test$y #The response data
mean_squared_error <- function(lam){ #Calculates the MSE for ridge_test for a given lambda value lam, using ridge_train to determine the coefficients
  beta_train <- ridge_reg(form, lam, ridge_train) #Calculates the coefficients using the ridge_train data set and lambda value lam
  y_hat_test <- predict(beta_train, ridge_test) #The predicted values of the ridge_test data set
  mean((y_test-y_hat_test)^2) #The mse
}

mse <- function(lam){ #Vectorizes the mean_sq_error function above
  unlist(lapply(lam, mean_squared_error))
}

#Make a data.frame of lambda values, calculate the mse of each, and then graph them
df <- data.frame(lambda=seq(0, 30, 0.01))
df$mse <- mse(df$lambda)
ggplot(df, aes(x=lambda, y=mse)) + geom_point() + labs(x=expression(lambda), y=expression(italic("MSE")))

## ------------------------------------------------------------------------
df2 <- data.frame(lambda=seq(0, 50, 0.01))
df2$mse <- mse(df2$lambda)
ggplot(df2, aes(x=lambda, y=mse)) + geom_point() + labs(x=expression(lambda), y=expression(italic("MSE")))


min_index <- which(df2$mse == min(df2$mse)) #The index of the lambda value that gives the smallest MSE
min_lambda <- df2$lambda[min_index] #That particular lambda value
min_lambda
mse(min_lambda)

## ------------------------------------------------------------------------
ridge_reg(form, min_lambda, ridge_train)

