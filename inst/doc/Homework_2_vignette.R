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

eps <- 0.001 #Our cut-off value epsilon
ggplot(data.frame(x=0), aes(x=x)) + stat_function(fun=nsd, color="blue") + xlim(0,30) +
  ylim(0,0.015) + geom_hline(yintercept=0, color="black") + geom_hline(yintercept=eps, color="red") +
  geom_vline(xintercept=20.5, color="purple")
nsd(20.5)

## ---- eval=TRUE----------------------------------------------------------
#library(devtools)
#install()
y_test <- ridge_test$y #The response data
mean_squared_error <- function(lambda){ #Calculates the MSE for ridge_test for a given lambda value lam
  predicted_temp <- ridge_reg(form, lambda, ridge_test)
  y_hat <- predict(predicted_temp, ridge_test) #The predicted values
  mean((y_test-y_hat)^2) #The mse
}

mse <- function(lam){ #Vectorizes the mean_sq_error function above
  unlist(lapply(lam, mean_squared_error))
}

df <- data.frame(lambda=seq(0, 20.5, 0.01))
df$mse <- mse(df$lambda)
ggplot(df, aes(x=lambda, y=mse)) + geom_point()


df2<- data.frame(lambda=seq(0, 1, 0.001))
df2$mse <- mse(df2$lambda)
ggplot(df2, aes(x=lambda, y=mse)) + geom_point()

df3 <-data.frame(lambda=seq(0,300, 5))
df3$mse <- mse(df3$lambda)
ggplot(df3, aes(x=lambda, y=mse)) + geom_point()



mean_squared_error_train <- function(lambda){ #Calculates the MSE for ridge_test for a given lambda value lam
  predicted_temp <- ridge_reg(form, lambda, ridge_train)
  y_hat <- predict(predicted_temp, ridge_train) #The predicted values
  mean((y_test-y_hat)^2) #The mse
}

mse_train <- function(lam){ #Vectorizes the mean_sq_error function above
  unlist(lapply(lam, mean_squared_error_train))
}

df4 <-data.frame(lambda=seq(0, 30, 0.01))
df4$mse <- mse_train(df4$lambda)
ggplot(df4, aes(x=lambda, y=mse)) + geom_point()


