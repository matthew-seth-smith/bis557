## ------------------------------------------------------------------------
library(bis557)
library(glmnet) #For the glmnet function

## ------------------------------------------------------------------------
devtools::document()
break_kkt

## ------------------------------------------------------------------------
X <- model.matrix(Sepal.Length ~ . -1, iris)
X <- scale(X)
iris$Sepal.Length <- iris$Sepal.Length - mean(iris$Sepal.Length)
fit <- cv.glmnet(X, iris$Sepal.Length, standardize=FALSE, intercept = FALSE)
plot(fit) #Get an estimate for the optimal lambda
lambda <- fit$lambda.min #Based on the above plot, after exponentiating
b <- fit$glmnet.fit$beta[,fit$lambda == fit$lambda.min]
#  glmnet(X, iris$Sepal.Length, lambda=lambda, standardize=TRUE)$beta
#b <- as.matrix(b)
#X_stand <- scale(X) #Standardize the data matrix X before plugging it into this function
document()
break_kkt(b, X, iris$Sepal.Length, lambda)

## ---- eval=FALSE---------------------------------------------------------
#  b
#  b$Petal.Length <- 0 #This was originally non-zero
#  break_kkt(b_updated, X_stand, iris$Sepal.Length, lambda)

