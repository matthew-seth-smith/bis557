## ------------------------------------------------------------------------
library(bis557)
install.packages("glmnet")
library(glmnet) #For the glmnet function

## ------------------------------------------------------------------------
break_kkt

## ------------------------------------------------------------------------
X <- model.matrix(Sepal.Length ~ ., iris)
plot(cv.glmnet(X, iris$Sepal.Length, standardize=TRUE)) #Get an estimate for the optimal lambda
lambda <- exp(-4.5) #Based on the above plot, after exponentiating
b <- glmnet(X, iris$Sepal.Length, lambda=lambda, standardize=TRUE)$beta
X_stand <- scale(X) #Standardize the data matrix X before plugging it into this function
break_kkt(b, X_stand, iris$Sepal.Length, lambda)

## ---- eval=FALSE---------------------------------------------------------
#  b
#  b$Petal.Length <- 0 #This was originally non-zero
#  break_kkt(b_updated, X_stand, iris$Sepal.Length, lambda)

