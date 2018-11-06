## ------------------------------------------------------------------------
library(bis557)
library(glmnet) #For the glmnet function

## ------------------------------------------------------------------------
break_kkt

## ------------------------------------------------------------------------
X <- scale(model.matrix(Sepal.Length ~ . -1, iris))
y <- iris$Sepal.Length - mean(iris$Sepal.Length)
fit <- cv.glmnet(X, y, standardize=FALSE, intercept = FALSE)
plot(fit) #Get an estimate for the optimal lambda
# We will use the largest lambda value that is within one standard deviation of the lambda that gives the minimum MSE
lambda <- fit$lambda.1se
b <- fit$glmnet.fit$beta[,fit$lambda == lambda]
b
break_kkt(b, X, y, lambda)

## ------------------------------------------------------------------------
b_updated <- b
b_updated["Petal.Length"] <- 0 #This was originally non-zero
break_kkt(b_updated, X, y, lambda)
b_updated2 <- b
b_updated2["Speciesversicolor"] <- 0.01 #This was originally zero
break_kkt(b_updated2, X, y, lambda)

