## ------------------------------------------------------------------------
library(bis557)
epan_kernel

## ------------------------------------------------------------------------
kern_density

## ------------------------------------------------------------------------
library(ggplot2)
x_data <- -4:4 #The data vector
ggplot(data.frame(x=0), aes(x=x)) + stat_function(fun=function(x){kern_density(x_data, 1, x)}, aes(color="h=1")) + stat_function(fun=function(x){kern_density(x_data, 2, x)}, aes(color="h=2")) + stat_function(fun=function(x){kern_density(x_data, 3, x)}, aes(color="h=3")) + stat_function(fun=function(x){kern_density(x_data, 5, x)}, aes(color="h=5")) + stat_function(fun=function(x){kern_density(x_data, 8, x)}, aes(color="h=8")) + scale_color_manual("Bandwidth", values=c("red", "orange", "blue", "purple", "black")) + xlim(-10,10)

## ------------------------------------------------------------------------
break_kkt

## ------------------------------------------------------------------------
library(glmnet) #For the cv.glmnet function
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

