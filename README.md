[![Build Status](https://travis-ci.org/matthew-seth-smith/bis557.svg?branch=master)](https://travis-ci.org/matthew-seth-smith/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557. I have updated it to be my personal package for
all the work done in this class. Most of this is based on the book A Computational Approach to Statistical Learning.

The first thing we've done is create and document a function that computes an ordinary least squares regression using SVD. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
```

Do NOT use `summary(fit)` after this, since the `lm` object created by `linear_model` does not have all of the required fields filled.

Next we created a function that computes the ridge regression using SVD. You can use it like this:

```{R}
library(bis557)
fit_reg <- ridge_reg(Sepal.Length ~., 1, iris)
predict(fit_reg, iris)
```

When we studied kernel regression, we made functions for evaluating the Epanechnikov Kernel (`epan_kernel`) and using that kernel to make a density estimator (`kern_density`). You can use them like this

```{R}
library(bis557)
epan_kernel(-5:5, 3)
x <- -4:4
h <- 2
x_new <- -10:10
kern_density(x, h, x_new)
```

In investigating the elastic net linear regression, we created a `break_kkt` function that checks which components of the coefficient vector break the KKT conditions. You can use it like this:

```{R}
library(bis557)
X <- scale(model.matrix(Sepal.Length ~ . -1, iris))
y <- iris$Sepal.Length - mean(iris$Sepal.Length)
fit <- glmnet::cv.glmnet(X, y, standardize=FALSE, intercept = FALSE)
lambda <- fit$lambda.1se
b <- fit$glmnet.fit$beta[,fit$lambda == lambda]
break_kkt(b, X, y, lambda)
```

When we looked at Generalized Linear Models, we first made a function `glm_irwls_ridge` to calculate the coefficients of a GLM with a ridge penalty. The function is based on the `casl_glm_irwls` function in the textbook. You can use it like this:

```{R}
n <- 1000
p <- 3
X <- cbind(1, matrix(rnorm(n * (p-1)), ncol = p-1))
mu <- 1 - pcauchy(X %*% beta)
y <- as.numeric(runif(n) > mu)
beta <- glm_irwls_ridge(X, y, family=binomial(link="cauchit"), lambda=10)
beta
```

We then created a class `sparse.matrix` for sparse matrices. We created methods for an instance function, matrix addition, matrix multiplication, and matrix transposition. You can use the class like this:
```{R}
a <- sparse.matrix(i=c(1,2), j=c(1,1), x=c(3,1))
b <- sparse.matrix(i=c(1,2,3), j=c(1,1,2), x=c(4.4,1.2,3))
c <- sparse.matrix(i=c(1,2), j=c(1,1), x=c(3,1), dims=c(3,2))
a
b
c
b + c
b %*% a
t(a)
```
