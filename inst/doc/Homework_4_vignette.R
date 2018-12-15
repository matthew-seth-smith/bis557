## ------------------------------------------------------------------------
library(bis557)
bis557:::glm_irwls_ridge

## ---- eval=FALSE---------------------------------------------------------
#  # Not evaluated because of problem with %*%
#  n <- 1000
#  p <- 3
#  X <- cbind(1, matrix(rnorm(n * (p-1)), ncol = p-1))
#  mu <- 1 - pcauchy(X %*% beta)
#  y <- as.numeric(runif(n) > mu)
#  beta <- glm_irwls_ridge(X, y, family=binomial(link="cauchit"), lambda=10)
#  beta

## ------------------------------------------------------------------------
sparse.matrix

## ------------------------------------------------------------------------
bis557:::`+.sparse.matrix`
bis557:::`%*%.sparse.matrix`
t.sparse.matrix

## ------------------------------------------------------------------------
a <- sparse.matrix(i=c(1,2), j=c(1,1), x=c(3,1))
b <- sparse.matrix(i=c(1,2,3), j=c(1,1,2), x=c(4.4,1.2,3))
c <- sparse.matrix(i=c(1,2), j=c(1,1), x=c(3,1), dims=c(3,2))
a
b
c
b + c
b %*% a
t(a)

