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
