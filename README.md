[![Build Status](https://travis-ci.org/matthew-seth-smith/bis557.svg?branch=master)](https://travis-ci.org/matthew-seth-smith/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557. I have updated it to be my personal package for
all the work done in this class.

So far the only thing we've done is create and document a function that computes a linear model using SVD, based on Chapter 2 in A Computational Approach to Statistical Learning. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
```

Do NOT use ```{R} summary(fit)``` after this, since the `lm` object created by `linear_model` does not have all of the required fields filled.
