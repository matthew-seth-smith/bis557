# Use this file to import new data sets
ridge_test <- read.csv("ridge_test.csv")
save(ridge_test, file = "../data/ridge_test.rda")

ridge_train <- read.csv("ridge_train.csv")
save(ridge_train, file = "../data/ridge_train.rda")
