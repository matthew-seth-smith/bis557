## ------------------------------------------------------------------------
library(keras) #For the MNIST data set
library(glmnet) #For the LASSO
set.seed(1729) #We set the seed so the results are reproducible, using Ramanujan's number as the seed

# The data, shuffled and split between train and test sets
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

x_train <- array_reshape(x_train, c(60000, 28^2))
x_test <- array_reshape(x_test, c(10000, 28^2))
y_train <- factor(y_train)
y_test <- factor(y_test)

# We only use use 1000 of the characters so that the training does not take too long
s <- sample(seq_along(y_train), 1000)
fit <- cv.glmnet(x_train[s,], y_train[s], family = "multinomial")
plot(fit)
# We will not need to use a subset of the test data set for the prediction
preds_1 <- predict(fit$glmnet.fit, x_test, s = fit$lambda.min, type = "class") #Using lambda.min
preds_2 <- predict(fit$glmnet.fit, x_test, s = fit$lambda.1se, type = "class") #Using lambda.1se
t_1 <- table(as.vector(preds_1), y_test)
t_2 <- table(as.vector(preds_2), y_test)
t_1
t_2
# The correct predictions are on the diagonals of these tables
sum(diag(t_1)) / sum(t_2) #0.8509
sum(diag(t_2)) / sum(t_2) #0.8473

## ------------------------------------------------------------------------
# Parameters for the convolutional neural network
batch_size <- 128
n_class <- 10
epochs <- 2

# Dimensions for the images
n_row <- 28
n_col <- 28

# We need to redefine the training and test data since we "flattened" the matrices into vectors
x_train_conv <- mnist$train$x
y_train_conv <- mnist$train$y
x_test_conv <- mnist$test$x
y_test_conv <- mnist$test$y

# Next we extract each picture from the data sets
x_train_conv <- array_reshape(x_train_conv, c(nrow(x_train_conv), n_row, n_col, 1))
x_test_conv <- array_reshape(x_test_conv, c(nrow(x_test_conv), n_row, n_col, 1))
input_shape <- c(n_row, n_col, 1)

# We need to transform the RGB values to the [0,1] range
x_train_conv <- x_train_conv / 255
x_test_conv <- x_test_conv / 255

# Lastly we convert the class vectors for the responses to binary class matrices
y_train_conv <- to_categorical(y_train_conv, n_class)
y_test_conv <- to_categorical(y_test_conv, n_class)

## ------------------------------------------------------------------------
# Define
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu',
                input_shape = input_shape) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = n_class, activation = 'softmax')

# Compile model
model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

# Train model
model %>% fit(
  x_train_conv, y_train_conv,
  batch_size = batch_size,
  epochs = epochs, #Using two epochs for the training
  validation_split = 0.2
)

## ------------------------------------------------------------------------
scores <- model %>% evaluate(
  x_test_conv, y_test_conv, verbose = 0
)

cat('Test loss:', scores[[1]], '\n')
cat('Test accuracy:', scores[[2]], '\n')

