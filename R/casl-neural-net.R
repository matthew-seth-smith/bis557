# Matthew Smith, 12/12/18
# In this file, we create all of the necessary functions for a simple dense neural network using
# the functions from the textbook A Computational Approach to Statistical Learning (CASL)


#' Create a Default List of Weights to Describe a Dense Neural Network (from CASL)
#' 
#' @description This function (from CASL) creates a default list of weights for a dense neural network using a standard normal distribution.
#' @param sizes A vector of integers giving the size of each layer, including the input and output layers
#' @return A list containing initialized weights and biases
#' @examples
#' weights <- casl_nn_make_weights(sizes=c(1, 25, 1)) #From page 216
#' @export
casl_nn_make_weights <- function(sizes){
  L <- length(sizes) - 1L
  weights <- vector("list", L)
  for (j in seq_len(L)){
    w <- matrix(rnorm(sizes[j] * sizes[j + 1L]), ncol = sizes[j], nrow = sizes[j + 1L])
    weights[[j]] <- list(w=w, b=rnorm(sizes[j + 1L]))
  }
  weights
}



#' Apply a Rectified Linear Unit (ReLU) to a Vector/Matrix (from CASL)
#' 
#' @description This function (from CASL) is the Rectified Linear Unit (ReLU) activator function used for neural networks. ReLU(x) = x if x > 0 and ReLU(x) = 0 otherwise.
#' @param v A numeric vector or matrix
#' @return The original input with negative values truncated to zero
#' @examples
#' casl_util_ReLU(rnorm(100))
#' @export
casl_util_ReLU <- function(v){
  v[v < 0] <- 0
  v
}



#' Apply Derivative of the Rectified Linear Unit (ReLU) to a Vector/Matrix (from CASL)
#' 
#' @description This function (from CASL) is the derivative of the Rectified Linear Unit (ReLU) activator function used for neural networks. The derivative is 1 for positive values and 0 otherwise.
#' @param v A numeric vector or matrix
#' @return A numeric vector of matrix of the same dimensions as the input but with positive values set to 1 and negative values set to zero
#' @examples
#' casl_util_ReLU_p(rnorm(100))
#' @export
casl_util_ReLU_p <- function(v){
  p <- v * 0
  p[v > 0] <- 1
  p
}



#' Derivative of the Mean Squared Error (MSE) Function (from CASL)
#' 
#' @description This function (from CASL) is the derivative of the mean squared error (MSE) loss function used for dense neural networks. We do not need the 1/n factor in front, because we will only end up using this function on scalar values.
#' @param y A numeric vector of responses
#' @param a A numeric vector of predicted responses
#' @return A numeric vector of the current derivative of the MSE function
#' @examples
#' casl_util_mse_p(0, rnorm(1))
#' @export
casl_util_mse_p <- function(y, a){
  (a - y)
}



#' Apply the Forward Propagation to a Set of Neural Network Weights and Biases (from CASL)
#' 
#' @description This function (from CASL) implements the forward propagation of the inputs through a dense neural network given the network's weights and biases.
#' @param x A numeric vector representing one row of the input
#' @param weights A list created by casl_nn_make_weights
#' @param sigma The activation function (such as ReLU)
#' @return A list containing the new weighted responses (z) and activations (a)
#' @examples
#' # From page 215, in the code for the casl_nn_sgd function
#' x <- rnorm(1)
#' sizes <- c(1, 25, 1)
#' weights <- casl_nn_make_weights(sizes)
#' # We then do something to update the weights, like stochastic gradient descent (SGD)
#' f_obj <- casl_nn_forward_prop(x, weights, casl_util_ReLU)
#' @export
casl_nn_forward_prop <- function(x, weights, sigma){
  L <- length(weights)
  z <- vector("list", L)
  a <- vector("list", L)
  for (j in seq_len(L)){
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    z[[j]] <- weights[[j]]$w %*% a_j1 + weights[[j]]$b
    a[[j]] <- if (j != L) sigma(z[[j]]) else z[[j]]
  }
  list(z=z, a=a)
}



#' Apply the Backward Propagation Algorithm to a Set of Neural Network Weights and Biases (from CASL)
#' 
#' @description This function (from CASL) implements the backward propagation of the derivatives of the loss function in a neural network over all the weights and biases, to be used in stochastic gradient descent (SGD).
#' @param x A numeric vector representing one row of the input
#' @param y A numeric vector representing one row of the response
#' @param weights A list created by casl_nn_make_weights
#' @param f_obj Output of the function casl_nn_forward_prop
#' @param sigma_p Derivative of the activation function
#' @param f_p Derivative of the loss function
#' @return A list containing the partial derivative of the loss function with respect to each of the weights and with respect to each of the biases
#' @examples
#' # From page 215, in the code for the casl_nn_sgd function
#' x <- rnorm(1)
#' y <- 0
#' sizes <- c(1, 25, 1)
#' weights <- casl_nn_make_weights(sizes)
#' # We then do something to update the weights, like stochastic gradient descent (SGD)
#' f_obj <- casl_nn_forward_prop(x, weights, casl_util_ReLU)
#' b_obj <- casl_nn_backward_prop(x, y, weights, f_obj, casl_util_ReLU_p, casl_util_mse_p)
#' @export
casl_nn_backward_prop <- function(x, y, weights, f_obj, sigma_p, f_p){
  z <- f_obj$z; a <- f_obj$a
  L <- length(weights)
  grad_z <- vector("list", L)
  grad_w <- vector("list", L)
  for (j in rev(seq_len(L))){
    if(j == L){
      grad_z[[j]] <- f_p(y, a[[j]])
    }else{
      grad_z[[j]] <- (t(weights[[j + 1]]$w) %*% grad_z[[j + 1]]) * sigma_p(z[[j]])
    }
    a_j1 <- if(j == 1) x else a[[j - 1L]]
    grad_w[[j]] <- grad_z[[j]] %*% t(a_j1)
  }
  list(grad_z=grad_z, grad_w=grad_w)
}



#' Apply Stochastic Gradient Descent (SGD) to Estimate a Neural Network from (CASL)
#' 
#' @description This function (from CASL) is the meat and potatoes of creating a neural network: using the functions for forward propagation and backward propagation to implement stochastic gradient descent (SGD) in creating a set of weights and biases for a dense neural network. We specify the architecture of the network and give the function training data X, responses y, a number of epochs to run the SGD, a positive learning rate eta, and (optionally) a list of starting weights.
#' @param X A numeric data matrix
#' @param y A numeric vector of responses
#' @param sizes A numeric vector giving the sizes of layers in the neural network
#' @param epochs Integer number of epochs to compute
#' @param eta Positive numeric learning rate
#' @param weights Optional list of starting weights
#' @param sigma The activation function (default is casl_util_ReLU)
#' @param sigma_p Derivative of the activation function (default is casl_util_ReLU_p)
#' @param f_p Derivative of the loss function (default is casl_util_mse_p)
#' @return A list containing the trained weights for the network
#' @examples
#' # From pages 216-217
#' X <- matrix(runif(1000, min=-1, max=1), ncol=1)
#' y <- X[,1,drop = FALSE]^2 + rnorm(1000, sd = 0.1)
#' weights <- casl_nn_sgd(X, y, sizes=c(1, 25, 1), epochs=25, eta=0.01)
#' @export
casl_nn_sgd <- function(X, y, sizes, epochs, eta, weights=NULL, sigma=casl_util_ReLU, sigma_p=casl_util_ReLU_p, f_p=casl_util_mse_p){
  if(is.null(weights)){
    weights <- casl_nn_make_weights(sizes)
  }
  for(epoch in seq_len(epochs)){
    for(i in seq_len(nrow(X))){
      f_obj <- casl_nn_forward_prop(X[i,], weights, sigma)
      b_obj <- casl_nn_backward_prop(X[i,], y[i,], weights, f_obj, sigma_p, f_p)
      for(j in seq_along(b_obj)){
        weights[[j]]$b <- weights[[j]]$b - eta * b_obj$grad_z[[j]]
        weights[[j]]$w <- weights[[j]]$w - eta * b_obj$grad_w[[j]]
      }
    }
  }
  weights
}



#' Predict Values from a Training Neural Network (from CASL)
#' 
#' @description This function (from CASL) uses the weights and biases calculated in a dense neural with using stochastic gradient descent (SGD) to calculate predicted values given new input data.
#' @param weights List of weights describing the neural network
#' @param X_test A numeric data matrix for the predictions
#' @return A matrix of predicted values
#' @examples
#' # From pages 216-217
#' X <- matrix(runif(1000, min=-1, max=1), ncol=1)
#' y <- X[,1,drop = FALSE]^2 + rnorm(1000, sd = 0.1)
#' weights <- casl_nn_sgd(X, y, sizes=c(1, 25, 1), epochs=25, eta=0.01)
#' y_pred <- casl_nn_predict(weights, X)
#' @export
casl_nn_predict <- function(weights, X_test){
  p <- length(weights[[length(weights)]]$b)
  y_hat <- matrix(0, ncol = p, nrow = nrow(X_test))
  for(i in seq_len(nrow(X_test))){
    a <- casl_nn_forward_prop(X_test[i,], weights, casl_util_ReLU)$a
    y_hat[i, ] <- a[[length(a)]]
  }
  y_hat
}

