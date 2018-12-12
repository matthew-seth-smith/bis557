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
#' @description This function (from CASL) is the Rectified Linear Unit (ReLU) activator function usedfor neural networks. ReLU(x) = x if x > 0 and ReLU(x) = 0 otherwise.
#' @param v A numeric vector or matrix
#' @return The original input with negative values truncated to zero
#' @examples
#' casl_util_ReLU(rnorm(100))

casl_util_ReLU <- function(v){
  v[v < 0] <- 0
  v
}






