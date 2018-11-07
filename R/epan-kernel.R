#' Calculate the Epanechnikov Kernel
#' 
#' @description This function returns a vector of values for the Epanechnikov Kernel that is the same length as the input vector. This implementation is based on the one in A Computational Approach to Statistical Learning, Pages 80-81.
#' @param x A vector of imputs for this function
#' @param h The bandwidth, which has the default value of 1
#' @return A numerical vector of K_h(x) evaluated at each value of x
#' @examples
#' x <- -5:5
#' h <- 3
#' epan_kernel(x, 3)
#' @export
epan_kernel <- function(x, h=1){
  x_scaled <- x/h #Make a vector of the scaled values of x
  indic <- as.numeric(abs(x_scaled) <= 1) #This is the indicator to see if the scaled values of x are in the required range
  # as.numeric makes TRUE into 1 and FALSE into 0
  (3/4) * (1 - x_scaled^2) * indic / h #Plug every value of x_scaled into the function, but then multiply every value by the indicator
  # Return this value
}


#' Calculate the Density Estimate using the Epanechnikov Kernel
#' 
#' @description This function returns a vector of values for the density function defined by the Epanechnikov Kernel.
#' @param x A vector of points to use for training the density function
#' @param h The bandwidth for the underlying Epanechnikov Kernel, which has the default value of 1
#' @param x_new The vector of values on which to evaluate the density function
#' @return A numerical vector of the density estimates of x_new using the Epanechnikov Kernel
#' @examples
#' x <- -4:4
#' h <- 2
#' x_new <- -10:10
#' kern_density(x, h, x_new)
#' @export
kern_density <- function(x, h, x_new){
  sapply(x_new, function(w){ #We use sapply to vectorize the function, which then returns a vector and not a list
    # We vectorize this anonymous function that actually does what we want
    mean(sapply(x, function(u){ #Now we vectorize over all the values of the test data x
      epan_kernel(w-u, h) #We use w for the current value of x_new and u for the current value of x
    })) #For each value w in x_new, we take the arithmetic mean of all these Epanechnikov Kernels
  })
}