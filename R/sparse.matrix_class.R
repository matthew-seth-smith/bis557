# Matthew Smith, 12/2/18
# In this file, we create all the necessary functions to implement a sparse.matrix class


#' Construct a sparse.matrix Object
#'
#' @description This functions creates an instance of the sparse.matrix object from scratch as a data.frame. By default, the dimensions will be the largest coordinates.
#' @param i A vector of row indeces of the non-zero entries of the sparse.matrix
#' @param j A vector of column indeces of the non-zero entries of the sparse.matrix
#' @param x A vector of values for the non-zero entries of the sparse.matrix
#' @param dims A vector of the desired dimensions of the sparse.matrix. By default, these are the largest coordinates
#' @return A sparse.matrix object, which is a list of a data.frame with a column i of row indeces, a column j of column indeces, and a column x of values for non-zero entries; and a vector of dimensions
#' @examples
#' a <- sparse.matrix(i=c(1,2), j=c(1,1), x=c(3,1))
#' @export
sparse.matrix <- function(i, j, x, dims=c(max(i), max(j))){
  if(length(i) != length(j) || length(j) != length(x) || length(x) != length(i)){
    stop("Unequal lengths of parameters. The number of rows (i), columns (j), and values (x) should be equivalent.")
  }
  ret <- list(entries=data.frame(i=i, j=j, x=x), dims=dims) #Store all of the information in a list of a data.frame and a vector
  class(ret) <- "sparse.matrix" #Make "sparse.matrix" the class of the return object
  ret #Return the updated vals object
}


#' Add Together Two sparse.matrix Objects
#' 
#' @description This method implements the sparse_add function from class for our new sparse.matrix object
#' @param a A sparse.matrix object
#' @param b A sparse.matrix object
#' @return A sparse.matrix that is the sum of a and b
#' @examples
#' a <- sparse.matrix(i=c(1,2), j=c(1,1), x=c(3,1), dims=c(3,2))
#' b <- sparse.matrix(i=c(1,2,3), j=c(1,1,2), x=c(4.4,1.2,3))
#' a + b
#' @export
`+.sparse.matrix` <- function(a, b){ #We use the tick marks because + is an infix operator. We do not need the generic function because + already exists as a method in R
  if(!inherits(b, "sparse.matrix")){ #The S3 dispatch method only checks that a is a sparse.matrix when calling this method, so here we check that b is as well
    stop("The object b is not a sparse.matrix.")
  }
  if(sum(a$dims != b$dims) > 0){ #We cannot add together two matrices that are not of the same dimensions
    stop("The sparse.matrices must have the same dimensions.")
  }
  c <- merge(a$entries, b$entries, by=c("i","j"), all=TRUE, suffixes=c("1","2")) #Merge the entries of a and b by the rows (i) and columns (j) of non-zero values
  c$x1[is.na(c$x1)] <- 0 #Fill in the missing values with 0 for the sum
  c$x2[is.na(c$x2)] <- 0 #Fill in the missing values with 0 for the sum
  c$x <- c$x1 + c$x2 #Take the sum of the entries for the two sparse.matrix's
  c <- c[, c("i", "j", "x")] #Keep the columns of c corresponding to the rows (i), columns (j), and values (x) of the non-zero entries of the resulting sparse.matrix
  ret <- list(entries=c, dims=a$dims) #Create the return object as a list
  class(ret) <- "sparse.matrix" #Set the class of the return object to be a sparse.matrix
  ret
}


# To implement the matrix multiplication of sparse.matrix objects, we define the generic function for %*% and a default function
`%*%` <- function(a, b){
  UseMethod("%*%", a)
}
`%*%.default` <- function(a, b){
  a %*% b
}


#' Matrix-Multiply Two sparse.matrix Objects
#' 
#' @description This method is the implementation of the sparse_multiply from the homework. It matrix-multiplies two sparse.matrix objects.
#' @param a A sparse.matrix object
#' @param b A sparse.matrix object
#' @return A sparse.matrix object that is the matrix multiple of a and b
#' @examples
#' a <- sparse.matrix(i=c(1,2), j=c(1,1), x=c(3,1))
#' b <- sparse.matrix(i=c(1,2,3), j=c(1,1,2), x=c(4.4,1.2,3))
#' b %*% a
#' @export
`%*%.sparse.matrix` <- function(a, b){ #We use the tick marks because %*% is an infix operator
  if(!inherits(b, "sparse.matrix")){ #The S3 dispatch method only checks that a is a sparse.matrix when calling this method, so here we check that b is as well
    stop("The object b is not a sparse.matrix.")
  }
  if(a$dims[2] != b$dims[1]){ #The number of columns of the first sparse.matrix must equal the number of rows of the second in order for multiplication to be defined
    stop("The number of columns of the first sparse.matrix must equal the number of rows of the second.")
  }
  # Since the rows of a determine the rows of the product and the columns of b determine the columns of the product, we must find the unique indeces of the rows of a and columns of b
  unique_rows <- unique(a$entries$i) #A vector of all the unique row indeces for non-zero entries of a
  unique_cols <- unique(b$entries$j) #A vector of all the unique column indeces for non-zero entries of b
  
  # Each entry of the product matrix c is the inner product (or dot product) of a row of a and a column of b
  # The dot product of row i of a (a[i,]) and column j of b (b([,j])) can only be non-zero if there is at least one shared index k for non-zero entries in a[i,] and b[,j]
  # That is, there is a value k such that a[i,k] and b[k,j] is not zero
  # If we only take the dot products for these shared indeces, we can avoid unnecessary calculations
  c <- data.frame(i=integer(), j=integer(), x=double()) #We initialize c as an empty data.frame
  for(i in unique_rows){ #Each row of a determines the rows of the product matrix c
    a_i <- a$entries[which(a$entries$i==i),] #Takes the ith row of a by finding all of the rows of the data.frame that have i as the i-value
    for(j in unique_cols){ #Each column of b determines the columns of the product matrix c
      b_j <- b$entries[which(b$entries$j==j),] #Takes the jth column of b by finding all of the rows of the data.frame that have j as the j-value
      k_vector <- intersect(a_i$j, b_j$i) #A vector of of the shared indeces for the columns of a[i,] and rows of b[,j]
      if(length(k_vector) > 0){ #If there is at least one overlap
        # To find where in a_i and b_j these indeces occur, we name their rows
        row.names(a_i) <- a_i$j
        row.names(b_j) <- b_j$i
        
        # We take the inner product (dot product) of row a_i and column b_j, but only for the indeces k_vector
        c_new <- as.numeric(
          as.matrix(a_i[as.character(k_vector),"x"]) %*% as.matrix(b_j[as.character(k_vector),"x"]))
        
        # We lastly append this new row to c
        c <- rbind(c, c(i, j, c_new))
      }
    }
  }
  colnames(c) <- c("i", "j", "x") #We reset the column names of the product c
  ret <- list(entries=c, dims=c(a$dims[1], b$dims[2])) #We create the return object as a list, using the proper dimensions
  class(ret) <- "sparse.matrix" #Set the class of the return object to be a sparse.matrix
  ret
}


#' Transpose a sparse.matrix Object
#' 
#' @description This method transposes a sparse.matrix object. Since we only record the non-zero entries in a sparse.matrix, all we need to do is reverse those entries' row and column indeces, as well as the dimensions.
#' @param a A sparse.matrix object
#' @return A sparse.matrix object that is the matrix transpose of a
#' @import Matrix
#' @examples
#' a <- sparse.matrix(i=c(1,2), j=c(1,1), x=c(3,1))
#' t(a)
#' @export
t.sparse.matrix <- function(a){ #We initialize the t() method for the sparse.matrix class. We do not need the generic function because t() already exists as a method in R
  a_t <- a$entries #Create the a data.frame of the entries that will be the transpose from the input a
  a_t$i <- a$entries$j #Replace the row indeces with the original column indeces
  a_t$j <- a$entries$i #Replace the column indeces with the original row indeces
  ret <- list(entries=a_t, dims=c(a$dims[2], a$dims[1])) #We create the return object as a list and change the dimensions
  class(ret) <- "sparse.matrix" #Set the class of the return object to be a sparse.matrix
  ret
}