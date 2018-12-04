# Matthew Smith, 12/2/18
# In this file, we create all the necessary functions to implement a sparse.matrix class


#' Construct a sparse.matrix Object
#'
#' @description This functions creates an instance of the sparse.matrix object, just adding "sparse.matrix" to the class vector. We assume that the object is a properly formatted data.frame.
#' @param a The object to make into a sparse.matrix
#' @return The input object, but with "sparse.matrix" appended to the first spot in its class vector
#' @import Matrix
#' @examples
#' X <- sparse.matrix(X)
#' @export
sparse.matrix <- function(a){
  if(!inherits(a, "data.frame")){ #If the input is not a data.frame
    stop("The object to be made a sparse.matrix is not a data.frame.")
  }
  if(ncol(a) != 3){ #The sparse.matrix must have exactly 3 columns
    stop("The data.frame to be made a sparse.matrix does not have 3 columns.")
  }
  if(sum(names(a) == c("i", "j", "x")) != 3){ #If the data.frame a does not have the proper column names
    stop("The object to be made a sparse.matrix does not have the proper column names: i for row indeces, j for column indeces, and x for values.")
  }
  class(a) <- c("sparse.matrix", class(a)) #Add "sparse.matrix" to the class vector, as the first class
  a #Return the updated vals object
}


#' Add Together Two sparse.matrix Objects
#' 
#' @description This method implements the sparse_add function from class for our new sparse.matrix object
#' @param a A sparse.matrix object (constructed from a data.frame)
#' @param b A sparse.matrix object (constructed from a data.frame)
#' @return A sparse.matrix that is the sum of a and b
#' @import Matrix
#' @examples
#' a <- sparse.matrix(data.frame(i=c(1,2), j=c(1,1), x=c(3,1)))
#' b <- sparse.matrix(data.frame(i=c(1,2,3), j=c(1,1,2), x=c(4.4,1.2,3)))
#' a + b
#' @export
`+.sparse.matrix` <- function(a, b){ #We use the tick marks because + is an infix operator. We do not need the generic function because + already exists as a method in R
  if(!inherits(b, "sparse.matrix")){ #The S3 dispatch method only checks that a is a sparse.matrix when calling this method, so here we check that b is as well
    stop("The object b is not a sparse.matrix.")
  }
  c <- merge(a, b, by=c("i","j"), all=TRUE, suffixes=c("1","2")) #Merge a and b by the rows (i) and columns (j) of non-zero values
  c$x1[is.na(c$x1)] <- 0 #Fill in the missing values with 0 for the sum
  c$x2[is.na(c$x2)] <- 0 #Fill in the missing values with 0 for the sum
  c$x <- c$x1 + c$x2 #Take the sum of the entries for the two sparse.matrix's
  sparse.matrix(c[, c("i", "j", "x")]) #Return the columns of c corresponding to the rows (i), columns (j), and values (x) of the non-zero entries of the resulting sparse.matrix
}


# To implement the matrix multiplication of sparse.matrix objects, we define the generic function for %*% and a default function
#`%*%` <- function(a, b){
#  UseMethod("%*%", a)
#}
#`%*%.default` <- function(a, b){
#  a %*% b
#}


#' Matrix-Multiply Two sparse.matrix Objects
#' 
#' @description This method is the implementation of the sparse_multiply from the homework. It matrix-multiplies two sparse.matrix objects.
#' @param a A sparse.matrix object (constructed from a data.frame)
#' @param b A sparse.matrix object (constructed from a data.frame)
#' @return A sparse.matrix object that is the matrix multiple of a and b
#' @import Matrix
#' @examples
#' 
#' @export
`%*%.sparse.matrix` <- function(a, b){ #We use the tick marks because %*% is an infix operator
  if(!inherits(b, "sparse.matrix")){ #The S3 dispatch method only checks that a is a sparse.matrix when calling this method, so here we check that b is as well
    stop("The object b is not a sparse.matrix.")
  }
  # Since the rows of a determine the rows of the product and the columns of b determine the columns of the product, we must find the unique indeces of the rows of a and columns of b
  unique_rows <- unique(a$i) #A vector of all the unique row indeces for non-zero entries of a
  unique_cols <- unique(b$j) #A vector of all the unique column indeces for non-zero entries of b
  
  # Each entry of the product matrix c is the inner product (or dot product) of a row of a and a column of b
  # The dot product of row i of a (a[i,]) and column j of b (b([,j])) can only be non-zero if there is at least one shared index k for non-zero entries in a[i,] and b[,j]
  # That is, there is a value k such that a[i,k] and b[k,j] is not zero
  # If we only take the dot products for these shared indeces, we can avoid unnecessary calculations
  c <- sparse.matrix(data.frame(i=integer(), j=integer(), x=double())) #We initialize c as an empty sparse.matrix
  for(i in unique_rows){ #Each row of a determines the rows of the product matrix c
    a_i <- a[which(a$i==i),] #Takes the ith row of a by finding all of the rows of the data.frame that have i as the i-value
    for(j in unique_cols){ #Each column of b determines the columns of the product matrix c
      b_j <- b[which(b$j==j),] #Takes the jth column of b by finding all of the rows of the data.frame that have j as the j-value
      k_vector <- intersect(a_i$j, b_j$i) #A vector of of the shared indeces for the columns of a[i,] and rows of b[,j]
      if(length(k_vector) > 0){ #If there is at least one overlap
        # To find where in a_i and b_j these indeces occur, we name their rows
        row.names(a_i) <- a_i$j
        row.names(b_j) <- b_j$i
        
        # We take the inner product (dot product) if row a_i and column b_j
        c_new <- as.numeric(
          as.matrix(a_i[as.character(k_vector),"x"]) %*% as.matrix(b_j[as.character(k_vector),"x"]))
        #c_new <- as.matrix(a_i$x[row.names=k_vector]) %*% as.matrix(b_j$x[row.names=k_vector])
        
        # We lastly append this new row to c
        c <- rbind(c, c(i, j, c_new))
      }
    }
  }
  colnames(c) <- c("i", "j", "x") #We reset the column names of c
  c #Return the product sparse.matrix c
}





#' Transpose a sparse.matrix Object
#' 
#' @description This method transposes a sparse.matrix object. Since we only record the non-zero entries in a sparse.matrix, all we need to do is reverse those entries' row and column indeces.
#' @param a A sparse.matrix object (constructed from a data.frame)
#' @return A sparse.matrix object that is the matrix transpose of a
#' @import Matrix
#' @examples
#' a <- sparse.matrix(data.frame(i=c(1,2), j=c(1,1), x=c(3,1)))
#' t(a)
#' @export
t.sparse.matrix <- function(a){ #We initialize the t() method for the sparse.matrix class. We do not need the generic function because t() already exists as a method in R
  a_t <- a #Create the sparse.matrix that will be the transpose from the input a
  a_t$i <- a$j #Replace the row indeces with the original column indeces
  a_t$j <- a$i #Replace the column indeces with the original row indeces
  a_t #Return the transposed sparse.matrix
}