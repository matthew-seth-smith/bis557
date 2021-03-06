% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparse.matrix_class.R
\name{+.sparse.matrix}
\alias{+.sparse.matrix}
\title{Add Together Two sparse.matrix Objects}
\usage{
\method{+}{sparse.matrix}(a, b)
}
\arguments{
\item{a}{A sparse.matrix object}

\item{b}{A sparse.matrix object}
}
\value{
A sparse.matrix that is the sum of a and b
}
\description{
This method implements the sparse_add function from class for our new sparse.matrix object
}
\examples{
a <- sparse.matrix(i=c(1,2), j=c(1,1), x=c(3,1), dims=c(3,2))
b <- sparse.matrix(i=c(1,2,3), j=c(1,1,2), x=c(4.4,1.2,3))
a + b
}
