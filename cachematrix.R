## The functions below 1. makeCacheMatrix and 2. cacheSolve do the following:
## 1. makeCacheMatrix creates special matrix object that can cache its inverse 
## 2. cacheSolve calculates the inverse of the matrix, check whether the 
## inverse has already been computed, and if yes, and the matrix has not 
## changed, then cacheSolve obtains the inverse from the cache

## 1. makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  set_mat <- function(square_mat) mat <<- square_mat
  get_mat <- function() mat
  list(set=set, get=get, set_mat=set_mat, get_mat=get_mat)
}


## 2. cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function above. If the inverse 
## has already been calculated (and the matrix has not changed) 
## the function cacheSolve will retrieve the inverse from the cache
## otherwise, cacheSolve calculates and returns the inverse 
## of the original special "matrix" object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat <- x$get_mat()
    if(!is.null(mat)) {
      message("Getting cached data")
      return(mat)
    }
  data <- x$get()
  mat <- solve(data, ...)
  x$set_mat(mat)
  mat
}

