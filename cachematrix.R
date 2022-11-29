## Below are two functions that are used to creat a special matrix object that
## stores a matrix and cache's its inverse.

## This function creates a special matrix object which is a list containing a
## function to 1. set the value of the matrix, 2. get the values of the matrix,
## set the value of the inverse matrix, and get the value of the inverse matrix.
## Input, x, must be a defined matrix with an inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <- NULL
  }
  get <- function() x
  seti <- function(solve) x <<- solve
  geti <- function() i
  list(set = set,
       get = get,
       seti = seti,
       geti = geti)
}



## The following function calculates the inverse of the matrix. However, it
## first checks to see if the inverse is already cached. If the inverse is
## cached it returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached inverse, i")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
}
