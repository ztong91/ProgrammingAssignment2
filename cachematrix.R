## Put comments here that give an overall description of what your
## functions do
## The aim of this assignment is to write a pair of function for makeCacheMatrix and cacheSolve that cache the inverse of a matrix.

## Write a short comment describing this function
## The makeCacheMatrix is a function which can create a matrix object that can cache the inverse of the input.


makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
  
  set <- function(y) {
    
    x <<- y
    inv <<- NULL
    
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function
## The cacheSolve is a function to compute the inverse of matrix return by the makeCacheMatrix.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    
    message("Getting cached result.")
    return(inv)
    
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  inv
}
