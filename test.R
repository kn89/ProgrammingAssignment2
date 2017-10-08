makeCacheMatrix <- function(x = matrix()) {
# Input matrix x must be always square invertible matrix
# This makeCacheMatrix function creates a special "matrix" object that can cache its inverse
# The output of makeCacheMatrix function are used as input for the cacheSolve() function
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
# List of output from makeCacheMatrix function
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
# Here input x is the output of makeCacheMatrix() earlier
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then 
# the cachesolve should retrieve the inverse from the cache.
  
inv = x$getinv()
  
# Check for the cache if the inverse was calcuated earlier and display the output from cache
# and skips computation
  if (!is.null(inv)){
    message("Found Inverse in Cache as: ")
    return(inv)
  }
  
# Calculates the inverse if not found in cache.
  inv_x = x$get() 
  inv = solve(inv_x, ...)
  
# sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}