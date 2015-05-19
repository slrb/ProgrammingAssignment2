## These functions cache the inverse of a matrix
## This allows the value to be looked up in the cache rather than recomputed


## makeCacheMatrix creates a special matrix object that can cache its inverse
## It stores a list of functions to set the value of the matrix, get the value of the matrix,
## set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # set the value of the inverse to NULL
  i <- NULL
  
  # set the value of the matrix
  # substitute x with y and restore the inverse to NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  # return the stored value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setInv <- function(solve) i <<- solve
  
  # return the stored value of the inverse
  getInv <- function() i
  
  # store the list of the four functions  
  list(set=set, get=get, setInv=setInv, getInv=getInv)

}

## This function computes the inverse of the special matrix returned by makeCacheMatrix
## and the value of the inverse is set in the cache
## If the inverse has already been calculated for the matrix, the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
  
  # Use the function getInv from makeCacheMatrix to get the value of the inverse
  i <- x$getInv()
  
  # If the value of the inverse is in memory, return the value (and exit the function)
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # If the value is not in memory, get the stored matrix
  data <- x$get()
  
  # Return a matrix that is the inverse of 'x'
  # The solve function is used for the calculation of the inverse
  i <- solve(data, ...)
  
  # Store the value of the inverse
  x$setInv(i)
  
  # Return the inverse of the matrix
  i
}
