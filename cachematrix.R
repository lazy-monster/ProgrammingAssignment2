## Pair of functions that cache the inverse of a matrix
## Usage: Pass the result of a makeCacheMatrix call to cacheSolve 

#' Util function that set the matrix and the inverse in an environment
#' @param x an invertible matrix
#' examples
#' x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' x$set(matrix(rnorm(16), 4, 4))
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will store the cached inverse matrix

  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Invalidate the cached inverse because the matrix has changed
  }

  # Function to get the matrix
  get <- function() {
    x
  }

  # Function to set the inverse matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }

  # Function to get the inverse matrix
  getInverse <- function() {
    inv
  }

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



#' Compute and cache the inverse of a matrix
#' @param x the result of a previous makeCacheMatrix call
#' @param ... additional arguments to pass to solve function
#' examples
#' x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' cacheSolve(x)
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to get the cached inverse

  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # If the inverse is not cached, compute it
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse

  x$setInverse(inv)  # Cache the inverse

  inv  # Return the inverse
}
