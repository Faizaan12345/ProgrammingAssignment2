# This script contains functions to cache and compute the inverse of a matrix efficiently.

# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize a variable to store the inverse, initially NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # When the matrix changes, invalidate the inverse cache
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() {
    inv
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The cacheSolve function computes the inverse of the special "matrix" and caches it for future requests.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Try to get the cached inverse
  if (!is.null(inv)) {  # If there's a cached inverse
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  data <- x$get()  # Get the matrix from the special object
  inv <- solve(data, ...)  # Calculate the inverse
  x$setinverse(inv)  # Cache the calculated inverse
  inv  # Return the inverse
}
