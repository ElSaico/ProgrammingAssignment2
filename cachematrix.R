# Sending assignment again for September, because this time I need the Signature Track

## Functions for creating an object that stores a matrix and its inverse, when calculated.
## When the matrix is changed, the inverse cache is nullified.

## Creates a wrapping object around a matrix that can store a cache of its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # contains the cached inverse
  set <- function(y) { # sets a new value and nullifies the inverse cache
    x <<- y
    inv <<- NULL
  }
  get <- function() x # retrieves the matrix
  setinverse <- function(inverse) inv <<- inverse # sets the inverse cache
  getinverse <- function() inv # retrieves the cached inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Tries to retrieve the inverse matrix of an object created by makeCacheMatrix,
## and calculates it if necessary

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # the inverse is already known; no need to recalculate
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) # calculates the inverse matrix
  x$setinverse(inv) # caches the inverse matrix for future use
  inv
}
