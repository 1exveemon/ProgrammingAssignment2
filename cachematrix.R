## This file contains functions for creating a matrix that allows for its
## inverse to be calculated once and cached for future calculations

## This function creates a matrix object that can cached.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        getInverse <- function() {
                inv
        }
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned
## If the inverse has already been computed and cached
## it returns only the cached inverse instead of fully recomputing it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
