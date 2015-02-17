# This module contains two functions:
# 1) makeCacheMatrix function keeps a cache of matrix and its inverse.
# 2) cacheSolve function calculates inverse for a given "cached matrix" 
#    and preserves it in cache for faster repeat calls

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  # cache of inverse matrix
    set <- function(y) {  # sets new matrix
        x <<- y
        i <<- NULL  # when setting new value, need to reset the inverse cache
    }
    get <- function() x  # returns the original matrix
    setinverse <- function(inverse) i <<- inverse  # updates the cache
    getinverse <- function() i  # returns the inverse matrix from cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # x is makeCacheMatrix
    # Any extra parameters will be passed to 'solve' on the first run
    # Returns a matrix that is the inverse of 'x'
    i <- x$getinverse()
    # First check the cache
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # Cache is empty: inverse, cache and return the result
    data <- x$get()
    i <- solve(data, ...) 
    x$setinverse(i)
    i
}