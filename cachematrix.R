## These functions implement a special matrix capable of caching
## its inverse and an inverse solving function that makes use of
## this capability to efficiently store that inverse after first
## computation.

## These functions are developed in satisfaction of Assignment
## 2 of the Coursera R Programming Course.

## Makes a special matrix that can cache its inverse for efficient
## later retrieval.
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    
    # Sets the value of the matrix.
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    
    # Gets current value of the matrix.
    get <- function() x
    
    # Caches the given inverse.
    setInverse <- function(inverse) cachedInverse <<- inverse
    
    # Returns the cached inverse.
    getInverse <- function() cachedInverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Solves for and returns the inverse of a cacheMatrix and caches
## that inverse for later efficient retrieval.  This function assumes
## that the given cacheMatrix is invertible.
cacheSolve <- function(x, ...) {
    
    ## if already cached return the inverse
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## otherwise, compute the inverse, cache it, and return
    ## the value
    data <- x$get()
    inverse <- solve(data) # use solve w/o arguments for inverse only
    x$setInverse(inverse)
    inverse
}
