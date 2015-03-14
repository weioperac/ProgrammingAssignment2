## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This's a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        # When set to a new matrix, clear its cached inverse as well.
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i
    
    #Create a list containing a function to set, get the matrix and its inverse.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()

    # When the cached inverse exists, retrieve the inverse from the cache directly.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    #If no cached inverse existing, caculate the inverse, cache it and return.
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}