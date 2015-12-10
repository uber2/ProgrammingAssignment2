## Matrix inversion is a costly computation and it is beneficial to cache the inverse
## of a matrix rather than compute it repeatedly. The two functions makeCacheMatrix
## and cacheSolve allow to cache the inverse of a matrix via the introduction of a
## special "matrix" object.

## This function creates a special "matrix" object that can cache its inverse "i".

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## Define setter and getter functions for the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    ## Define setter and getter functions for the inverse
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    
    ## Return a list containing functions (input for cacheSolve)
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix". 

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    
    ## if the inverse has already been calculated, return it from the cache
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## if the inverse has NOT been calculate before, calculate it now and
    ## store it in the special "matrix" object
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
