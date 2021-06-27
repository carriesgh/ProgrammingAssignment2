## makecacheMatrix and cacheSolve together will create an object that will store
## a matrix and cache its inverse

## Creates a matrix that can cache its inverse. Returns a list of functions 
## (set, get, setInverse, and getInverse) that will be used to cache the inverse 
## matrix.


makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
            x <<- y
            invM <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) invM <<- inv
    getInverse <- function() invM
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## Computes and returns the inverse of the matrix returned by makeCacheMatrix. If the 
## the matrix has not changed and its inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
    invM <- x$getInverse()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setInverse(invM)
    invM    
}

