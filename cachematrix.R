## These functions allow us to cache the results of matrix inversion (using 'solve')
## Since this can be a resource intensive task, we will use cheap storage to prevent running the same computation repeatedly

## This function will create a "special matrix" from the one passed in (really a list of functions, one of which stores the actual matrix).  It has the following methods:
## get - returns the matrix
## set - sets the matrix so we can determine if it has been computed before
## getsolve - gets the results of the cached matrix inversion
## setsolve - sets the results of the matrix inversion in cache
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    # Cache the matrix in the parent/global environment
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    # Cache the matrix inversion in the parent/global environment
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    # return a list of the functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function calculates an inverse matrix using solve.
## If this has already been calculated for the specified matrix, the value is pulled from the cache instead of recalculating
cacheSolve <- function(x, ...) {
    ## Pull the cached inverse matrix
    s <- x$getsolve()
    # If it exists, just return it
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    # If it doesn't already exist, calculate it and cache it
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
