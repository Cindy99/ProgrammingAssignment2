## These two functions solve for the inverse of an invertible matrix,
## and cache that inverse

## This function caches a matrix and returns a list containing four functions -
## set(), get(), setinverse(), and getinverse()
## minv caches the solution

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(source) minv <<- source
    getinverse <- function() minv

    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverted matrix
## If this is the first time it has been called with a given cached matrix,
##  it will solve for the inverse and cache the value in x$setInverse
## If called again, it will return the previously cached matrix minv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached matrix inversion")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
}
