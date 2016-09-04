## cachematrix.R contains a pair of functions that cache the inverse of 
## a matrix.
##

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setinv <- function(solve) mat <<- solve
    getinv <- function() mat
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the matrix object returned by the 
## function makeCacheMatrix.  If the inverse has already been calculated,
## the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getinv()
    if(!is.null(mat)) {
        message("getting cached matrix")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setinv(mat)
    mat
}
