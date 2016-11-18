## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly. The following functions cache the inverse of a matrix.
##
## example: 
## >  x <- cbind(c(1,-0.25),c(-0.25, 1))
## >  x
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    
    get <- function() x
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    setsolve <- function(solve) im <<- solve
    getsolve <- function() im
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    im <- x$getsolve()
    if (!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    im <- solve(x$get(), ...)
    x$setsolve(im)
    im
}
