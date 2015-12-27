## Create a cached matrix object that can be used to repeatably solve the inverse of the marix, but only calculates the inverse once.
##
## Testing:
##  m <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(m)      # Change the matrix being cached.
##  m <- cacheMatrix$get()  # Returns the matrix being cached.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cInv <- NULL
    set <- function(y) {
        x <<- y
        cInv <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) cInv <<- solve
    getmatrix <- function() cInv
    list(set = set, get = get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invFun <- x$getmatrix()
    if(!is.null(invFun)) {
        message("getting cached data")
        return(invFun)
    }
    data <- x$get()
    invFun <- solve(data, ...)
    x$setmatrix(invFun)
    invFun
}
