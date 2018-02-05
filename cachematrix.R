## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.
##
## A function will used to store the matrix and its inverse.
## Another function will be used to calculate the inverse of the matrix and to cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    cachedInverseMatrix <- NULL
    
    set <- function(y) {
        x <<- y                          ## sets the matrix x
        cachedInverseMatrix <<- NULL     ## reset the cached inverse matrix to NULL
    }
    
    get <- function() x                     ## returns the matrix x
    
    setInverse <- function(inverseMatrix) cachedInverseMatrix <<- inverseMatrix   ## cache the inverse of the matrix
    
    getInverse <- function() cachedInverseMatrix                                  ## returns the cached inverse of the matrix
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverseMatrix <- x$getInverse()
    
    if (!is.null(inverseMatrix)) {
        message("getting cached inverse matrix")
        return(inverseMatrix)
    }
    
    data <- x$get()
    inverseMatrix <- solve(data)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
