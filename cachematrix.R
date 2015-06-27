## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(A = matrix()) {
    I <- NULL
    set <- function(y) {
        A <<- y
        I <<- NULL
    }
    get <- function() A
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve retrieves the inverse
## from the cache.

cacheSolve <- function(A, ...) {
    I <- A$getinverse()
    if (!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    M <- A$get()
    I <- solve(M)
    A$setinverse(I)
    I
}
