## These functions allow me to determine the inverse of a set matrix and store
## the value in the cache. That way, if I need to determine the inverses again,
## the function doesn't take as long to compute and I can retrieve the values
## quickly.

## This function creates a special matrix with the functions to set and get the
## values of the matrix and the inverses.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function checks to see if the inverse of the matrix has been previously
## calculated and, if not, computes the inverse of the matrix and sets it in the
## cache. If it has been computed, it gets the inverse from the cache and skips
## computation.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

