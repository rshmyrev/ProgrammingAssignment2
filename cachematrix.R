## Below are two functions that are used to create a special object that
## stores a matrix and caches the inverse of the matrix.

## makeCacheMatrix creates a special "matrix", which is really
## a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special "matrix" returned by
## makeCacheMatrix above. However, it first checks to see if the inverse of
## the matrix has already been calculated. If so, it gets the inverse of the
## matrix from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    mtrx <- x$get()
    inv <- solve(mtrx, ...)
    x$setInverse(inv)
    inv
}
