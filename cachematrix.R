## Two functions, makeCacheMatrix, and cacheSolve, work together to invert
## matrices.
## makeCacheMatrix is a function that evaluates a matrix type
## object, caches it, and stores four functions that can set a new matrix
## for caching, get the cached matrix, set the inverted matrix, or get the
## inverted matrix. 
## cacheSolve is a function that evaluates the objected created by 
## makeCacheMatrix, which is a list of 4 functions, and if there isn't an
## inverted matrix, it would make one with solve, and cache it, ortherwise it
## would simply call the cached inverted matrix.

## Evaluates a matrix object, and caches it

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverted <- inverse
    getInverse <- function() inverted
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverted <- x$getInverse()
    if(!is.null(inverted)) {
        message("getting cached inverted matrix...")
        return(inverted)
    } else {
        uninverted <- x$get()
        inverted <- solve(uninverted)
        x$setInverse(inverted)
    } return(inverted)
}
