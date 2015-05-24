## Put comments here that give an overall description of what your
## functions do

## Evaluates a matrix object, and caches it

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inverted <- mean
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
        message("getting cached matrix")
        return(inverted)
    } else {
        uninverted <- x$get()
        inverted <- solve(uninverted)
        x$setInverse(inverted)
    } return(inverted)
}
