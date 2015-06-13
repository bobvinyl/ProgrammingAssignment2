## The following functions store a matrix and its inverse (once calculated)
## in cache, avoiding the expensive process of calculating the inverse 
## repeatedly.

## makeCacheMatrix() stores the matrix passed into the function as well as 
## getter and setter functions for both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## Get the matrix
    get <- function() {
        x
    }
    
    ## Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Get the inverse
    getInverse <- function() {
        inv
    }
    
    ## Set the inverse
    setInverse <- function(invmat) {
        inv <<- invmat
    }
    
    ## Return a list of the getter and setter functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Retrieve (and calculate if null) the inverse of the matrix cached by the call
## to makeCacheMatrix().

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    ## If the inverse has been calculated and cached already, return it
    if(!is.null(inv)) {
        message("Returning cached inverse of the matrix.")
        return(inv)
    }
    
    ## If the inverse has not been calculated, calculate it and cache and 
    ## return it.
    x$setInverse(solve(x$get(), ...))
    x$getInverse()
}