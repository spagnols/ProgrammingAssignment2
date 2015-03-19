## Below are two functions that are used to create a special object that stores
## a matrix and cache's its inverse.

## Function makeCacheMatrix creates a special "matrix" object that can cache
## its inverse. It returns a list containing functions to set and get values of
## the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {            ## sets the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function()               ## gets the value of the matrix
        x
    setInverse <- function(solve)   ## sets the value of the inverse matrix
        inv <<- solve
    getInverse <- function()        ## gets the value of the inverse matrix
        inv
    ## return list
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function cacheSolve calculates the inverse of the special "matrix" created
## with the makeCacheMatrix function, avoiding re-computation if the inverse
## matrix is already cached.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {                 ## check if inverse matrix is cached
        message("getting cached data")
        return(inv)                     ## return cached inverse matrix
    }
    data <- x$get()
    inv <- solve(data, ...)             ## otherwise, calculate inverse matrix
    x$setInverse(inv)                   ## cache inverse matrix
    inv                                 ## return calculated inverse matrix
}