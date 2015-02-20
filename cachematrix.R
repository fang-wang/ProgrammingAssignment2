## Assignment 2 for R Programming.
## 2/20/2015

## The functions below create a matrix and 
## perform matrix inversion. The inverse is
## stored in the cache.

## 1) Function makeCacheMatrix creates a matrix
##    that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # Define function set()
    # assign matrix value and initialize inverse matrix to NULL
    set <- function (y) {
        x <<- y
        inverse <<- NULL
    }
    
    # Define function get()
    # get the matrix value. Syntax: x$get()
    get <- function() x
    
    # Define function setinverse()
    # assign value as the matrix inverse
    setinverse <- function(inv) inverse <<- inv
    
    # Define function getinverse()
    # get the inverse matrix. Syntax: x$getinverse()
    getinverse <- function() inverse
    
    # makeCacheMatrix output is a list containing the
    # four functions defined above.
    list(set=set, get=get, 
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## 2) Function cacheSolve inverse a matrix and stores its
## inverse in the cache.

cacheSolve <- function(x, ...) {
    
    # check whether the inverse is cached, and if so,
    # the cached inverse is directly returned.
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("get the inverse matrix from cache")
        return(inverse)
    }
    
    # If the inverse is not cached, go ahead to calculate
    # the inverse using the solve() function. The inverse
    # matrix is returned.
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}
