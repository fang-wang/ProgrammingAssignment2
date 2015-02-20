## The functions below create a matrix and then
## perform matrix inversion and store the inverse
## in the cache.

## Function makeCacheMatrix creates a matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function (y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    list(set=set, get=get, 
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Function cacheSolve inverse a matrix and stores its
## inverse in the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("get the inverse matrix from cache")
        return(inverse)
    }
    
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}
