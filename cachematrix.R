## My first function will create a special "matrix" object that can cache its
## inverse, and the second will compute the inverse of the special "matrix" 
## returned by the the first function. But if the inverse has already been 
## calculated, then it will retrieve the inverse from the cache.

## It will make a special "matrix" object the can cache its inverse

makeCacheMatrix <- function(sMatx = matrix()) {
    a <- NULL
    
    ## The 4 functions to set and get the value of the vector and mean
    set <- function(value) {
      sMatx <<- value
      a <<- NULL
    }
    get <- function() matx
    setinverse <- function(solve) a <<- solve
    getinverse <- function() a
    
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## If it hasn't been calculated yet, it will compute the inverse of the special
## "matrix" returned by first function. Otherwise, it will retrieve it from the cache.

cacheSolve <- function(sMatx, ...) {
    ## Checking if the inverse has been calculated already
    inv <- sMatx$getinverse()
    if(!is.null(inv)) {
        message("Getting the cached data")
        return(inv)
    }
    
    ## Calculating and returning the inverse of the matrix
    matx1 <- sMatx$get()
    inv <- solve(matx1, ...)
    sMatx$setinverse(inv)
    inv
}
