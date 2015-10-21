## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()){

    cache <- NULL
	
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
	
    get <- function() x
    setinverse <- function(inverse) cache <<- inverse
    getinverse <- function() cache
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    cache <- x$getinverse()
    
    if(!is.null(cache)) {
        message("getting cached data.")
        return(cache)
    }
    
    data <- x$get()
    cache <- solve(data, ...)
    x$setinverse(cache)
    return(cache)
    
}
