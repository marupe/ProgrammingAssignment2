## The first function creates a special "matrix" object 
## than can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## im will store the chache matrix
    im <- NULL
    
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    
    ##return the matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    
    ## If the inverse is already calculated, return it
    if (!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    
    ## If the inverse is not calculated, calculate it
    data <- x$get()
    im <- solve(data, ...)
    
    # Cache the inverse
    x$setinverse(im)
    
    # Return it
    im
    
    
}
