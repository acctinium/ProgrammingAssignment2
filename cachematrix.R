# This function performs a Matrix inversion and cashes the result.
# Matrix inversion is usually a costly computational process and 
# there may be benefit in caching the result rather than calculating 
# it repeatedly.


# makeCacheMatrix creates a special "matrix", which is really a 
# list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL 
    set <- function(y) { 
        x <<- y 
        inv <<- NULL 
    }
    get <- function() x 
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


# The cacheSolve function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed.
# If so, it returns the result without further computation.
# Otherwise it computes the inverse and sets it to the cache value.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get() 
    inv <- solve(data, ...)
    x$setinverse(inv) 
    inv
}