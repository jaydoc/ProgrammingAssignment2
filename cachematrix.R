## The following function creates a list (in this context, a matrix) containing a function to
## set and get the values of the matrix and its inverse.

## function that creates a special matrix which can then cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL   # since the matrix changed
    }
   
    get <- function() x
    
    setinv <- function(inv_) inv <<- inv_
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  
}

## to compute the inverse of above matrix. If not available it calculates and sets the value in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv        
}
