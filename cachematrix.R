## This function creates a matrix object with 4 associated
## functions;
## set ()       Sets the matrix data
## get ()       Returns the matrix data
## setInv ()    Sets the inverse of the matrix
## getInv ()    Return the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    
    inv_cached <- NULL
    set <- function(y) {
        x <<- y
        inv_cached <<- NULL
    }
    get <- function() x
    setinv <- function(inv_set) inv_cached <<- inv_set
    getinv <- function() inv_cached
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
    
}

## Return a matrix that is the inverse of 'x'
## This function takes a 'makeCacheMatrix' object,
## it then checks to see if the inverse has been created already
## and either retruns the cached matrix or calculats and returns the
## matrix


cacheSolve <- function(x, ...) {
    
    inv_cached <- x$getinv()
    if(!is.null(inv_cached)) {
        message("getting cached data")
        return(inv_cached)
    }
    data <- x$get()
    inv_cached <- solve (data, ...)
    x$setinv(inv_cached)
    inv_cached
    
}
