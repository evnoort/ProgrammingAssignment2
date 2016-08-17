## This is the implementation of a cache mechanism to hold the inverse of a matrix

## Post: Return a cache matrix object for matrix x.
## The cache matrix object stores the matrix and its inverse matrix value  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Post: The inverse of the cache matrix object x is returned either from cache
## or from calculation (if not in cache). On calculation the inverse is stored in the cache matrix object.  

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        m
    }
    else {
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
    }
}
