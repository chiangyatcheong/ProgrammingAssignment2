## The following function calculates the inverse of the matrix
## created with the above function. However, it first checks to see if the
## mean has already been calculated. If so, it gets the inverse from the
## makeCacheMatrix and skips the computation. Otherwise, it calculates the 
## inverse using cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## The below function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

## This assumes that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
