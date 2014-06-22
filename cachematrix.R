## Two functions to calculate the invers of a matrix
## If the inverse of a matrix has already been calculated the 
## result will be retrieved from a cache to avoid recalculating


## makeCacheMatrix creates a list of 4 functions ($set, $get, $setinverse, 
## $getinverse) A matrix can be input into this list using $set.
## This list can then be passed to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a makeCacheMatrix object and calculates 
## the inverse of the matrix present. If the inverse has not previously
## been calculated it is stored in the cache. If it has it is retrieved
## from the cache and no calculation takes place.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

