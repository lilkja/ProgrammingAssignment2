## This is the modified version of makeVector example.
## Instead of caching the mean of a vector, the functions are used 
## to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This is a modified version of the cachemean example.
## Instead of returning the mean of a vector, it returns the
## inverse of a matrix - it first checks if the inverse
## has already been found.

cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinverse(i)
    i
}
