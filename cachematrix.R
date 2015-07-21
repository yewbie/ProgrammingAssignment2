## The first function creates a special "matrix". The second function calculates
## the inverse fo the special "matrix".

## This function creates a special "matrix" object that can cache its inverse.

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


## This function computes the inverse of a special matrix 'x'.
## If the inverse has already been calculated (and the matrix has not changed), 
## then this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
