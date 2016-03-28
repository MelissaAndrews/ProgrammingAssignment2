## These functions are used to cache and recall the inverse of a matrix.
## It is assumed the supplied matrix is always invertible.

## This function creates a special matrix with the following events.
## set = set the value of the matrix, get = get the value of the matrix.
## setinverse = set the inverse of the matrix, getinverse = get the inverse of the matrix.

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


## This function calculates the inverse of a special matrix created with the makeCacheMatrix.
## If the inverse has already been calculated, the cached inverse is returned.
## Otherwise, the inverse is calculated using solve() and stored in the special matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}
