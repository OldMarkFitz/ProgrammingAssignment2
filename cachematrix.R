## These functions will return the inverse of a supplied matrix, however 
## if the same matrix is passed as an input parameter, it will return
## the cached result( i.e will not recalculate the result).

## This function sets and gets the inverse matrix and will cache it

makeCacheMatrix <- function(x = matrix()) {
        m <-NULL
        set <- function(y){
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will get the cached inverse matrix or calculate
## it if it's the first time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        t <- x$getInverse()
        if (!is.null(t)) {
            message("getting cached data")
            return(t)
        }
        mat <- x$get()
        t <- solve(mat)
        x$setInverse(t)
        t
}
