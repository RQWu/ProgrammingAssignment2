## The following function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        iv  <- NULL
        set  <- function(y){
                x <<- y
                iv <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) iv  <<- inverse
        getinverse  <- function() iv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function computes the inverse. 
## If the inverse has already been calculated,then it should retrieve the inverse from the cache.
## If not, it computes the inverse and sets the value in the cache via setinverse function.


cacheSolve <- function(x, ...) {
        iv  <- x$getinverse()
        if (!is.null(iv)){
                message("getting cached data")
                return(iv)
        }
        data  <- x$get()
        iv  <- solve(data, ...)
        x$setinverse(iv)
        iv
}
