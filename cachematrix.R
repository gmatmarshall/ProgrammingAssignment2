

## This creates a four functions that are made specifically for the matrix in the argument. 
## If the argument is x, then we access setinverse function using x$setinverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        

}


## This uses the functions created by makeCacheMatrix() to compute the inverse
## if and only if it has not been ccomputed before.

cacheSolve <- function(x, ...) {
        inv<- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
