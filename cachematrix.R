## The functions will create a matrix and store the inverse of the matrix. 
## If the inverse of the matrix is already calculated, the value will return 
## from the cache with a message, instead of calculating it again.

## The makeCacheMatrix function creates a matrix that will cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setM <- function(y) {
        x <<- y
        i <<- NULL
    }
    getM <- function () x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(setM = setM, getM = getM, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the matrix
## return the cached value with a message. 

cacheSolve <- function(x, ...) { 
    i <- x$getInverse() ## get cache data
    if(!is.null(i)) { 
        message("getting cached inverse matrix")
        return(i)
    } else {
    data <- x$getM()
    i <- solve(data, ...)
    x$setInverse(i)
    return(i)
    }
}
