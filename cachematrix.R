## Matrix inversion is usually a costly computation 
## The functions below cache the inverse of a matrix rather than compute it repeatedly


## Take a matrix and return the list of 4 functions (get,set, getinverse, setinverse) 
## The matrix inversion result is cached inside

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list (set=set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## computer the inverse matrix based on the speical "matrix" created by makeCacheMatrix
## It first checks to see if the inverse matrix has already been computered. 
## If so, it returns the result from cache. Otherwsie, it computers the inverse matrix 
## and set the data in the cache via setinverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (! is.null(i)) {
        message("getting cached inversed matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
