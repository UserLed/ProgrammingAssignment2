## Put comments here that give an overall description of what your
## functions do

## make a matrix, set initial value to null and set the matrix into cache

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
               set <- function(y) {
                x <<- y
                i <<- NULL
                }

        get <-function(){ x }
        setinv <- function(solve) {i <<- solve}
        
        getinv <- function() {i}
        list(set = set, get = get, 
                setinv = setinv,
                getinv = getinv)
}


## solve matrix using stored cache value if available, otherwise solve if NULL 

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
                i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
        
}
