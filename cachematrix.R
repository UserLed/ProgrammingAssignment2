## create a function that sets initial inverse matrix value to NULL and assigns object values using <<- operator

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL               ## i = inverse matrix

                set <- function(y) {
                x <<- y         ## using  <<- operator to assign a value to an object
                i <<- NULL      ## in an environment that is different from the current environment
        }


## functions to print/return matrix, set matrix inverse &
## return a inverse matrix that will be called by cacheSolve function below


        get <-function(){ x }     ## returns the vector input to makeCacheMatrix function
        setinv <- function(solve) {i <<- solve}  ## sets matrix inverse using solve()
        getinv <- function() {i}                ## returns matrix inverse set by setinv
        list(set = set, get = get,              ## aggregate 4 values in a list
        setinv = setinv,
        getinv = getinv)
}


## return a matrix that is the inverse of x
## use stored cache value if available, otherwise solve if NULL

cacheSolve <- function(x=matrix(), ...) {

                i <- x$getinv() ## calls get(inv) function of x
                if(!is.null(i)) {
                message("getting cached data") ## indicates to command line user "from cache"
                return(i) #return cached inverse matrix if is NOT NULL
        }
        data <- x$get() ##otherwise use get to set x to data
        i <- solve(data, ...)   ## and solve (create) a matrix that is inverse of x
        x$setinv(i)             ##store the inverse matrix just created
        i                       ## print result (inverse matrix)
}