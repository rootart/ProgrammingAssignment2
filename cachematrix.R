## Introduce `makeCacheMatrix` constructor function and `cacheSolve` function in 
## order to optimize getting of inverted matrix by caching already calculated results.

## Constuctor function for matrix object 
## with stores inverted version of it.

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL;
    set <- function(y) {
            x <<- y
            inverted <<- NULL
    }
    get <- function() x
    setinverted <- function(invertedValud) inverted <<- invertedValud
    getinverted <- function() inverted
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}


## Returns inverted matrix either from the cached version
## or by explicitely calculating it with help of `solve` function.
## x has to be `makeCacheMatrix` object.

cacheSolve <- function(x, ...) {
    inverted <- x$getinverted()
    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    data <- x$get()
    inverted <- solve(data, ...)
    x$setinverted(inverted)
    inverted
}
