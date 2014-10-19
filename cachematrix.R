## Functions to make a matrix and to calculate and cache it's inverse

## The wrapper function with methods to set, cache and return matrix and it's inverse
##      Make new matrix like my_matrix <- makeCacheMatrix(1:4,2,2
##      Methods:
##      'set' - to change existing matrix
##      'get' - to get matrix data
##      'setInv' - used by cacheSolve function to cache matrix inverse, not intended to be invoked manually
##      'getInv' - gets cached inverse, could, but not intended to be called manually
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inv2) inv <<- inv2
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The function to calculate, put to cache, and return fresh or cached inverse 
##      Use like cacheSolve(my_matrix), where is my_matrix contents from previous function
##      Calculates, returns and store in cache (via 'setInv' invoke) inverse if called for a matrix for the first time
##      Returns cached inverse via 'getInv' if called for the matrix for the second time
cacheSolve <- function(x, ...) {
        inv2 <- x$getInv()
        if(!is.null(inv2)) {
                message("getting cached data")
                return(inv2)
        }
        data <- x$get()
        inv2 <- solve(data, ...)
        x$setInv(inv2)
        inv2
}
