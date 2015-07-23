## These functions provide an efficient method for inverting matrices.
## Given some matrix m, makeCacheMatrix will create a list of functions
## that can be passed to cacheSolve in order to return the inverse of m.

# Use: create matrix m and pass to makeCacheMatrix. Pass result to cacheSolve.

## makeCacheMatrix creates a list of functions to be performed on object x
## These functions allow the initial and inverted matrices to be set and accessed. 

makeCacheMatrix <- function(x = matrix()) {
        # clear any past values of inv
        inv <- NULL
        # set is used to change the contents of the matrix stored in x
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        # get is used to return the matrix stored in x
        get <- function() x
        # setinv is used to set the matrix inv 
        setinv <- function(inverted) inv <<- inverted
        # getinv is used to get the matrix inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Given a matrix stored using set in above function, now efficiently invert it.
## cacheSolve takes the matrix x and first checks if an inversion is saved in cache.
## If so, it returns the cached version and exits.
## If not, it computes the inverse, stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Fetch cached version of inv
        inv <- x$getinv()
        ## Check to see if cached version exists. If so, return it and end.
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        ## If not, rest of code will execute, as if an else statement present.
        ## Store cached matrix in sata variable.
        data <- x$get()
        ## perform inversion and store in inv
        inv <- solve(data, ...)
        ## store new value of inv in cache
        x$setinv(inv)
        ## return the inverted matrix.
        inv
}