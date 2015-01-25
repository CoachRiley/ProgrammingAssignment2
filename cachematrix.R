## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse.
##  To properly execute this code, assign a variable to hold the list using code in the console window
##  such as "Var1 <- makeCacheMatrix(dat)" where dat is a variable holding the matrix to be inverted.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(bob) inv <<- bob
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

## Continuing with the above syntax, run this code with the following command:
## "cacheSolve(Var1)"

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
