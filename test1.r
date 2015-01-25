#The first function, makeCacheMatrix, exists only to initialize the cached variable (inv)
#and to define four functions - set, get, setsolve, and getsolve.

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#The second function, cacheSolve, takes the invertible matrix as an input.  It first uses the 'getsolve'
#function to see if there exists a cached version of the matrix, which it returns if it exists.
cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
#if the cached version does not exist, the function reads in the matrix and computes its inverse, stored in
#the variable 'data'.
        data <- x$get()
        inv <- solve(data, ...)
#for future runs, the value of the cached matrix is stored in the global environment using setsolve.        
        x$setsolve(data)
        inv
}


