## Put comments here that give an overall description of what your
## functions do

## 1. makeCacheMatrix: creates a special "matrix" that can cache its iverter.
## 2. cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function
#  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # Define function to set the value for the matrix.
                  # Also clears clears the cache
        set <- function(y) {
                x <<- y         # Set the value
                m <<- NULL      # Clear cache
        }
        get <- function() x     # Define the function to get the value of the matrix
                                # Also funvrion to set inverse, used by getm() when there is no cached inverse
        setm <- function(inverse) m <<- inverse
        getm <- function() m    # Defin a function to get the inverse
        
        # Return a list of all functions
        list(set = set, get = get, setm = setm, getm = getm)

}

## Write a short comment describing this function
#  This function computes the inverse of the special "matrix" returned 
#  by makeCacheMatrix above. If the inverse has already been calculated 
#  (and the matrix has not changed), then the cachesolve retrieves the 
#  inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getm()     # Fetch the cached value for the inverse
        if(!is.null(m)) { # Here we return cache if it is not empty
                message("getting cached data")
                return(m)
        }
        # The cache is empty, it calculate if, then cache it and return it
        data <- x$get()         # Get the value of matrix
        m <- solve(data, ...)   # Calculate the inverse
        x$setm(m)               # Cache the result
        return(m)               # Return the inverse

}
