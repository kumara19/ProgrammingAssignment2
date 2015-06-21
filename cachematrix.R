## These functions introduce a special matrix object which caches its inverse.
## The create such a special matrix m you execute \code{m <- makeCacheMatrix(x)}
## where x is an ordinary matrix. You can then get the value with \code{m$get()}
## and change the value with \code{m$set(y)} where y is an ordinary matrix.
## You can get the inverse with \code{cacheSolve(m)}.


#' Create a special "matrix" object that can cache its inverse.
#' 
#' @param x A matrix
#' 
#' @return A list containing four functions to set and get the value of the
#'     matrix and to set and get the inverse of the matrix
#'     

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    
    set <- function(y) { 
        x <<- y  # Set the value
        m <<- NULL  # Clear the cache
        # Define function to get the value of the matrix
    }
    get <- function() x
    # Define function to set the inverse. This is only used by getinverse() when
    # there is no cached inverse
    setinverse <- function(inverse) m <<-inverse
    # Define function to get the inverse
    getinverse <- function() m
    
    # Return a list with the above four functions
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


#' Return inverse of matrix x
#' 
#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the cachesolve retrieves the 
#' inverse from the cache.
#' 
#' @param x a special matrix created with makeCacheMatrix
#' 
#' @return The inverse of the matrix x
#'

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse() # This fetches the cached value for the inverse
    inverse <- makeCacheMatrix$getinverse()
    if(!is.null(m)) {
        # If the cache was not empty, we can just return it
        message ("getting cached today")
        return(m)
    }
    # The cache was empty. We need to calculate it, cache it, and then return it.
    data <- x$get() # Get value of matrix
    m <- solve(data) # Calculate inverse
    x$setinverse(m) # Cache the result
    return(m) # Return the inverse
}
