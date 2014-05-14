##
## The first function, makeCacheMatrix, creates a special "matrix" object
## which can cache its inverse. 
## It returns a list of functions to:
## - set the value of the matrix (set)
## - get the value of the matrix (get)
## - set the value of the inverse (set_inverse)
## - get the value of the inverse (get_inverse)
##
##########################################################################
##
## The second function, cacheSolve, calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix. It first checks if the 
## inverse has already been calculated. If so, it gets the inverse from 
## the cache (using get_inverse) and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the cache via the 
## set_inverse function.
##
##########################################################################
##
## Example use:
## x <- matrix(rnorm(25), nrow = 5)          Create a square matrix
## y <- makeCacheMatrix(x)                   Create the special matrix obj
## y$get()                                   Get the special matrix
## cacheSolve(y)                             Calculate and get the inverse
## cacheSolve(y)                             Call again to get cached inverse
##                                            
##########################################################################

## Getters and setters for the matrix (my_matrix) and inverse (cached_inverse)

makeCacheMatrix <- function(my_matrix = matrix()) {
        cached_inverse <- NULL                  # cache empty until it gets set
        
        set <- function(new_matrix) {
                my_matrix <<- new_matrix        # set the new matrix value
                cached_inverse <<- NULL         # old cache value invalidated 
        }
        
        get <- function() my_matrix             # getter, return the matrix
        
        set_inverse <- function(inverted_matrix) {
                cached_inverse <<- inverted_matrix      # set the cache value
        }        
        
        get_inverse <- function() cached_inverse        # getter, return cache
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Look up or calculate the inverse of the matrix.  If possible, return a 
## valid cached value, otherwise solve for it and stuff it in the cache
## for subsequent use.

## Assume a square matrix that is invertible.  If not, would use alternatives
## such as ginv (generalized invert) from the MASS package.
##     library(MASS)
##     inverse <- ginv(data,...)

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()   # see if the cache is populated
        
        if (!is.null(inverse)) {
                message("using cached data")
                return(inverse)         # exit returning the cached value
        }
        
        ## no valid cache, let's calculate it
        
        ## Assume a square matrix that is invertible.  If not, would use 
        ## alternatives like ginv (generalized invert) from the MASS package.
        ##     library(MASS)
        ##     inverse <- ginv(data,...)
        
        data <- x$get()                 # get the special matrix
        inverse <- solve(data, ...)     # works only for square matrix (see
        ##                                above for alternatives)
        
        x$set_inverse(inverse)          # cache it for next time
        return(inverse)                 # return the just calculated inverse
}
