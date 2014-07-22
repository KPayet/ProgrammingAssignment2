## cachematrix.R: Kevin Payet
## Code to compute the inverse of a matrix, and cache it so that the computation
## is done only one time. The work is divided between two functions:

## makeCacheMatrix returns a list of functions:
## set & get:                   used to set and retrieve the matrix M
## set_inverse & get_inverse:   used to set and retrieve the inverse I_M of M
## both matrices are cached so that the computation is done only once (by cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    set_inverse <- function(inv) {
        inverse <<- inv
    }
    
    get_inverse <- function() {
        inverse
    }
    
    list(set = set, get = get, 
         set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve looks for the inverse of the matrix in the list parameter
## if found, it returns the inverse and does nothing else
## if not found, it computes the inverse of the matrix and caches it with 
## set_inverse()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        print("Retrieving cached computation of inverse...")
        return(inverse)
    }
    
    print("No cached data. Computing inverse...")
    
    mat <- x$get()
    
    inverse <- solve(mat)
    
    x$set_inverse(inverse)
    
}
