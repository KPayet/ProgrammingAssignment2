## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
    
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

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
