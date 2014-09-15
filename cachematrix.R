## The functions below are used to create a special object
## that stores a matrix and to compute and cache the inverse
## of that matrix

## makeCacheMatrix() creates a special object that stores
## a matrix and caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) {
        ## Check if input value is the inverse of x
        if(!identical(inverse, solve(x))) {
            warning("Input is not the inverse of x.")
        }
        inv <<- inverse
    }
    get_inverse <- function() inv
    
    list(set = set, get = get, 
         set_inverse = set_inverse,
         get_inverse = get_inverse)    
}

## cacheSolve() computes and returns the inverse of the
## inputed matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("Retrieving cached data ...")
        return(inv)
    }
    data <- x$get()
    
    ## Check if x is a square matrix
    if(ncol(data) != nrow(data)) {
        stop("Input matrix is not square.")
    } 
    
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}
