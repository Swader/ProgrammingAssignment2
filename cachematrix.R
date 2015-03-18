## These functions are for programming assignment 2 and are used to calculate
## and cache the inverse of a square, inversible matrix. A matrix is inversible
## if and only if its determinant is not equal to 0

## The following functions can be used like this:
## m <- matrix(1:4,2,2)
## newM <- makeCacheMatrix(m)
## cacheSolve(newM)

## This function first creates a variable to hold the inverse matrix.
## Then, the set method is defined which can be used to change the underlying
## original matrix. In this method, the x variable from the parent scope is
## overridden with the new value and the inverse is reset to null.
## The get method just returns the originally passed in matrix.
## The setInverse method is used to manually set the inversed matrix, for cache
## GetInverse will return the inverse, whether it still be NULL or a real value
## Finally, this function returns a list which behaves like an object with
## methods (get(), set(), etc...)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() return(x)
    setInverse <- function(inversion) {
        inverse <<- inversion
    }
    getInverse <- function() return(inverse)
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function gets the inversion from our "custom matrix" as given by the
## makeCacheMatrix function. If the inversion still hasn't been calculated (i.e
## it is still NULL) then this function gets the original matrix, solves it, and
## manually sets the inversion as a property of the matrix object with 
## x$setInverse. Otherwise, it just returns the inversion if already calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inversion <- x$getInverse()
    if (!is.null(inversion)) {
        return(inversion)
    }
    
    matrix <- x$get()
    inversion <- solve(matrix)
    x$setInverse(inversion)
    inversion
}
