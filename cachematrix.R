## makeCacheMatrix provides an object for storing the resulting inverse matrix of
## an initial matrix.  Object provides set and get methods for access to the
## matrix inverse if it has already been calculated

## An object for storing the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # returns the initial matrix
    get <- function() x
    
    # store the matrix inverse so we don't re-compute it next time cacheSolve is called
    setMatrixInverse <- function(matrixInverse) m <<- matrixInverse
    
    # return the cached inverse matrix
    getMatrixInverse <- function() m
    
    list(get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
    
}


## Calculates the inverse of the provided cache matrix object, but first looks for a cached
## result and that the original matrix is the same.  If the inverse has already been calculated
## the function will return the precomputed matrix otherwise it will call solve
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrixInverse()
    
    # check to see if the matrix we've been passed is the same as what we may have a cached a result for
    # also check if we already have the matrix inverse calculated
    if(exists("origMatrix")) { 
        if(identical(origMatrix, x$get()) & (!is.null(m))) {
            message("getting cached data")
            return(m)
        }
    }
    origMatrix <<- x$get()    
    # calculate the matrix inverse
    m <- solve(origMatrix)
    # cache the solution
    x$setMatrixInverse(m)
    # return the matrix inverse
    m
}
