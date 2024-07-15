## These functions cache the inverse of a matrix to avoid recomputing it.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse property

    ## set: Method to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse when the matrix is set
    }

    ## get: Method to get the matrix
    get <- function() x

    ## setInverse: Method to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse

    ## getInverse: Method to get the inverse of the matrix
    getInverse <- function() inv

    ## Return a list of the methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Retrieve the inverse from the cache
    inv <- x$getInverse()

    ## If the inverse is already set, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## Get the matrix from the object
    mat <- x$get()

    ## Calculate the inverse using solve function
    inv <- solve(mat, ...)

    ## Set the inverse to the object
    x$setInverse(inv)

    ## Return the inverse
    inv
}