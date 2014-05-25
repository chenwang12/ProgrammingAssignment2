## Caching the inverse of a matrix 
## This function is aimed to save the time and cost of calculating a 
## inverse of a matrix repeatedly by caching the inverse for future use.


## makeCacheMatrix Computes the inverse of a special matrix
makeCacheMatrix <- function(x = matrix()) {
        ## if a matrix is called without a method
        m <- NULL
        ## set the value of the matrix
        set <- function (y){
                x <<- y
                m <<- NULL
        }
        ## get the value of the matrix
        get <- function()x
        ## set the value of inverse
        ## get the value of inverse
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## cache the inverse of matrix
## cacheSolve function retrieves the inverse of the matrix from the 
## cache,if the inverse has already been calculated by makeCacheMatrix. 
## Otherwise, it computes the inverse by function solve(x).


cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                message ("getting cached data")
                return(m)
                        }
        m <- solve(x$get())
        x$setInverse(m)
        m
}
