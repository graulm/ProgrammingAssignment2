## This document include two funstions: makeCacheMatrix and cacheSolve
## which are part of the Programming Assigment 2

## makeCacheMatrix creates the special "matrix" object that cache the inverse of the "matrix"
## The input paramet must be a square matrix

makeCacheMatrix <- function(x) {
        if (class(x) != "matrix") {
                stop('input parameter must be a matrix, you entered: ', class(x))
        }
        if (dim(x)[1] != dim(x)[2]) {
                stop('matrix must be square, you entered a matrix with dimemsion: ', dim(x)[1], ' x ', dim(x)[2])
        } 
        
        
        m <- NULL
        set <- function(y) {
                x <<- y 
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse already exists in the cache, the fucntion return it form the cache.
## Otherwise, the inverse is calcualted, cache it, and returned back to the caller.

cacheSolve <- function(x, ...) {
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
