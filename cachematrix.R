## Cache inverse of a matrix rather than compute it repeatedly
## Saves costly computation time

setwd('C:/Users/Jodi/Desktop/Coursera')
source("ProgrammingAssignment2/cachematrix.R")


## Creates a matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## Computes inverse of matrix returned by function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
}

mat <- makeCacheMatrix(matrix(1:4, 2, 2))
mat$get()
mat$getInv()
cacheSolve(mat)
mat$getInv()
