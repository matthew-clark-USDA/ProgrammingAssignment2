## The functions below caches the Inverse of a Matrix
## The makeCacheMatrix and cacheSolve functions are used to create a matrix and cache its inverse.

##The below function creates a special matrix object that can be used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The below cacheSolve function computes the inverse of the special matrix created by the
## makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting inversed matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

#Test that the function work properly

qmatrix <- makeCacheMatrix(matrix(1:4,2,2))
qmatrix$get()
qmatrix$getInverse()
cacheSolve(qmatrix)

qmatrix <- makeCacheMatrix(matrix(c(2, 2, 1, 4), 2, 2))
qmatrix$get()
qmatrix$getInverse()
cacheSolve(qmatrix)


