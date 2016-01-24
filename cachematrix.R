## makeCacheMatrix(x): make a cacheable matrix
##      arguments:
##              x: a matrix
##      return: a list represent the cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
        ## cache for the inverseMatrix
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(i) inverseMatrix <<- i
        getInverseMatrix <- function() inverseMatrix
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}

## cacheSolve(x, ...): solve the cacheable matrix
##      arguments:
##              x: a cacheable matrix returned by makeCacheMatrix()
##      return: inverse of x

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverseMatrix()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        matrix <- x$get()
        inverseMatrix <- solve(matrix, ...)
        x$setInverseMatrix(inverseMatrix)
        inverseMatrix
}
