## This pair of functions caches the inverse of an invertible matrix.

## Sample input follows:
##    mymatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##    cacheSolve(mymatrix)

## This function sets and gets the value of the matrix,
## and sets and gets the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(solve) {
                inv <<- solve
        } 
        getinv <- function() {
                inv
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function checks to see if the inverse of the matrix has already
## been caclulated. If it has been calculated, it retrieves the cached
## inverse. Otherwise, it calculates the inverse.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Retrieving cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}