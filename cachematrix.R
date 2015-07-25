# The following two functions work as a pair to
# calculate and cache the inverse of a special
# "matrix", avoiding potentially time-consuming,
# repeated computations.

# This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}

# This function computes the inverse of the special
# "matrix" above or retrieves it if already solved.
# Assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
