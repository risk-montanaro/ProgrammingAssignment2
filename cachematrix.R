## The two functions below will cache the inverse of a matrix

## The function makeCacheMatrix will create a special object
## that cache its inverse. Please input an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) m <<- solve
    get_inverse <- function() m
    list(set = set, get = get, 
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## The function cacheSolve computes the inverse of the special object
## returned by the function above. The output is the inverted matrix

cacheSolve <- function(x, ...) {
    m <- x$get_inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m
}
