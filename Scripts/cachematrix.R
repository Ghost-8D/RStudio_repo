## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. This pair 
## of functions cache the inverse of a matrix to prevent unnecessary computations.
## 
## Usage example (for invertible matrix):
## > cm1 <- makeCacheMatrix(matrix(c(-1, 1, 1.5, -1), 2, 2))  # initialize object
## > cacheSolve(cm1)  # calculate inverse matrix and save to cache
## > cacheSolve(cm1)  # retrieve inverse from cache


## This function creates a cache matrix object which gives the ability to cache
## the inverse matrix (of an invertible matrix) and return it on demand. If the
## provided matrix is not invertible a message will appear.

makeCacheMatrix <- function(x = matrix()) {
    # If determinant of the matrix is zero, the matrix is not invertible.
    # This trick prevents underflow issues.
    if (is.infinite(determinant(x)$modulus)){
        message("Warning! The provided matrix is not invertible.")
        message("\tYou can use the set() function to use a different matrix.")
    }
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) m <<- inv
    get_inv <- function() m
    list(set = set, get = get,
        set_inv = set_inv,
        get_inv = get_inv)
}


## This function checks if the inverse of the matrix is cached. If the inverse
## was cached before it returns the inverse matrix from the cache, otherwise it
## calculates the inverse, saves it in cache and returns it. If the matrix is
## not invertible an error message will appear and nothing will be returned.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$get_inv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    # If matrix is not invertible display error message and return nothing
    if (is.infinite(determinant(data)$modulus)){
        message("Error! The provided matrix is not invertible.")
        # matrix(NA, nrow(data), ncol(data)) # to return matrix with NA
    } else {
        m <- solve(data, ...)
        x$set_inv(m)
        m
    }
}

