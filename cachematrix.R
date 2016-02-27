## The following code creates an efficient mechanism to calculate
## and cache the inverse of a matrix.  The cached value can be
## retrieved at O(1)

## Model object for the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() {
        return(x)
    }
    set <- function(x) {
        x <<- x
        inv <<- NULL
    }
    getInverse <- function() {
        return(inv)
    }
    setInverse<- function(inv) {
        inv <<-inv
    }
    list(get = get,
         set = set, 
         getInverse = getInverse,
         setInverse = setInverse)
}


## Controller for matrix object
## Retrieves cached value if it exists, otherwise calculates the 
## matrix inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    } else {
        inv <- solve(x$get())
        x$setInverse(inv)
        return(inv)
    }
}
