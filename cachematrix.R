## Code to calculate the inverse of a matrix or retrieve the inverse
## from cache if it has already been calculated

## Creates a special matrix object, returns a list that sets the value of the matrix,
## gets the value of the matrix, sets the inverse and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Calculates the inverse of the special matrix created above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
