## The following funcitons can cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invx <<- solve #calculate the inverse of x
        getinverse <- function() invx
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        invx <- x$getinverse()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinverse(invx)
        invx
        ## Return a matrix that is the inverse of 'x'
}

