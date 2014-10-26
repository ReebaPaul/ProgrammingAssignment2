## Function to cache the inverse of a matrix

## makeCachematrix uses solve function in R to get the inverse
## matrix, but stores the result in a variable i, so that computation 
## in repeated request can be avoided.
makeCacheMatrix <- function(x = matrix()) {
 	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function which returns the inverse , uses cache if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inv data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
