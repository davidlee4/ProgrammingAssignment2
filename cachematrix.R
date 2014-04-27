## Functions to allow the creation of a matrix, where we can cache the inverse.
## This should allow the reuse the inverse without having to recalculate.

## Function below creates our matrix, where we will then be able to store the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        }


## The following function calculates the inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been calculated.
##If it has it gets the inverse from the cache. Otherwise, it calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }
}
