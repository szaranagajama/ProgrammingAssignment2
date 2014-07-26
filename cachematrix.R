

## The purpose of these functions is to create a matrix object 
## and store its inverse in cache. Subsequnt function first checks 
## if there is an inverse matrix stored in cache if so recalls it if not
## inverses it and puts it in cache
## Assumption is being made that provided matrix is always invertible


## The function makecacheMatrix creates a special "matrix" object, 
## and stores its inversed version in cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## sets m to null
        set <- function(y) {
                x <<- y ##sets the vaue of x in the enclosing environment
                m <<- NULL ## sets vaue of m to Null in enclosing environment, 
		               ##because the cached vaue is no longer valid
        }
        get <- function() x ## defines function, returns x
        setinverse <- function(z) m <<- z ## defines function, assignes value to m in enclosing environment
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special matrix
## returned by makeCacheMatrix but first checks if the inverse 
## has already been calculated (and the matrix has not changed)
## If inverse matrix has been calculated retrives its value
## if not calculates it and stores in cache.

## The function will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	  m <- x$getinverse() ##gets the inversed matrix from cache
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)##checks if m is null 
                ## if not returns the inversed matrix from cache
        }
        data <- x$get()
        m <- solve(data)##inverses provided matrix
        x$setinverse(m)## and stores it in the cache
        m ##returns inversed matrix
}        


