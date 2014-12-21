## Coursera R Programming
## author: Ken Lynch
## date  : Dec. 20, 2014
## These functions demostrate R capability of caching complex operations
## to improve performance for repeatedly calling an operation where the 
## result does not change.

## Function: makeCacheMatrix - stores a matrix with additional functions 
## to get or set the source matrix and get or set the inverse of the matrix
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


## Function: cacheSolve - uses the functions provided by a matrix stored with 
## makeCacheMatrix to either return the cached inverse matrix stored in makeCacheMatrix 
## or if it does not exist, the inverse matrix will be calculated, stored in makeCacheMatrix 
## and returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }

	  ## otherwise, we're calculating the inverse and returning
 
        data <- x$get()
        m <- (solve(data,...) %*% data)
        x$setinverse(m)
        m
}
