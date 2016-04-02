## Matrix inversion is costly so these functions cache the inverse of a matrix, 
## rather than repeatedly computing it. First, call makeCacheMatrix to create the
## maker matrix object. Then pass this object into cacheSolve to calculate the
## inverse. Call getinverse (e.g. y$getinverse) to test that the matrix has been
## cached.
## NB. Creating a simple 2x2 square matrix of 4,2,7,6 should return an inverse of
## 0.6, -0.2, -0.7, 0.4

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## get and set functions get and set the original matrix.
## getinverse and setinverse get and set the inverse matrix
## returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## define the set function which assigns the value of y to our special
    ## matrix, and sets our inverse matrix to NULL.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## define the get function which returns x
    get <- function() x
    
    ## define the setinverse function which assigns the inverse to i
    setinverse <- function(inverse) i <<- inverse
    
    ## define the getinverse function which returns i (the inverse)
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated then it returns the cached data instead.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## call getinverse and check whether it is null, to see whether inverse is cached
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        ## return the cached inverse
        return(i)
    }
    ## inverse hasn't been cached so get the special matrix.
    data <- x$get()
    ## run solve() to calculate the inverse of the matrix and assign to i
    i <- solve(data, ...)
    ## call setinverse to assign the value of i to the cache variable
    x$setinverse(i)
    ## return the calculated inverse
    i
}
