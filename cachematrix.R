## These functions cache the inverse of a matrix

## This function creates a special "matrix" that is a list of four 
## functions. The first function set sets the value of the 
## matrix. The second function get returns the value of the matrix.
## The third function setinverse sets the value of the inverse of
## the matrix in the cache. The fourth function getinverse returns
## the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates the inverse of the special "matrix"
## created with the above funciton. If the inverse has already 
## been calculated then it simply returns the cached value of
## the inverse rather than computing it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
          message("getting cached data")
          return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
