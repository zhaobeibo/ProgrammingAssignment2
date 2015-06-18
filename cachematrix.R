## Overall Description
## This program can cache the inverse of a matrix so as to avoid
## computing its inverse repeatedly. In order to do so, a special
## "matrix" object is created to cache its inverse. Before the inverse
## of the special "matrix" object is going to be computed, the program
## will check if the computation has been done before. If that's the case
## the program will retrieve the inverse from the cache without computing


## function makeCacheMatrix.R creates a special "matrix object
## that can cache the inverse of the matrix

makeCacheMatrix <- function(x=matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get =  get,
       setinv = setinv,
       getinv = getinv)
}

## function cacheSolve.R computes the inverse of the special "matrix 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated, then the cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
## Return a matrix that is the inverse of 'x'
}
