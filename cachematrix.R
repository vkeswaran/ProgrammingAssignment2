## The functions below will create a list of functions for a given square matrix and will cache the inverse of the same

## This function creates a special matrix object that consists of a matrix and functions to operate on that matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


## This function, given the special matrix object, will calculate its inverse if not already available and sets it into the
## objects cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrixdata <- x$get()
  i <- solve(matrixdata)
  x$setInverse(i)
  i
}