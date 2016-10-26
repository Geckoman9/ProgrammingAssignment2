#This code creates an action to take a matrix and create an inverse of it.
#If that inverse has already been saved into the environment,
#The second function will call the cached version.

## Creates a set of functions that allow the next function to scope them
## and check for the cache of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(v){
    x <<- v
    matinv <<- NULL
  }
  matx <- function() x
  setinv <- function(invmat) matinv <<- invmat
  invmatx <- function() matinv
  list(set = set, matx = matx, setinv = setinv, invmatx = invmatx)
}


## Either pulls a cached version of the inverse matrix or creates the 
## inverse matrix.

cacheSolve <- function(x, ...) {
  matinv <- x$invmatx()
  if(!is.null(matinv)) {
    message("Retrieving cached data.")
    return(matinv)
  }
  matx <- x$matx()
  matinv <- solve(matx)
  x$setinv(matinv)
  matinv
  ## Return a matrix that is the inverse of 'x'
}
