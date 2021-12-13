## makeCacheMatrix and cacheSolve are combined to cache the inverse of a matrix.

## makeCacheMatrix creates a list that can set, get, set the inverse, and get the inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes the output of makeCacheMatrix to return the inverse of the matrix, if the inverse is not set in
##            the output of makeCacheMatrix, then cacheSolve solves for the inverse and sets the inverse in makeCacheMatrix

cacheSolve <- function(x, ...) {
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
