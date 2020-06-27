## This whole function works to cache the inverse of a matrix.

## This function creates a special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list (set=get,
      get=get,
      setInverse=setInverse,
      getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by the above function 
##If the inverse has already been calculated then this should return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
