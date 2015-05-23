## Functions to create a 'cached matrix' (capable of caching its inverse),
## and getting the inverse of a cached matrixs

## Converts a matrix into a cached matrix, capable of caching its inverse

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the cached matrix x

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
