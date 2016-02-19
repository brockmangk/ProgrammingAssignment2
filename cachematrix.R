## The following two functions work together to compute and cache the inverse of a square matrix given as a parameter.
## Cached results will be retrieved instead of re-calculating the inverse of any matrix already processed.

## The makeCacheMatrix function will create a special matrix object designed to cache its inverse.  It leverages lexical scoping.

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


## When called with a square matrix as parameter, cacheSolve will use makeCacheMatrix to calculate the inverse or retrieve it
## from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
