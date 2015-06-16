## The functions cache inverse of matrix where it needs to be used often.

## makeCacheMatrix contains functions to store the inverse of a matrix
## so that it can be accessed without having to be computed again.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x        ## gets original matrix
  setinverse <- function(inverse) i <<- inverse   ## inverse is cached during first execution
  getinverse <- function() i                      ## inverse is returned from cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## casheSolve checks if the inverse of the matrix is cached, and 
## and returns the inverse from the cache. If cache is empty, it
## computes the inverse and caches the value for future reference.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()           ## check cached value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)                   ## if cache is not NULL, return inverse from cache
  }
  data <- x$get()               ## if cache is NULL, compute inverse
  i <- solve(data, ...)
  x$setinverse(i)               ## store inverse in cache
  i                             ## Return a matrix that is the inverse of 'x'
}
