## 'cacheSolve' calculates the inverse and caches
## Returns cached inverse on next call

## Stores the inverse and the matrix
## Clears cached inverse, if matrix changes

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse <<- inverse
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## If the inverse is already in cache, then returns cached
## Else, calculates the inverse using solve function in R library and caches
## Passes ... arguments to solve for inverse

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() ## Check for cached results using getInverse
  if(!is.null(inv)) {
    message("Returning cached inverse")
    return(inv) ## Return cached inverse
  }
  mtx <- x$get() ## get Matrix
  inv <- solve(mtx, ...) ## Calculate inverse
  x$setInverse(inv) ## Cache calculated results
  inv
}
