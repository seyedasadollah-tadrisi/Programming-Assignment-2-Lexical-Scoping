## Pair of functions that cache the inverse of a matrix 

## makeCacheMatrix caches the inverse of a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL 
 set <- function(y) {
   x <<- y 
   inv <<- NULL
 }
 get <- function() x
 setinverse <- function(solve) inv <<- solve
 getinverse <- function() inv 
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix",
## and returns the inverse from the cache if it has already been calculated. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  ## If the inverse of 'x' has already been calculated then return
  ## the inverse  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}