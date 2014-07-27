## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( x = matrix() ) {
  inv <- NULL
  setMatrix <- function( y ) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function( aInverse ) inv <<- aInverse
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from the cache.
## @return
##	 a matrix that is the inverse of 'x'
cacheSolve <- function( x, ... ) {
  inv <- x$getInverse()
  if( !is.null( inv ) ) {
    message("getting cached data")
    return( inv )
  }
  message("compute new data and store in cache")
  data <- x$getMatrix()
  inv <- solve(data, ...)
  x$setInverse( inv )
  inv
}
