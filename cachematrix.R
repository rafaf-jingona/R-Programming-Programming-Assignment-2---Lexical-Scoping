## Caching the Inverse of a Matrix
## Matrix Inversion is usually a costly computation and there may be some benefit to caching the inversse of a matricx rather than computing it again and again
## The following is a pair of finctions that are used to create a special object that stores a mantrix and caches its inverse

## This function creates a special matrix objectthat can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function computes the inverse of the special matrix created byt the makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then it shoul be retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  ##return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
