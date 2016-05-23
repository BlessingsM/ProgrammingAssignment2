## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## The function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
   ## Initialization of inv
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




## The function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated then it 
## should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
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
