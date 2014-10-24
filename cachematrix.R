
# Create special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  u  <- NULL
  set  <- function(y){
    x <<- y
    u <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) u  <<- inverse
  getinverse  <- function() u
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


# Compute the inverse of the matrix. If inverse has already been calculated (and no changes in the matrix have occured), cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  u  <- x$getinverse()
  if (!is.null(u)){
    message("getting cached data")
    return(u)
  }
  data  <- x$get()
  u  <- solve(data, ...)
  x$setinverse(u)
  u
}