## Below are functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## The function below creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function blow calculates the inverse of the "matrix" returned 
## by "makeCacheMatrix" above. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
