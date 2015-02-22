#The assignment is to write a pair of functions that
#cache the inverse of a matrix.

#the matrix name assigned is mt

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


#Matrix inversion is usually a costly computation and their may be some
#benefit to caching the inverse of a matrix rather than compute it
#repeatedly (there are also alternatives to matrix inversion that we will
#not discuss here). Your assignment is to write a pair of functions that
#cache the inverse of a matrix.

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}
