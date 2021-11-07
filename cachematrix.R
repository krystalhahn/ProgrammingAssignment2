## Cache the inverse of a matrix


## Create special matrix object that can cache its inverse

makeCacheMatrix <- function(mtx = matrix()) {
  inv <- NULL
  set <- function(x) {
    mtx <<- x
    inv <<- NULL
  }
  get <- function() mtx
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'mtx'

cacheSolve <- function(mtx, ...) {
  inv <- mtx$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mtx$get()
  inv <- solve(data, ...)
  mtx$setinv(inv)
  return(inv)
}
