## Cache the inverse of a matrix

## Create special matrix object that can cache its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(mtx = matrix()) {
  inv <- NULL
  set <- function(x) {    # use '<<-' to assign a value to an object in an environment different from the current environment
    mtx <<- x
    inv <<- NULL          # sets value of inv to Null
  }
  get <- function() mtx
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'mtx'
## If the inverse has already been calculated, cacheSolve() will retrieve the inverse from the cache

cacheSolve <- function(mtx, ...) {
  inv <- mtx$getinv()
  if(!is.null(inv)) {      # if the inverse has already been calculated
    message("getting cached data")    # get the inverse from the cache
    return(inv)                       # return cached matrix
  }
  data <- mtx$get()         # if not, calculate the inverse
  inv <- solve(data, ...) 
  mtx$setinv(inv)           # set the value of the inverse in the cache by setinv()
  return(inv)               # return inverse of matrix
}
