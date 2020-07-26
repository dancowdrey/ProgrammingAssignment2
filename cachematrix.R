## Put comments here that give an overall description of what your
## functions do
## These functions calculate the inverse of a matrix and save this to cache
## This saves processing power where the inverse of the same matrix is required in future
## In this case, the inverse can simply be retrieved from the cache

## Write a short comment describing this function
## Creates a special "matrix" object can can cache its inverse
## Contains a function to set/get value of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Checks to see if the inverse of a matrix has already been calculated
## If it has, function retrieves inverse from cache
## If not, the inverse is computed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
