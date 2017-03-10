
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inv to hold value of inverse
  inv <- NULL
  ## set function to assign new value, and set inv to NULL.  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## function to get value of matrix
  get <- function() x
  ## function to assign value of inv 
  setinverse <- function(inverse) inv <<- inverse
  ## function to get value of inv when called
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix function.
## If the inverse has already been calculated (and matrix has not changed),
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
