## In makeCacheMatrix the function configurates the matrix as a list to save its inverse.
## In cacheSolve the function verifies if the inverse has been calculated and then returns it.
## If the inverse hasn't been calculated, the function calculates it before returning it.

## This function receives a matrix and returns a special matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix_inv <<- inverse
  getinverse <- function() matrix_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function receives a special matrix and returns its inverse.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
