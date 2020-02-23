## These functions cache the inverse of a mtarix

## Function to create matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverseMatrix 
  inverseMatrix <- NULL
  
  ##  Define the get & set functions for the inverseMatrix 
  set <- function(y) {
      x <<- y
      inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverseMatrix <<- solve
  getInverse <- function () inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function to create and inverse of the matrix if it does not already exists.

cacheSolve <- function(x, ...) {
  ## Get the cached value for the inverseMatrix
  inverseMatrix <- x$getInverse()
  if (!is.null(inverseMatrix)) {
    message("Getting the cached data for the inverse of the matrix")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
