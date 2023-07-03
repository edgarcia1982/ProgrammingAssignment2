## It creates a special "matrix" object that can cache its inverse and 
## implements the caching mechanism in the cacheSolve function.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## The setMatrix function allows updating the matrix.
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  ## The getMatrix function returns the original matrix.
  getMatrix <- function() {
    x
  }
  ## The setInverse function sets the cached inverse.
  setInverse <- function(solve){
    inverse <<- solve
  }
  ## The getInverse function retrieves the cached inverse.
  getInverse <- function() {
    inverse
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function takes the x object (created by makeCacheMatrix) 
## as input and retrieves the cached inverse from it if it exists. If the inverse
## is not cached, it computes the inverse using the solve function, sets the 
## cached inverse using setInverse, and returns the inverse.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$getMatrix()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
