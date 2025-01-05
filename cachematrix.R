makeCacheMatrix <- function(x = matrix(1)) {
  inv <- NULL
  
  makeMatrix <- function(y, cols, rows) {
    mat <- matrix(y, ncol=cols, nrow=rows)
    x <<- mat
    inv <<- NULL
  }
  getMatrix <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(makeMatrix = makeMatrix, getMatrix = getMatrix,
       setInverse = setInverse, 
       getInverse = getInverse)		
}

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$getMatrix()
  i <- solve(m)
  x$setInverse(i)
  i
}
