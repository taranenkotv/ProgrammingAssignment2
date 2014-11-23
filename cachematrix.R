## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly.
## Functions below provides this functionality.

## The makeCacheMatrix function creates a special "matrix", which is really
## a list containing a function to:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse matrix
##   get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  ## set inverse matrix of 'x' to NULL
  inverse <- NULL
  
  ## define 'set' function which set new matrix 'x'
  set <- function(y) {
    ## set new matrix
    x <<- y
    ## reset inverse matrix to NULL
    inverse <<- NULL
  }
  
  ## define 'get' function which return the matrix 'x'
  get <- function() x
  
  ## define 'setInverse' function which set inverse matrix of 'x'
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  
  ## define 'getInverse; function which return inverse matrix of 'x'
  getInverse <- function() inverse
  
  ## return
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

### This is Solve function in R
## The 'cacheSolve' function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse matrix has already been calculated. If so, it gets the inverse 
## matrix from the cache and skips the computation. Otherwise, it calculates
## the mean of the data and sets the value of the mean in the cache via the 
## setmean function.
cacheSolve <- function(x, ...) {
  
  ## get inverse matrix from cache
  inverse <- x$getInverse()
  
  ## checks to see if the inverse matrix has already been calculated
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## get original matrix
  data <- x$get()
  
  ## calculate inverse matrix
  inverse <- solve(data)
  
  ## save result in cache
  x$setInverse(inverse)
  
  ## return a matrix that is the inverse of 'x'
  inverse
}
