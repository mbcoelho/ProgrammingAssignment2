## Pair of functions (makeCacheMatrix, cacheSolve)  that cache the inverse 
## of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initialize an empty object to store matrix
  i <- NULL
  
  ## set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## return matrix
  get <- function() x
  
  ## set inverse matrix
  setinverse <- function(solve) i <<- solve
  
  ## return inverse matrix
  getinverse <- function() i
  
  ## define function arguments
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve  
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## assigned the result of getting inverse matrix to object i
  i <- x$getinverse()
  
  ## if the inverse matrix was previously calculated (not NULL) return the
  ## inverse matrix stored under i
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## initialize a new temporary matrix object based on the previously 
  ## created matrix
  data <- x$get()
  
  ## calculate inverse matrix using function solve()
  i <- solve(data, ...)
  
  ## assign inverse matrix to cached object i
  x$setinverse(i)
  
  ## returns cached inverse matrix i
  i
}