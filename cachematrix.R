##makeCacheMatrix
#This function creates a special "matrix" object that can cache its inverse.

#Assume that the matrix supplied is always invertible
# X is a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  #1. set the value of the matrix
  m <- NULL
  set <-function(y){
    x <<- y
    m <<- NULL
  }
  
  #2. get the value of the matrix  
  get <- function() x
  
  #3. set the inverse of the matrix  
  setInverse <- function(inverse) m <<- inverse
  
  #4. get the inverse of the matrix  
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}





##cacheSolve
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# assume that the matrix supplied is always invertible.


cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m)){
    message("Getting cached Inverse of the matrix")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
