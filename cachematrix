## My functions establish the matrix and 
## then retrun its inverse

## Estabilshin Cache Matrix Definition

makeCacheMatrix <- function(x = matrix()) {
  elc <- NULL
  set <- function(y) {
    x <<- y
    elc <<- NULL
  }
  
## Establish Get function  
  get <- function() x
  setinverse <- function(inverse) elc <<- inverse
  getinverse <- function() elc
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Establish inverse of the Cash Matrix

cacheSolve <- function(x, ...) {
  elc <- x$getinverse()
  if(!is.null(elc)) {
    message("getting cached reversed data")
    return(elc)
  }
  data <- x$get()
  elc <- mean(data, ...)
  x$setinverse(elc)
  return (elc)
}
