## A matrix that returns its inverse, quickly from a saved version if 
## the inverse has been calculated previously, or just in time
## if this is the first request in which case it also saves the 
## value for the future.

# Usage: 
# cm <- makeCacheMatrix(mat)
# cacheSolve(cm) #takes a bit
# cacheSolve(cm) #much faster

## create a list of functions to operate on the matrix:
## set, get, setinverse, and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## return the matrix inverse, solving for the inverse if the inverse
## has not been previously computed.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    #message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}