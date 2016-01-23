## Pair of functions that cache the inverse of a matrix. 
## Functions create cache-able "matrix" object. 
## If the the matrix has already been inversed, gets inverse from cache as opposed to recalculating

## creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  
  #set the value of the inversed matrix
  setinverse <- function(inverse) i <<- inverse
  
  #get the value of the inversed matrix
  getinverse <- function() i
  
  #returns list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## finds the inverse of a "matrix" object (defined in makeCacheMatrix)
## if the inverse has already been calculated, retrieves the cached inversed matrix
cacheSolve <- function(x, ...) {
  
  #checks to see if inverse is already cached
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #if not already cached, get the original matrix and inverse it (solve)
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
