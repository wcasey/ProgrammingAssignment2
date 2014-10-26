## Matrix inversion is usually a costly computation and their may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly (there are also 
## alternatives to matrix inversion that we will not discuss here). 

## This function creates an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # set value to null
  inv <- NULL
  
  # set matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL # matrix has changed, reassign to NULL
  }
  
  # get matrix value
  get <- function() x
  
  # set inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # get inverse  matrix
  getinverse <- function() inv
  
  # return a list containing all functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function creates an inverse matrix unless one has been cached
cacheSolve <- function(x, ...) {
  
  # get inverse
  inv <- x$getinverse()
  
  # check if it inverse matrix is cached
  # if yes, return cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not, get matrix
  data <- x$get()
  
  # compute inverse of matrix
  inv <- solve(data, ...)
  
  # cache inverse of matrix
  x$setinverse(inv)
  
  # return inverse
  inv
}
