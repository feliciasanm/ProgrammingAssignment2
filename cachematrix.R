## These are a pair of functions that cache the inverse of a matrix
## by creating and using a special "matrix" object to manage it

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Return the inverse matrix of the special "matrix" object created
## by makeCacheMatrix function, returning from its cache if possible

cacheSolve <- function(x, ...) {
        
  inv <- x$getinverse()
  
  if(is.null(inv)) {
  
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
  
  } else message("Returning cached result...")
  
  inv
}
