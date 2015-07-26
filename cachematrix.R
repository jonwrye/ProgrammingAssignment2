## This pair of functions will calculate the inverse of a matrix and retrive it from the cache (if it has already been calulated).
## The first function - makeCacheMatrix - returns a list of 4 functions for setting and getting a matrix, and setting and getting the inverse of a matrix.
## The second function - cacheSolve - will look for the cached inverse of the matrix calculated by the makeCacheMatrix function. If it is there, it will return it. If not, it will calculate it, and return it. 

## This function takes an optional matrix object argument. If no argument is supplied, the matrix will be empty and can be set later using the 'set' function returned in the function list.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function requires the object created by the 'makeCacheMatrix' function to be passed as an argument. 
## This function will use the 'getinverse' function of that object to determine if the inverse has been calculated. If not, it will call the 'setinverse' function of the object to save the inverted matrix to cache after it is calculated.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
