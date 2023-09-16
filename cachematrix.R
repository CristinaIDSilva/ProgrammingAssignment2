## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##initializing inverse as NULL
  set <- function(y) {  ##function to set matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x     ##function to get matrix
  setsolve <- function(solve) inv <<- solve ##set the inverse of matrix
  getsolve <- function() inv ##get the inverse of matrix
  list(set = set, get = get, ##return list
       setsolve = setsolve,
       getsolve = getsolve)
}



##This function computes the inverse of the special "matrix" created by 
##makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)     ##calculate the inverse
  x$setsolve(inv)
  inv  ##Return a matrix that is the inverse of 'x'
}