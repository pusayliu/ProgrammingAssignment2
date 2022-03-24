## The following R code is about caching the inverse of a matrix rather than compute it repeatedly

## Write a short comment describing this function
## This function is about the value and inverse of the inputted matrix, and returns a special matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }


## The following code first inpect if the inverse has been calculated. 
## If so, the function gets the inverse from the cache and skips the calculation.
## Otherwise, it calculates the inverse of the inputted matrix and sets the value of the inverse matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
