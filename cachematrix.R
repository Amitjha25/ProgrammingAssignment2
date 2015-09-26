
## This program is to create matrix and generate inverse of matrix and cache it. 
## If the inverse of matrix is cache, it will pull the cache inverse matrix else generate on run time
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Inversing the matrix as its not cache.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
