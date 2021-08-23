## These functions allow for an inverse matrix to be computed and cached
## therefore allowing this cached value to be returned instead of computing
## again.

## This function creates the sub-functions to set, get, setinverse
## and getinverse.
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse= getinverse
  )
}


## This function first checks if an inverse has already been computed
## and cached, and returns the cached value if it has. If not, is computes the
## inverse and returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached inverse")
    return(z)
  }
  data <- x$get()
  z <- solve(data,...)
  x$setinverse(z)
  z
}