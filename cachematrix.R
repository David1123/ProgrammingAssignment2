## The functions take a matrix, and then solve for its inverse,
## retrieving a cached value if called multiple times

## Function creates a 'matrix' that can be evaluated by cacheSolve
## to quickly give the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function returns the inverse of a matrix created by makeCacheMatrix,
## either by calculating it, or by returning the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("Cached value for you!")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m    
    
}