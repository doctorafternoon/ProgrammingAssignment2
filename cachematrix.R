
## makeCacheMatrix: takes a matrix 'x' and creates a matrix cache object,
##                  containing 'x' as data and associated cache functions

## cacheSolve: takes a matrix cache object and returns the inverse of the stored
##             matrix data, checking for a cached version before performing a
##             new calculation 


## takes a matrix 'x' (assumed to be invertible), returns a matrix cache object 
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## takes a matrix cache object and returns its inverse 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  print(x)
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
