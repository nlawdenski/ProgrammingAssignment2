## makeCacheMatrix creates a list object that can store the inversion
## of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  ## the setfunction overwrites any previously cached data
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## the set and get inv functions initialize the list objects for caching the 
  ## matrix inversions
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  ## Creating the list object
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## cacheSolve returns the matrix inversion and caches it in the matrix object

cacheSolve <- function(x, ...) {
  
  ## First we check for cached data and return it if we find any
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## The solve command when only passed one matrix solves for the 
  ## matrix inversion
  data <- x$get()
  m <- solve(data, ...)
  ## This caches the inversion in the object
  x$setinv(m)
  ## This returns the newly solved matrix inversion
  m
  
  }
