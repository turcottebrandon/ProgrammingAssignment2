

makeCacheMatrix <- function(x = matrix()) {
  
  #makeCacheMatrix() defines 4 functions:
  #
  #  set() changes the stored matrix
  #  get() returns the current stored matrix
  #  setinv() stores the value of the inverted matrix..
  #  getinv() ..and returns it
  ##########################
  
  m <- NULL  #define a temporary variable 'm' and set to NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    matrix(x,sqrt(length(x)),sqrt(length(x)))
  }
  
  setinv <- function(solve) { 
    m <- solve
  }
  getinv <- function() {
    m
  }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function (x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #cacheSolve()
  #
  #  Looks for the value m stored previously with getinv. If it exists in memory
  #  it will display "getting cached data" and load the matrix.  If it does not
  #  exist, the inverse will be calcuated and displayed.
  #
  ##########################
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}