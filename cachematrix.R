#### Example:
#
# a <- makeCacheMatrix(c(1,2,3,4))
# b <- makeCacheMatrix(c(3,2,1,1,1,-1,0,1,2))
# cacheSolve(a)
#        [,1] [,2]
#   [1,]   -2  1.5
#   [2,]    1 -0.5
#
# cacheSolve(b)
#         [,1]       [,2]       [,3]
#    [1,]  0.5 -0.3333333  0.1666667
#    [2,] -0.5  1.0000000 -0.5000000
#    [3,] -0.5  0.6666667  0.1666667
#
# c <- makeCacheMatrix(c(1,2,3,4,5,6))
#
# cacheSolve(a)
#   getting cached data
#         [,1] [,2]
#    [1,]   -2  1.5
#    [2,]    1 -0.5
#
# cacheSolve(c)
#         [,1] [,2]
#   [1,]   -2  1.5
#   [2,]    1 -0.5
#
################################################



makeCacheMatrix <- function(x = matrix()) {
  
  #makeCacheMatrix() contains 4 functions:
  #
  #  set() changes the stored matrix
  #  get() returns the current stored matrix
  #  setinv() stores the value of the inverted matrix..
  #  getinv() ..and returns it
  ##########################
  ##########################
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    matrix(x,sqrt(length(x)),sqrt(length(x)))
  }
  
  setinv <- function(solve) { 
    m <<- solve
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