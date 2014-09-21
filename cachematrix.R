## Caching of matrix inversion results for performance
## Thre are two function:
##    makeCacheMatrix creates an "object" to store a matrix and its inverse
##    cacheSolve to return the solved (inverted) matrix from the object,
##         either from the cache or by computing and storing it
## Example usage:
##    mat_inv <- makeCacheMatrix()
##    mat_ex <- matrix(c(4,3,2,7,5,4,9,8,7),3,3)
##    mat_inv$set(mat_ex)
##    cacheSolve(mat_ex)
##  subsequent calls to cacheSolve will use the cache
## 
## note this is per the assignment and not generic
## see https://class.coursera.org/rprog-007/forum/thread?thread_id=380 
## and https://class.coursera.org/rprog-007/forum/thread?thread_id=713#post-3127
## function commenting is per google guidelines:
##   https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#comments

makeCacheMatrix <- function(x = matrix()) {
  # make an object thaat stores a matrix and its inverse
  # for use with cacheSolve function
  # The result has internal functions:
  #    $get - return the stored matrix
  #    $set - set the stored matrix
  #    $getinv - get the inverse (may be null)
  #    $setinv - store the inverse (only for use by cacheSolve
  
  invmat <- NULL
  set <- function(y) {
    matrx <<- y
    invmat <<- NULL
  }
  get <- function() matrx
  setinv <- function(invm) invmat <<- invm
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  # return the result of solving (inverting) the matrix stored in the object x
  # either by accessing the cache or computing and storing the solution.
  # X must be the result of a previous call to makeCacheMatrix
  invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinv(invm)
  invm
}

