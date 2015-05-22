## The 2 functions below take a matrix as an input and returns its inverse. 
## Before computing the inverse it checks to see if the inverse has been cached.
## If yes, it returns the cached inverse and if no, it computes the inverse.


## The makeCacheMatrix function returns a list of functions to
##       set a matrix
##       get a matrix
##       set the inverse of the matrix
##       get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The cacheSolve function returns the inverse of the matrix used as input to the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
