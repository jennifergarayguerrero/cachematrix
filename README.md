# cachematrix
## makeCacheMatrix--This function creates a special "matrix" object that 
## can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  jen <- NULL
  set <- function(y){
    x <<- y
    jen <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {jen <<- inverse}
  getInverse <- function()  {jen}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  jen <- x$getInverse()
  if(!is.null(jen)){
    message("getting catched data")
    return(jen)
  }
  mat <- x$get()
  jen <- solve(mat, ...)
  x$setInverse(jen)
  jen
}
