## Comments below give an overall description of what my
## functions do

## makeCacheMatrix  a special "matrix"
## which is really a list containing a function to
##
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
## the names of the list's elements/functions are:
## set,get,setinverse,getinverse.
## 
## makeCacheMatrix is a modified version of makeVector


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve retrieves the inverse from 
## the cache.
## 
## cacheSolve is a modified version of cachemean

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
  return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}