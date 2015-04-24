## This pair of functions cache the inverse of a matrix
## (an invertible matrix)

## makeCacheMatrix is a function that creates a matrix
## object which is really a list containing a function to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## cacheSolve computes the inverse of the matrix returned by
## makeCacheMatrix via setinverse and if it is already calculated, it
## retrieves the inverse of the matrix from the cache via getinverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


